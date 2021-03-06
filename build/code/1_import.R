# Data Import Function ----------------------------------------------------

# This script compiles the data for each of the 50 states from Zillow's
# raw ZTRAX .txt files. It will take a while to run because it is iterating
# over all 50 states + Washington DC, but is a more modern version of 
# Zillow's own script which relies on base R and is therefore slower and 
# more verbose.

# Filtering the data, selecting needed columns, compiling the data for
# particular states is easier in this script because it is wrapped in a 
# single function. 

# -------------------------------------------------------------------------

# load pacman -- package manager
if(!require('pacman')) install.packages('pacman')

# load (and install if not present) other packages needed
p_load(dplyr,
       purrr,
       readr,
       here,
       bit64,
       readxl,
       data.table
       )

# set options
options(scipen = 999)

# create folder for data if it doesn't exist
if (!dir.exists(here('build', 'temp', 'ztrax'))) 
  {dir.create(here('build', 'temp', 'ztrax'))}


# get column headers from layout file -------------------------------------
# assessment headers
asmt_head = as.data.table(read_xlsx(here('build', 'data', 'layout.xlsx'), 
                      sheet = 1))

# transaction headers
trans_head = as.data.table(read_xlsx(here('build', 'data', 'layout.xlsx'), 
                                     sheet = 2))

# column names for assessment tables
main_cols = asmt_head[TableName == "utMain", .(FieldName)]
bldg_cols = asmt_head[TableName == "utBuilding", .(FieldName)]
area_cols = asmt_head[TableName == "utBuildingAreas", .(FieldName)]

# column names for the transaction tables
trans_cols = trans_head[TableName == "utMain", .(FieldName)]
prop_cols = trans_head[TableName == "utPropertyInfo", .(FieldName)]

# write the function ------------------------------------------------------
ztrax_import = function(state) {
  # create the first dataframe -- assessment main table
  main = fread(here("build", "data", "ZTRAX", state, "ZAsmt", "Main.txt"),
                    sep = '|',
                    header = FALSE,
                    stringsAsFactors = FALSE,             
                    quote = "",                                
                    col.names = t(main_cols))
  
  # select only columns to keep
  main = main[, .(RowID, ImportParcelID, FIPS, State, County, 
                  PropertyFullStreetAddress, PropertyCity,
                  PropertyZip, NoOfBuildings, LotSizeAcres, 
                  LotSizeSquareFeet, PropertyAddressLatitude,
                  PropertyAddressLongitude, 
                  PropertyAddressCensusTractAndBlock)]
  
  
  # house characteristics frame
  build = fread(here("build", "data", "ZTRAX", state, "ZAsmt", "Building.txt"),
               sep = '|',
               header = FALSE,
               stringsAsFactors = FALSE,             
               quote = "",                                
               col.names = t(bldg_cols))
  
  # keep the wanted columns and filter the type of property
  build = build[PropertyLandUseStndCode %in% c("RR000", "RR101", "RR999"), 
                .(RowID, NoOfUnits, PropertyLandUseStndCode, YearBuilt, 
                  EffectiveYearBuilt, YearRemodeled, NoOfStories, TotalRooms,
                  TotalBedrooms, TotalKitchens, TotalCalculatedBathCount, 
                  BuildingOrImprovementNumber)] 
  
  
  # square feet table
  area = fread(here("build", "data", "ZTRAX", state, "ZAsmt", "BuildingAreas.txt"),
               sep = '|',
               header = FALSE,
               stringsAsFactors = FALSE,             
               quote = "",                                
               col.names = t(area_cols))
  
  # filter and keep columns
  area = area[BuildingAreaStndCode %in% c("BAL", "BAF", "BAE", "BAG",
                                              "BAJ", "BAT", "BLF"),
              list(sqfeet = max(BuildingAreaSqFt, na.rm = T)), 
              by = c("RowID", "BuildingOrImprovementNumber")]

  
  # merge
  asmt = merge(main, build, all.x = TRUE, by = "RowID")
  asmt = merge(asmt, area, all.x = TRUE, by = c("RowID", "BuildingOrImprovementNumber"))
  
  # drop data tables
  rm(main, build, area)
  
  # create folder for data if it doesn't exist
  if (!dir.exists(here('build', 'temp', 'ztrax', state))) 
    {dir.create(here('build', 'temp', 'ztrax', state))}
  
  # write the file
  fwrite(asmt, here("build", "temp", "ztrax", state, "asmt.csv"))
  
  # remove
  rm(asmt)
  
  
  # and finally the transaction tables
  trans = read_delim(here('build', 'data', 'ZTRAX', state, 'ZTrans', 'Main.txt'),
                     delim = '|',
                     col_names = FALSE,
                     escape_double = FALSE,
                     trim_ws = TRUE,
                     col_types = cols_only(
                       X1 = col_guess(),  # TransId
                       X5 = col_guess(),  # DataClassStndCode
                       X7 = col_date(),   # RecordingDate
                       X18 = col_date(),  # DocumentDate
                       X19 = col_date(),  # SignatureDate
                       X25 = col_guess(), # SalesPriceAmount
                       X31 = col_guess(), # IntraFamilyTransferFlag
                       X33 = col_guess(), # PropertyUseStndCode
                       X39 = col_guess(), # LenderTypeStndCode
                       X63 = col_guess(), # LoanTypeStndCode
                       X66 = col_guess()  # LoanTypeProgramStndCode
                     ))
  
  # keep the field names in the header df
  trans_cols = trans_cols %>%
    filter(FieldName %in% c('TransId', 'DataClassStndCode', 'RecordingDate', 
                            'DocumentDate', 'SignatureDate', 'SalesPriceAmount',
                            'IntraFamilyTransferFlag', 'PropertyUseStndCode',
                            'LenderTypeStndCode', 'LoanTypeStndCode', 
                            'LoanTypeProgramStndCode'))
  
  # assign the columns
  colnames(trans) = t(trans_cols)
  
  # specify function for "not %in%"
  `%not%` = Negate(`%in%`)
  
  # filter for observations with non-zero and non-NA sales prices, dates,
  # and other codes to ensure we're only dealing with the residential housing
  # market and not things like inter-family transfers, reverse mortgages, or
  # mortgage refinancing. See the ZTRAX transaction table documentation for
  # full explanations of the Codes being filtered for here. Also, keeping NA
  # values for codes since many of the observations don't have these information
  # and we would lose a huge amount of data if we were to filter them out.
  trans = trans %>% 
    filter(!is.na(SalesPriceAmount),
           SalesPriceAmount > 0,
           DataClassStndCode %in% c('D', 'H', 'U', 'J', 'V', 'M') | is.na(DataClassStndCode),
           IntraFamilyTransferFlag == 'N' | is.na(IntraFamilyTransferFlag),
           PropertyUseStndCode %in% c('CD', 'CP', 'MH', 'NW', 'RR', 'SR') |
             is.na(PropertyUseStndCode),
           LenderTypeStndCode %not% c('RE', 'RM') | is.na(LenderTypeStndCode),
           LoanTypeStndCode %in% c('AS', 'BL', 'CE', 'CL', 'DP', 'FE', 'FM',
                                   'HE', 'EB', 'EX', 'FA') |
             is.na(LoanTypeStndCode),
           LoanTypeProgramStndCode %in% c('CV', 'FH', 'VA', 'SV') |
             is.na(LoanTypeProgramStndCode)
    ) %>%
    # according to Zillow, should prefer DocumentDate to SignatureDate to
    # RecordingDate, so coalesce creates a new column called sale_date taking
    # this priority
    mutate(sale_date = coalesce(DocumentDate, SignatureDate, RecordingDate)) %>%
    select(TransId, sale_date, SalesPriceAmount)
  
  # convert to data.table
  trans = as.data.table(trans)
  
  # now to the transaction data -- use the readr package's read_delim function
  # instead of fread because we're going to specify which columns to import 
  # during the file reading. This saves a ton of memory which is important
  # for the large states' transaction data. Need to specify using the column
  # position which columns you want. Not very elegant but gets the job 
  # done locally.
  prop = read_delim(here('build', 'data', 'ZTRAX', state, 'ZTrans', 'PropertyInfo.txt'),
                    delim = '|',
                    col_names = FALSE,
                    escape_double = FALSE,
                    trim_ws = TRUE,
                    col_types = cols_only(
                      X1 = col_double(),
                      X47 = col_guess(),
                      X64 = col_guess(),
                      X65 = col_guess()
                    ))
  
  # filter the property info column table to just the wanted columns
  prop_cols = prop_cols %>%
    filter(FieldName %in% c('TransId', 'PropertySequenceNumber', 
                            'LoadID', 'ImportParcelID'))
                    
  # match the headers to the df
  colnames(prop) = t(prop_cols)
  
  # filter for first property sequence ID
  prop = prop %>% filter(PropertySequenceNumber == 1,
                         !is.na(ImportParcelID))
  
  # convert to data.table -- better memory management
  prop = as.data.table(prop)
  
  # drop columns
  prop = prop[, .(TransId, ImportParcelID)]
  
  # merge the two trans tables
  trans = merge(trans, prop, by = "TransId")
  rm(prop)
  
  # merge
  asmt = fread(here("build", "temp", "ztrax", state, "asmt.csv"))
  trans = merge(trans, asmt, by = "ImportParcelID")
  
  # drop
  rm(asmt)
  
  # write
  fwrite(trans, here("build", "temp", "states", paste0(state, '.csv')))
  
  # drop
  rm(trans)
  
}


# call the function -------------------------------------------------------
# get the list of state names from the folder 
state_names = list.files(here('build', 'data', 'ZTRAX'))

# call the function
map(state_names, ztrax_import)

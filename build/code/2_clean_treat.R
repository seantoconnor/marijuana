# Description -------------------------------------------------------------

# This script imports the state files generated in `1_import.R` to clean
# and assign marijuana legalization treatment variables. The process 
# follows the original cleaning done previously. Any edits to cleaning 
# should be made in this file.

# It replaces the four scripts previously used in the project, 
# `1_updated_clean.R`, `2_treatment.R`, `3_merge.R`, and 
# `4_inflation_adjust.R`

# -------------------------------------------------------------------------

# load pacman -- installs and loads other packages
if(!require('pacman')) install.packages('pacman')

# load other packages
p_load(tidyverse,
       here,
       zoo
       )


# write a function to load and clean state data ---------------------------
# cleaning function
state_clean = function(state) {
  # load the state data
  df = read_csv(here('build', 'temp', 'states', paste0(state, '.csv')))
  
  # clean the data -- same process as before
  df = df %>% 
    # create price per square foot
    mutate(ppsqft = SalesPriceAmount / sqfeet,
           # year
           year = format(as.Date(sale_date, format="%Y-%m-%d"),"%Y"),
           year = as.integer(year),
           # year-quarter var for fixed effects
           yearQuarter = as.yearqtr(sale_date, format="%Y-%m-%d"),
           # year-month for fixed effects
           yearMonth = as.yearmon(sale_date, format="%Y-%m-%d")) %>%
    # select the variables to keep
    select(ImportParcelID, sale_date, year, SalesPriceAmount, 
           State, County, PropertyCity, PropertyZip, 
           PropertyFullStreetAddress, PropertyAddressCensusTractAndBlock,
           PropertyAddressLatitude, PropertyAddressLongitude, YearBuilt,
           TotalRooms, TotalBedrooms, TotalCalculatedBathCount,
           sqfeet, ppsqft, yearQuarter, yearMonth) %>%
    filter(sqfeet > quantile(sqfeet, .001, na.rm = TRUE), 
           year > 1994,
           year < 2019,
           TotalBedrooms < quantile(TotalBedrooms, .99999, na.rm = TRUE),
           sqfeet < quantile(sqfeet, .999, na.rm = TRUE),
           TotalCalculatedBathCount < quantile(TotalCalculatedBathCount, .9999, na.rm = TRUE),
           TotalRooms < quantile(TotalRooms, .9999, na.rm = TRUE),
           ppsqft < quantile(ppsqft, .999, na.rm = TRUE),
           YearBuilt < 2019, 
           TotalCalculatedBathCount > .74) %>%
    # rename and rearrange the variables
    select(houseID = ImportParcelID, date = sale_date, year, yearQuarter,
           yearMonth, price = SalesPriceAmount, sqfeet, ppsqft, state = State, 
           county = County, city = PropertyCity, zip = PropertyZip, 
           address = PropertyFullStreetAddress, 
           censusTract = PropertyAddressCensusTractAndBlock,
           latitude = PropertyAddressLatitude,
           longitude = PropertyAddressLongitude, 
           yearBuilt = YearBuilt, rooms = TotalRooms,
           bedrooms = TotalBedrooms, bathrooms = TotalCalculatedBathCount)
  
  # create a price index
  index = tibble(year = seq(1993, 2018)) %>%
    mutate(price_index = case_when(year == 1993 ~ 144.48/251.10, 
                                   year == 1994 ~ 148.23/251.10, 
                                   year == 1995 ~ 152.38/251.10, 
                                   year == 1996 ~ 156.86/251.10,
                                   year == 1997 ~ 160.53/251.10,
                                   year == 1998 ~ 163.01/251.10,
                                   year == 1999 ~ 166.58/251.10,
                                   year == 2000 ~ 172.19/251.10,
                                   year == 2001 ~ 177.04/251.10,
                                   year == 2002 ~ 179.87/251.10,
                                   year == 2003 ~ 184.00/251.10,
                                   year == 2004 ~ 188.91/251.10,
                                   year == 2005 ~ 195.27/251.10,
                                   year == 2006 ~ 201.56/251.10,
                                   year == 2007 ~ 207.34/251.10,
                                   year == 2008 ~ 215.25/251.10,
                                   year == 2009 ~ 214.56/251.10,
                                   year == 2010 ~ 218.08/251.10,
                                   year == 2011 ~ 224.92/251.10,
                                   year == 2012 ~ 229.59/251.10,
                                   year == 2013 ~ 232.95/251.10,
                                   year == 2014 ~ 236.71/251.10,
                                   year == 2015 ~ 237.00/251.10,
                                   year == 2016 ~ 240.01/251.10,
                                   year == 2017 ~ 245.13/251.10,
                                   year == 2018 ~ 251.10/251.10, 
                                   TRUE ~ as.numeric(year)))
  
  # merge the index with the transaction data
  df = left_join(df, index, by = "year")
  
  # calculate the deflated price
  df = df %>%
    mutate(deflated_price = price / price_index,
           deflated_ppsqft = deflated_price / sqfeet) %>%
    select(-c(price, ppsqft)) %>%
    mutate(price = deflated_price,
           ppsqft = deflated_ppsqft) %>%
    select(-c(deflated_price, deflated_ppsqft, price_index))
}

# get a list of states
state_names = list.files(here('build', 'data', 'ZTRAX'))

# map the function to a dataframe
pot = map_df(state_names, state_clean)



# create the treatment variables ------------------------------------------
pot = pot %>%
  # date of legalization vote
  mutate(vote = ifelse(date > "2016-11-08" & state == "CA" |
                         date > "2012-11-06" & state == "CO" |
                         date > "2016-11-08" & state == "MA" |
                         date > "2016-11-06" & state == "MI" |
                         date > "2016-11-08" & state == "NV" |
                         date > "2014-11-04" & state == "OR" |
                         date > "2012-11-06" & state == "WA" |
                         date > "2014-11-04" & state == "DC", 1, 0),
         # date of legal possession
         possession = ifelse(date > "2016-11-09" & state == "CA" |
                               date > "2012-12-06" & state == "CO" |
                               date > "2016-12-15" & state == "MA" |
                               date > "2016-12-06" & state == "MI" |
                               date > "2017-01-01" & state == "NV" |
                               date > "2015-07-01" & state == "OR" |
                               date > "2012-12-06" & state == "WA" |
                               date > "2015-02-26" & state == "DC", 1, 0),
         # date of legal sales
         sale = ifelse(date > "2018-01-01" & state == "CA" |
                         date > "2014-01-01" & state == "CO" |
                         date > "2018-07-01" & state == "MA" |
                         date > "2017-01-01" & state == "NV" |
                         date > "2015-10-01" & state == "OR" |
                         date > "2014-07-08" & state == "WA", 1, 0),
         # date of legal sales at dispensaries
         saleDisp = ifelse(date > "2018-01-01" & state == "CA" |
                             date > "2014-01-01" & state == "CO" |
                             date > "2018-11-20" & state == "MA" |
                             date > "2017-01-01" & state == "NV" |
                             date > "2015-10-01" & state == "OR" |
                             date > "2014-07-08" & state == "WA", 1, 0),
         # date of medical
         medical = ifelse(date > "1996-11-06" & state == "CA" |
                            date > "2001-06-01" & state == "CO" |
                            date > "2012-05-31" & state == "CT" |
                            date > "2011-07-01" & state == "DE" |
                            date > "2017-01-03" & state == "FL" |
                            date > "2000-06-14" & state == "HI" |
                            date > "2014-01-01" & state == "IL" |
                            date > "2013-01-01" & state == "MA" |
                            date > "2008-12-04" & state == "MI" |
                            date > "2014-05-30" & state == "MN" |
                            date > "2018-12-06" & state == "MO" |
                            date > "2001-10-01" & state == "NV" |
                            date > "2013-07-23" & state == "NH" |
                            date > "2010-07-01" & state == "NJ" |
                            date > "1998-12-03" & state == "OR" |
                            date > "2016-05-17" & state == "PA" |
                            date > "2006-01-03" & state == "RI" |
                            date > "1998-11-03" & state == "WA" |
                            date > "2018-07-01" & state == "WV" |
                            date > "2010-06-20" & state == "DC", 1, 0))


# merge in the state-level controls ---------------------------------------
states = read_csv(here("build", "data", "state_controls", "Control Variables Data.csv"))
update = read_csv(here("build", "data", "state_controls", "StateControls17_18.csv"))

# merge old and new gdp/pop data
indicators = bind_rows(states, update) %>%
  arrange(state, year) %>%
  fill(`total land area`) %>%
  mutate(density = population / `total land area`) %>%
  select(state, year, gdp = GDP, population, land = `total land area`, 
         density, houseUnits = `housing units`)

# merge the indicator data with the housing data
pot = left_join(pot, indicators, by = c("year", "state"))

# write the cleaned data file
write_csv(pot, here('analysis', 'data', 'zillow.csv'))

# clear the environment
rm(list = ls())


# write a file without doing any cleaning, just renaming ------------------
raw_data = function(state) {
  # load the state data
  df = read_csv(here('build', 'temp', 'states', paste0(state, '.csv')))
  
  # clean the data -- same process as before
  df = df %>% 
    # create price per square foot
    mutate(ppsqft = SalesPriceAmount / sqfeet,
           # year
           year = format(as.Date(sale_date, format="%Y-%m-%d"),"%Y"),
           year = as.integer(year),
           # year-quarter var for fixed effects
           yearQuarter = as.yearqtr(sale_date, format="%Y-%m-%d"),
           # year-month for fixed effects
           yearMonth = as.yearmon(sale_date, format="%Y-%m-%d")) %>%
    # select the variables to keep
    select(ImportParcelID, sale_date, year, SalesPriceAmount, 
           State, County, PropertyCity, PropertyZip, 
           PropertyFullStreetAddress, PropertyAddressCensusTractAndBlock,
           PropertyAddressLatitude, PropertyAddressLongitude, YearBuilt,
           TotalRooms, TotalBedrooms, TotalCalculatedBathCount,
           sqfeet, ppsqft, yearQuarter, yearMonth) %>%
    # rename and rearrange the variables
    select(houseID = ImportParcelID, date = sale_date, year, yearQuarter,
           yearMonth, price = SalesPriceAmount, sqfeet, ppsqft, state = State, 
           county = County, city = PropertyCity, zip = PropertyZip, 
           address = PropertyFullStreetAddress, 
           censusTract = PropertyAddressCensusTractAndBlock,
           latitude = PropertyAddressLatitude,
           longitude = PropertyAddressLongitude, 
           yearBuilt = YearBuilt, rooms = TotalRooms,
           bedrooms = TotalBedrooms, bathrooms = TotalCalculatedBathCount)
  
  # create a price index
  index = tibble(year = seq(1993, 2018)) %>%
    mutate(price_index = case_when(year == 1993 ~ 144.48/251.10, 
                                   year == 1994 ~ 148.23/251.10, 
                                   year == 1995 ~ 152.38/251.10, 
                                   year == 1996 ~ 156.86/251.10,
                                   year == 1997 ~ 160.53/251.10,
                                   year == 1998 ~ 163.01/251.10,
                                   year == 1999 ~ 166.58/251.10,
                                   year == 2000 ~ 172.19/251.10,
                                   year == 2001 ~ 177.04/251.10,
                                   year == 2002 ~ 179.87/251.10,
                                   year == 2003 ~ 184.00/251.10,
                                   year == 2004 ~ 188.91/251.10,
                                   year == 2005 ~ 195.27/251.10,
                                   year == 2006 ~ 201.56/251.10,
                                   year == 2007 ~ 207.34/251.10,
                                   year == 2008 ~ 215.25/251.10,
                                   year == 2009 ~ 214.56/251.10,
                                   year == 2010 ~ 218.08/251.10,
                                   year == 2011 ~ 224.92/251.10,
                                   year == 2012 ~ 229.59/251.10,
                                   year == 2013 ~ 232.95/251.10,
                                   year == 2014 ~ 236.71/251.10,
                                   year == 2015 ~ 237.00/251.10,
                                   year == 2016 ~ 240.01/251.10,
                                   year == 2017 ~ 245.13/251.10,
                                   year == 2018 ~ 251.10/251.10, 
                                   TRUE ~ as.numeric(year)))
  
  # merge the index with the transaction data
  df = left_join(df, index, by = "year")
  
  # calculate the deflated price
  df = df %>%
    mutate(deflated_price = price / price_index,
           deflated_ppsqft = deflated_price / sqfeet) %>%
    select(-c(price, ppsqft)) %>%
    mutate(price = deflated_price,
           ppsqft = deflated_ppsqft) %>%
    select(-c(deflated_price, deflated_ppsqft, price_index))
}

# get a list of states
state_names = list.files(here('build', 'data', 'ZTRAX'))

# map the function to a dataframe
pot_raw = map_df(state_names, raw_data)

# write the raw file
write_csv(pot_raw, here('build', 'output', 'zillow_raw.csv'))


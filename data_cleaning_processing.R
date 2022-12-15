
# Part 1: Build County-Level Dataset --------------------------------------

## Load Appalachian Counties list from the ARC
appalachian_counties <- read_excel('data/appalachian_counties_ARC_2021.xlsx')

## Create dummy variable for medicaid expansion and a variable for date of expansion

expansion <- c("Kentucky", "Maryland", "New York", "Ohio", "West Virginia",
               "Pennsylvania")
no_expansion <- c("Alabama", "Georgia", "Mississippi", "North Carolina", 
                  "South Carolina", "Tennessee", "Virginia")

appalachian_counties <- appalachian_counties %>% 
  mutate(medicaid_expansion = as.numeric(STATE %in% expansion),
         expansion_date = make_date(2014, 1, 1))

### Re-code dates for Pennsylvania (late expansion)

appalachian_counties$expansion_date[appalachian_counties$STATE == "Pennsylvania"] <-
  make_date(2015,1,1)

### Re-code medicaid expansion as a factor

appalachian_counties$medicaid_expansion <- factor(appalachian_counties$medicaid_expansion,
                                                  levels = c(0,1),
                                                  labels = c("Non-Expansion", "Expansion"))

## Load geometry files from US Census Bureau, merge into data 
county_shapes <- read_sf(dsn = 'data/cb_2021_us_county_5m/cb_2021_us_county_5m.shp')

county_shapes <- county_shapes %>% 
  mutate(FIPS = GEOID)

appalachian_counties <- appalachian_counties %>% 
  left_join(county_shapes, by = 'FIPS') %>% 
  select(FIPS, STATE, COUNTY, medicaid_expansion, expansion_date, geometry)

# Part 2: Load Time Series of Overdose Death Rates and Census Data --------

## Load overdose death rate data

overdose_data <- read_csv("data/NCHS_-_Drug_Poisoning_Mortality_by_County__United_States.csv")

### Keep columns for FIPS codes, year, and modeled overdose deaths

overdose_data <- overdose_data %>% 
  select(FIPS, Year, `Model-based Death Rate`) %>%
  rename(estimated_overdose_death_rate = `Model-based Death Rate`)

## Pull ACS data from Tidycensus

### Apologies for the tedious code, this could've been a for loop. We were a bit worried about variable name changes, so we kept it as is just to be safe!

census_api_key("b79b301dc87bb0fd551147883a8141dca4e2823e")

### Pull 2010 ACS county-level data 

acs_2010 <- get_acs(geography = "county",
                    variables = c(population = "B01001_001",
                                  white_pop = "B01001H_001",
                                  hisp_pop = "B01001I_001",
                                  asian_pop = "B02001_005",
                                  black_pop = "B02001_003",
                                  male_pop = "B01001_002",
                                  med_income = "B19013_001",
                                  med_age = "B01002_001",
                                  pov = "B17025_002"),
                    year = 2010)

acs_2010 <- acs_2010 %>% 
  select(-moe, -NAME) %>% 
  spread(key = variable, value = estimate) %>% 
  mutate(Year = 2010,
         white_share = 100 * (white_pop/population),
         hisp_share = 100 * (hisp_pop/population),
         asian_share = 100 * (asian_pop/population),
         black_share = 100 * (black_pop/population),
         male_share = 100 * (male_pop/population),
         poverty_rate = 100 * (pov/population))


### Pull 2011 ACS county-level data 

acs_2011 <- get_acs(geography = "county",
                    variables = c(population = "B01001_001",
                                  white_pop = "B01001H_001",
                                  hisp_pop = "B01001I_001",
                                  asian_pop = "B02001_005",
                                  black_pop = "B02001_003",
                                  male_pop = "B01001_002",
                                  med_income = "B19013_001",
                                  med_age = "B01002_001",
                                  pov = "B17025_002"),
                    year = 2011)

acs_2011 <- acs_2011 %>% 
  select(-moe, -NAME) %>% 
  spread(key = variable, value = estimate) %>% 
  mutate(Year = 2011,
         white_share = 100 * (white_pop/population),
         hisp_share = 100 * (hisp_pop/population),
         asian_share = 100 * (asian_pop/population),
         black_share = 100 * (black_pop/population),
         male_share = 100 * (male_pop/population),
         poverty_rate = 100 * (pov/population))


### Pull 2012 ACS county-level data 

acs_2012 <- get_acs(geography = "county",
                    variables = c(population = "B01001_001",
                                  white_pop = "B01001H_001",
                                  hisp_pop = "B01001I_001",
                                  asian_pop = "B02001_005",
                                  black_pop = "B02001_003",
                                  male_pop = "B01001_002",
                                  med_income = "B19013_001",
                                  med_age = "B01002_001",
                                  pov = "B17025_002"),
                    year = 2012)

acs_2012 <- acs_2012 %>% 
  select(-moe, -NAME) %>% 
  spread(key = variable, value = estimate) %>% 
  mutate(Year = 2012,
         white_share = 100 * (white_pop/population),
         hisp_share = 100 * (hisp_pop/population),
         asian_share = 100 * (asian_pop/population),
         black_share = 100 * (black_pop/population),
         male_share = 100 * (male_pop/population),
         poverty_rate = 100 * (pov/population))

### Pull 2013 ACS county-level data 

acs_2013 <- get_acs(geography = "county",
                    variables = c(population = "B01001_001",
                                  white_pop = "B01001H_001",
                                  hisp_pop = "B01001I_001",
                                  asian_pop = "B02001_005",
                                  black_pop = "B02001_003",
                                  male_pop = "B01001_002",
                                  med_income = "B19013_001",
                                  med_age = "B01002_001",
                                  pov = "B17025_002"),
                    year = 2013)

acs_2013 <- acs_2013 %>% 
  select(-moe, -NAME) %>% 
  spread(key = variable, value = estimate) %>% 
  mutate(Year = 2013,
         white_share = 100 * (white_pop/population),
         hisp_share = 100 * (hisp_pop/population),
         asian_share = 100 * (asian_pop/population),
         black_share = 100 * (black_pop/population),
         male_share = 100 * (male_pop/population),
         poverty_rate = 100 * (pov/population))

### Pull 2014 ACS county-level data 

acs_2014 <- get_acs(geography = "county",
                    variables = c(population = "B01001_001",
                                  white_pop = "B01001H_001",
                                  hisp_pop = "B01001I_001",
                                  asian_pop = "B02001_005",
                                  black_pop = "B02001_003",
                                  male_pop = "B01001_002",
                                  med_income = "B19013_001",
                                  med_age = "B01002_001",
                                  pov = "B17025_002"),
                    year = 2014)

acs_2014 <- acs_2014 %>% 
  select(-moe, -NAME) %>% 
  spread(key = variable, value = estimate) %>% 
  mutate(Year = 2014,
         white_share = 100 * (white_pop/population),
         hisp_share = 100 * (hisp_pop/population),
         asian_share = 100 * (asian_pop/population),
         black_share = 100 * (black_pop/population),
         male_share = 100 * (male_pop/population),
         poverty_rate = 100 * (pov/population))

### Pull 2015 ACS county-level data 

acs_2015 <- get_acs(geography = "county",
                    variables = c(population = "B01001_001",
                                  white_pop = "B01001H_001",
                                  hisp_pop = "B01001I_001",
                                  asian_pop = "B02001_005",
                                  black_pop = "B02001_003",
                                  male_pop = "B01001_002",
                                  med_income = "B19013_001",
                                  med_age = "B01002_001",
                                  pov = "B17025_002"),
                    year = 2015)

acs_2015 <- acs_2015 %>% 
  select(-moe, -NAME) %>% 
  spread(key = variable, value = estimate) %>% 
  mutate(Year = 2015,
         white_share = 100 * (white_pop/population),
         hisp_share = 100 * (hisp_pop/population),
         asian_share = 100 * (asian_pop/population),
         black_share = 100 * (black_pop/population),
         male_share = 100 * (male_pop/population),
         poverty_rate = 100 * (pov/population))

### Pull 2016 ACS county-level data 

acs_2016 <- get_acs(geography = "county",
                    variables = c(population = "B01001_001",
                                  white_pop = "B01001H_001",
                                  hisp_pop = "B01001I_001",
                                  asian_pop = "B02001_005",
                                  black_pop = "B02001_003",
                                  male_pop = "B01001_002",
                                  med_income = "B19013_001",
                                  med_age = "B01002_001",
                                  pov = "B17025_002"),
                    year = 2016)

acs_2016 <- acs_2016 %>% 
  select(-moe, -NAME) %>% 
  spread(key = variable, value = estimate) %>% 
  mutate(Year = 2016,
         white_share = 100 * (white_pop/population),
         hisp_share = 100 * (hisp_pop/population),
         asian_share = 100 * (asian_pop/population),
         black_share = 100 * (black_pop/population),
         male_share = 100 * (male_pop/population),
         poverty_rate = 100 * (pov/population))

### Pull 2017 ACS county-level data 

acs_2017 <- get_acs(geography = "county",
                    variables = c(population = "B01001_001",
                                  white_pop = "B01001H_001",
                                  hisp_pop = "B01001I_001",
                                  asian_pop = "B02001_005",
                                  black_pop = "B02001_003",
                                  male_pop = "B01001_002",
                                  med_income = "B19013_001",
                                  med_age = "B01002_001",
                                  pov = "B17025_002"),
                    year = 2017)

acs_2017 <- acs_2017 %>% 
  select(-moe, -NAME) %>% 
  spread(key = variable, value = estimate) %>% 
  mutate(Year = 2017,
         white_share = 100 * (white_pop/population),
         hisp_share = 100 * (hisp_pop/population),
         asian_share = 100 * (asian_pop/population),
         black_share = 100 * (black_pop/population),
         male_share = 100 * (male_pop/population),
         poverty_rate = 100 * (pov/population))

### Pull 2018 ACS county-level data 

acs_2018 <- get_acs(geography = "county",
                    variables = c(population = "B01001_001",
                                  white_pop = "B01001H_001",
                                  hisp_pop = "B01001I_001",
                                  asian_pop = "B02001_005",
                                  black_pop = "B02001_003",
                                  male_pop = "B01001_002",
                                  med_income = "B19013_001",
                                  med_age = "B01002_001",
                                  pov = "B17025_002"),
                    year = 2018)

acs_2018 <- acs_2018 %>% 
  select(-moe, -NAME) %>% 
  spread(key = variable, value = estimate) %>% 
  mutate(Year = 2018,
         white_share = 100 * (white_pop/population),
         hisp_share = 100 * (hisp_pop/population),
         asian_share = 100 * (asian_pop/population),
         black_share = 100 * (black_pop/population),
         male_share = 100 * (male_pop/population),
         poverty_rate = 100 * (pov/population))

### Pull 2019 ACS county-level data 

acs_2019 <- get_acs(geography = "county",
                    variables = c(population = "B01001_001",
                                  white_pop = "B01001H_001",
                                  hisp_pop = "B01001I_001",
                                  asian_pop = "B02001_005",
                                  black_pop = "B02001_003",
                                  male_pop = "B01001_002",
                                  med_income = "B19013_001",
                                  med_age = "B01002_001",
                                  pov = "B17025_002"),
                    year = 2019)

acs_2019 <- acs_2019 %>% 
  select(-moe, -NAME) %>% 
  spread(key = variable, value = estimate) %>% 
  mutate(Year = 2019,
         white_share = 100 * (white_pop/population),
         hisp_share = 100 * (hisp_pop/population),
         asian_share = 100 * (asian_pop/population),
         black_share = 100 * (black_pop/population),
         male_share = 100 * (male_pop/population),
         poverty_rate = 100 * (pov/population))

### Append into single ACS data frame, rename GEOID as FIPS

acs_10_to_19 <- rbind(acs_2010, acs_2011, acs_2012, acs_2013, acs_2014, acs_2015, acs_2016, acs_2017, acs_2018, acs_2019)
acs_10_to_19 <- acs_10_to_19 %>% 
  mutate(GEOID = as.numeric(GEOID)) %>% 
  rename(FIPS = GEOID)


# Part 3: Merge Data into County-Year Panel -------------------------------

## Create four versions of the Appalachian counties data, one for each year in our study

appalachian_counties_10 <- appalachian_counties %>% 
  mutate(Year = 2010)
appalachian_counties_11 <- appalachian_counties %>% 
  mutate(Year = 2011)
appalachian_counties_12 <- appalachian_counties %>% 
  mutate(Year = 2012)
appalachian_counties_13 <- appalachian_counties %>% 
  mutate(Year = 2013)
appalachian_counties_14 <- appalachian_counties %>% 
  mutate(Year = 2014)
appalachian_counties_15 <- appalachian_counties %>% 
  mutate(Year = 2015)
appalachian_counties_16 <- appalachian_counties %>% 
  mutate(Year = 2016)
appalachian_counties_17 <- appalachian_counties %>% 
  mutate(Year = 2017)
appalachian_counties_18 <- appalachian_counties %>% 
  mutate(Year = 2018)
appalachian_counties_19 <- appalachian_counties %>% 
  mutate(Year = 2019)

appalachian_counties_panel <- rbind(appalachian_counties_10,
                                    appalachian_counties_11,
                                    appalachian_counties_12,
                                    appalachian_counties_13,
                                    appalachian_counties_14,
                                    appalachian_counties_15,
                                    appalachian_counties_16,
                                    appalachian_counties_17,
                                    appalachian_counties_18,
                                    appalachian_counties_19)

## Create variable for time since expansion and treatment variable

appalachian_counties_panel <- appalachian_counties_panel %>% 
  mutate(t_expansion = Year - year(expansion_date),
         treat = ifelse(t_expansion >= 0, medicaid_expansion, 0))

## Merge panel with overdose data

appalachian_counties_overdose_panel <- appalachian_counties_panel %>% 
  mutate(FIPS = as.numeric(FIPS)) %>% 
  left_join(overdose_data, by = c("FIPS", "Year"))

## Merge panel with ACS data (save as an RDS file)

year_county_panel <- appalachian_counties_overdose_panel %>% 
  left_join(acs_10_to_19, by = c("FIPS", "Year"))

# Part 4: Define "High" and "Low Risk" Counties ---------------------------

# Note: We define high risk counties as counties with a pre-expansion compound annual growth rate (CAGR) of drug overdose death rates above the 
# 75th percentile and similarly define "low risk" counties as counties with a pre-expansion CAGR of drug overdose death rates below the 25th 
# percentile. Counties in between the 25th and 75th percentiles of pre-expansion CAGR of drug overdose rates are simply defined as "moderate risk."

## Limit dataset to pre-expansion and select necessary variables, reshape to wide, calculate overdose death rate CAGR

pre_expansion_panel <- year_county_panel %>% 
  filter(between(t_expansion, -4, -1)) %>% 
  select("FIPS", "t_expansion", "estimated_overdose_death_rate", "geometry", "medicaid_expansion") %>% 
  spread(key = t_expansion, value = estimated_overdose_death_rate) %>% 
  mutate(pre_expansion_CAGR = (((`-1`/`-4`)^(1/3)) - 1) * 100) 

## Generate five-number summary for CAGR
five_num_CAGR <- fivenum(pre_expansion_panel$pre_expansion_CAGR)

## Code risk levels based on CAGR
pre_expansion_panel <- pre_expansion_panel %>% 
  mutate(risk = if_else(pre_expansion_CAGR > five_num_CAGR[4], "High", if_else(pre_expansion_CAGR < five_num_CAGR[2], "Low", "Moderate"))) %>% 
  select("FIPS", "geometry", "pre_expansion_CAGR", "risk", "medicaid_expansion")

## Save risk levels as a separate dataframe
risk_levels <- pre_expansion_panel %>% 
  select("FIPS", "pre_expansion_CAGR", "risk")

## Merge risk levels in to main panel and county dataset
year_county_panel <- year_county_panel %>% 
  left_join(risk_levels, by = "FIPS")

appalachian_counties <- appalachian_counties %>% 
  mutate(FIPS = as.numeric(FIPS)) %>% 
  left_join(risk_levels, by = "FIPS")

# Part 5: Save Panel and County Dasets as .Rds Files ----------------------

## Restrict county-year panel to years of analysis, tell R it's a panel, and save as .rds file
CY_panel <- year_county_panel %>% 
  filter(between(t_expansion, -4, 4))

# panel_data(year_county_panel, id = "COUNTY", wave = "Year")

saveRDS(CY_panel, file = "CY_panel.rds")

## Save counties dataset, this file is purely for mapping and tabulations, but no inference
saveRDS(appalachian_counties, file = "appalachian_counties.rds") 



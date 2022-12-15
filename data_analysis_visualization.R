
# Data Description --------------------------------------------------------

## Load panel data
CY_panel <- readRDS("CY_panel.rds")

## Load county data
appalachian_counties <- readRDS("appalachian_counties.rds")

## Tabulate counties by expansion status
expansion_count <- as.numeric(table(appalachian_counties$medicaid_expansion))
expansion_count


# Descriptive Statistics --------------------------------------------------

## Map counties by expansion status

appalachian_counties_map <- st_as_sf(appalachian_counties)

expansion_map <- tm_shape(appalachian_counties_map) + 
  tm_polygons("medicaid_expansion",
              title = "Medicaid Expansion Status",
              palette = 'Pastel1') +
  tm_borders()

expansion_map

## Line plot of drug overdose death rates over time, by expansion status

line_plot_panel <- CY_panel %>% 
  group_by(t_expansion, medicaid_expansion) %>% 
  summarize(estimated_overdose_death_rate = weighted.mean(
    estimated_overdose_death_rate, w = population, na.rm = T), .groups = 'drop')

overdose_line_plot <- line_plot_panel %>% 
  ggplot(aes(x = t_expansion,
             y = estimated_overdose_death_rate,
             color = medicaid_expansion)) +
  geom_line() +
  geom_point() +
  geom_vline(aes(xintercept = -1)) +
  scale_color_brewer(palette = "Pastel1", name = "Medicaid Expansion Status") +
  labs(x = "Years since Medicaid Expansion",
       y = "Drug Overdose Deaths per 100,000")

overdose_line_plot

## Diff-in-Means Table by Expansion Status

### Restrict data to the year prior to Medicaid expansion
t_expansion_county_panel <- CY_panel %>% 
  filter(t_expansion == -1) %>% 
  rename(weights = population,
         `Poverty Rate` = poverty_rate,
         `Median Age` = med_age,
         `Male Share` = male_share,
         `Black Share` = black_share,
         `Hispanic Share` = hisp_share,
         `White Share` = white_share,
         `Asian Share` = asian_share) %>%
  select('medicaid_expansion', 'Poverty Rate', 'Median Age', 'Male Share', 'Black Share', 'Hispanic Share', 'White Share', 'Asian Share')

### Show diff between treatment and control groups 
diff_in_means_table <- datasummary_balance(~medicaid_expansion,
                                           data = t_expansion_county_panel,
                                           fmt = 2,
                                           dinm_statistic = "p.value",
                                           title = "Difference-in-means between Expansion and Non-Expansion Counties, One Year Prior to Medicaid Expansion",
                                           notes = "Note: Observations are weighted by the population in each county.",
) %>% 
  kable_styling(latex_options = "HOLD_position")

diff_in_means_table


# Findings ----------------------------------------------------------------

## Diff-in-Diff Estimates

### Subset high-risk counties
high_risk_panel <- CY_panel %>% 
  filter(risk == "High")


### Subset low-risk counties
low_risk_panel <- CY_panel %>% 
  filter(risk == "Low")

### Generate estimates
did_models <- list(
  "All Counties" <- feols(estimated_overdose_death_rate ~ 
                            treat + poverty_rate + med_age + male_pop + black_pop + hisp_pop + asian_pop | COUNTY + Year, 
                          data = CY_panel,
                          weights = CY_panel$population,
                          cluster = CY_panel$COUNTY),
  
  "High Risk" <- feols(estimated_overdose_death_rate ~ 
                         treat + poverty_rate + med_age + male_pop + black_pop + hisp_pop + asian_pop | COUNTY + Year, 
                       data = high_risk_panel,
                       weights = high_risk_panel$population,
                       cluster = high_risk_panel$COUNTY),
  
  "Low Risk" <- feols(estimated_overdose_death_rate ~ 
                        treat + poverty_rate + med_age + male_pop + black_pop + hisp_pop + asian_pop | COUNTY + Year, 
                      data = low_risk_panel,
                      weights = low_risk_panel$population,
                      cluster = low_risk_panel$COUNTY)
)

names(did_models) <- c("All Counties", "High Risk", "Low Risk")

#### Set up an object to customize regression table goodness-of-fit statistics
gm <- tibble::tribble(
  ~raw,        ~clean,          ~fmt,
  "nobs",               "N",                  0,
  "r.squared",          "R-squared",      3,
  "adj.r.squared",      "Adj. R-squared", 3,
  "FE: COUNTY",         "County FEs",          0,
  "FE: Year",    "Year FEs",    0
)

#### Display results with added formatting options    
did_estimation_results <- modelsummary(did_models,
                                       fmt = "%.4f",
                                       coef_omit = 'Intercept',
                                       coef_rename = c("treat" = "Medicaid Expansion", 
                                                       "poverty_rate" = "Poverty Rate (0-100)",
                                                       "med_age" = "Median Age",
                                                       "male_pop" = "Male Population Share (0-100)",
                                                       "black_pop" = "Black Population Share",
                                                       "hisp_pop" = "Hispanic Population Share",
                                                       "asian_pop" = "Asian Population Share"),
                                       gof_map = gm,
                                       title = 'Effect of Medicaid Expansion on Drug Overdose Death Rates',
                                       stars = c('*' = .1, '**' = .05, '***' = .01),
                                       notes = 'Robust standard errors clustered by county are shown in parentheses.
                      Observations are weighted by the population in each county.') %>% 
  kable_styling(latex_options = c("hold_position", "scale_down"))

did_estimation_results

## Event Study Estimates

### Estimates Plot for ALL COUNTIES
ES_mod <- feols(
  estimated_overdose_death_rate ~ i(factor(t_expansion), as.numeric(medicaid_expansion), ref = -1) 
  + poverty_rate + med_age + male_pop + black_pop + hisp_pop + asian_pop
  | COUNTY + Year,
  data = CY_panel, 
  weight = CY_panel$population,
  cluster = CY_panel$COUNTY)

iplot(
  ES_mod,
  pt.join = TRUE,
  ci.join = TRUE,
  ci.lwd = 0,
  grid.par = list(vert = FALSE),
  main = "",
  ylab = "Est. Effect on Overdose Death Rates",
  xlab = "Years since Medicaid Expansion")

### Estimates Plot for HIGH RISK COUNTIES
ES_mod_high_risk <- feols(
  estimated_overdose_death_rate ~ i(factor(t_expansion), as.numeric(medicaid_expansion), ref = -1) 
  + poverty_rate + med_age + male_pop + black_pop + hisp_pop + asian_pop
  | COUNTY + Year,
  data = high_risk_panel, 
  weight = high_risk_panel$population,
  cluster = high_risk_panel$COUNTY)

iplot(
  ES_mod_high_risk,
  pt.join = TRUE,
  ci.join = TRUE,
  ci.lwd = 0,
  grid.par = list(vert = FALSE),
  main = "",
  ylab = "Est. Effect on Overdose Death Rates",
  xlab = "Years since Medicaid Expansion")

### Estimates Plot for LOW RISK COUNTIES
ES_mod_low_risk <- feols(
  estimated_overdose_death_rate ~ i(factor(t_expansion), as.numeric(medicaid_expansion), ref = -1) 
  + poverty_rate + med_age + male_pop + black_pop + hisp_pop + asian_pop
  | COUNTY + Year,
  data = low_risk_panel, 
  weight = low_risk_panel$population,
  cluster = low_risk_panel$COUNTY)

iplot(
  ES_mod_low_risk,
  pt.join = TRUE,
  ci.join = TRUE,
  ci.lwd = 0,
  grid.par = list(vert = FALSE),
  main = "",
  ylab = "Est. Effect on Overdose Death Rates",
  xlab = "Years since Medicaid Expansion")

#### Tabulated Event Study Estimates (Appendix III in the paper)
ES_models <- list(
  "All Counties" <- feols(
    estimated_overdose_death_rate ~ i(factor(t_expansion), as.numeric(medicaid_expansion), ref = -1) 
    + poverty_rate + med_age + male_pop + black_pop + hisp_pop + asian_pop
    | COUNTY + Year,
    data = CY_panel, 
    weight = CY_panel$population,
    cluster = CY_panel$COUNTY),
  
  "High Risk" <- feols(
    estimated_overdose_death_rate ~ i(factor(t_expansion), as.numeric(medicaid_expansion), ref = -1) 
    + poverty_rate + med_age + male_pop + black_pop + hisp_pop + asian_pop
    | COUNTY + Year,
    data = high_risk_panel, 
    weight = high_risk_panel$population,
    cluster = high_risk_panel$COUNTY),
  
  "Low Risk" <- feols(
    estimated_overdose_death_rate ~ i(factor(t_expansion), as.numeric(medicaid_expansion), ref = -1) 
    + poverty_rate + med_age + male_pop + black_pop + hisp_pop + asian_pop
    | COUNTY + Year,
    data = low_risk_panel,
    weights = low_risk_panel$population,
    cluster = low_risk_panel$COUNTY)
)

names(ES_models) <- c("All Counties", "High Risk", "Low Risk")

#### Set up an object to customize regression table goodness-of-fit statistics
gm <- tibble::tribble(
  ~raw,        ~clean,          ~fmt,
  "nobs",               "N",                  0,
  "r.squared",          "R-squared",      3,
  "adj.r.squared",      "Adj. R-squared", 3,
  "FE: COUNTY",         "County FEs",          0,
  "FE: Year",    "Year FEs",    0
)

#### Display results with added formatting options    
ES_estimation_results <- modelsummary(ES_models,
                                      fmt = "%.4f",
                                      coef_omit = "^(?!.*_expansion)", 
                                      coef_rename = c("factor(t_expansion)::-4:as.numeric(medicaid_expansion)" = "(Year -4) * Expansion",
                                                      "factor(t_expansion)::-3:as.numeric(medicaid_expansion)" = "(Year -3) * Expansion",
                                                      "factor(t_expansion)::-2:as.numeric(medicaid_expansion)" = "(Year -2) * Expansion",
                                                      "factor(t_expansion)::0:as.numeric(medicaid_expansion)" = "(Year 0) * Expansion",
                                                      "factor(t_expansion)::1:as.numeric(medicaid_expansion)" = "(Year 1) * Expansion",
                                                      "factor(t_expansion)::2:as.numeric(medicaid_expansion)" = "(Year 2) * Expansion",
                                                      "factor(t_expansion)::3:as.numeric(medicaid_expansion)" = "(Year 3) * Expansion",
                                                      "factor(t_expansion)::4:as.numeric(medicaid_expansion)" = "(Year 4) * Expansion"),
                                      gof_map = gm,
                                      title = 'Effect of Medicaid Expansion on Drug Overdose Death Rates',
                                      stars = c('*' = .1, '**' = .05, '***' = .01),
                                      notes = 'Control covariates include county-level poverty rate, median age, male pop. share, Black pop. share, White pop. share, 
                      Hispanic pop. share, and Asian pop. share.
                      Robust standard errors clustered by county are shown in parentheses.
                      Observations are weighted by the population in each county.') %>% 
  kable_styling(latex_options = "hold_position")

ES_estimation_results


# Appendix II: Defining "High" and "Low Risk" Counties -------------------

## Map of Pre-Expansion Overdose Death Rate CAGR 

### Covert counties datafile to sf
pre_expansion_shapes <- st_as_sf(appalachian_counties)

CAGR_map <- tm_shape(pre_expansion_shapes) + 
  tm_polygons("pre_expansion_CAGR",
              title = "CAGR of Overdose Death Rates",
              palette = 'PuRd') +
  tm_borders()

CAGR_map

## Five-number Summary of Pre-Expansion Overdose Death Rate CAGR
five_num_CAGR <- fivenum(appalachian_counties$pre_expansion_CAGR)

## Two-Way Table of Risk Levels by Expansion Status
expansion_risk_count <- table(appalachian_counties$medicaid_expansion, appalachian_counties$risk)

expansion_risk_count %>% 
  kable(align = "c",
        caption = "Two-Way Table of County Expansion and Risk Status")

## Diff-in-Means Table High Risk vs. Low Risk
t_risk_expansion_county_panel <- CY_panel %>% 
  filter(t_expansion == -1) %>% 
  filter(risk != "Moderate") %>% 
  rename(weights = population,
         `Poverty Rate` = poverty_rate,
         `Median Age` = med_age,
         `Male Share` = male_share,
         `Black Share` = black_share,
         `Hispanic Share` = hisp_share,
         `White Share` = white_share,
         `Asian Share` = asian_share) %>%
  select('risk', 'Poverty Rate', 'Median Age', 'Male Share', 'Black Share', 'Hispanic Share', 'White Share', 'Asian Share')

# Show diff between treatment and control groups 
diff_in_means_table_2 <- datasummary_balance(~risk,
                                             data = t_risk_expansion_county_panel,
                                             fmt = 2,
                                             dinm_statistic = "p.value",
                                             title = "Difference-in-means between High and Low Risk Counties, One Year Prior to Medicaid Expansion",
                                             notes = "Note: Observations are weighted by the population in each county.",
) %>% 
  kable_styling(latex_options = "HOLD_position")

diff_in_means_table_2







# Data Issues

## ACS only available in five-year samples, single year ACS has a lot of missing data. Should we expand the sample in a wider direction?

We could widen the study period to 2009-2013 and 2014-2018, allowing us to use those two sets of survey data. This would require some back-end reconfiguration of the panel dataset, but not too tricky. Would mean that overdose death rates are period averages, rather than single-year estimates.

Harold: try to find ACS1 microdata and aggregate up from smaller subdivisions (from the [Census Bureau website](https://www.census.gov/data/developers/data-sets/acs-1year.html), ACS1 data is only available for geographies of population 65,000 and above). Looks like we will need to proceed with a county-five year period panel.

See if we can aggregrate up to PUMAs to get the controls. How often do PUMAs change? According to this [handbook](https://www.census.gov/content/dam/Census/library/publications/2020/acs/acs_pums_handbook_2020_ch02.pdf), PUMAs only are redrawn every 10 years, so this is actually a potential solution! We would just need a crosswalk between 2010 PUMAs and counties. According to this [source](https://iecam.illinois.edu/browse/about-public-use-microdata-areas-pumas), PUMAs and county lines are fairly consistent. RIIPPPPPP actually counties do not map to PUMA well, sometimes multiple counties are included in one PUMA, othertimes one county is split into multiple PUMAs.
 
## Sex/Race sub-samples

Doesn't seem possible because this is county-level data? Could try limiting to majority Black or majority Male counties, but would imagine that sample size issues could make things challenging here.

## Should we look outside of Appalachia?

I worry a bit about defining proper scope here. Also probably exposes us to additional OVB concerns.

## Should we drop Pennsylvania?

Would prefer to have a discrete treatment dummy?

Harold: Keep Pennsylvania for staggered implementation; create time-staggered treatment variable.

## Should we attempt to do matching?

Not a ton of differences between treatment and control groups outside of overdose death rates and racial composition. Still worth trying to match?

## Should we control for number of drug addiction clinics by county?

Is this data even available? IT IS [here](https://www.samhsa.gov/data/data-we-collect/n-ssats-national-survey-substance-abuse-treatment-services) but they don't include county FIPS codes :(

Harold: Try to see what's out there.

## Other controls

- Education?

## Diff-in-Means for descriptive statistics and weights for population

## Model for longer time frame ('16 and '18)

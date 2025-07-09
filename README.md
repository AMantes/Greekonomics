# Greekonomics: The True State of the Greek Economy

### Overview

This is a data-driven exploration of the Greek economy (Greekonomics: Episode 51 and 52) compared to the EU27 and the 10 poorest EU countries. This repository contains an R script that generates visualizations for key economic indicators, including real disposable income, GDP per capita, unemployment, government debt, current account balance, sectoral investment, overqualification rates, and poverty risk.

### Data Sources

All data is sourced from Eurostat (https://ec.europa.eu/eurostat):


- tepsr_wc310: Real gross disposable income per capita
- sdg_10_10: Real GDP per capita (Purchasing Power Standards)
- tipsna40: Real GDP per capita (constant 2015 prices)
- tipsgo10: Government debt as a percentage of GDP
- tipsbp20: Current account balance as a percentage of GDP
- nama_10_a64_p5: Sectoral investment (NACE Rev.2)
- tipsun20: Unemployment rates (youth and total)
- lfsa_eoqgan: Overqualification rates
- tipslc10: People at risk of poverty or social exclusion
- nama_10_lp_ulc: Compensation of employees per hour worked
- tipslc30: Severely materially and socially deprived people
- sdg_16_20: Population reporting occurrence of crime, violence or vandalism in their area
- sdg_03_60: Self-reported unmet need for medical examination and care
- sdg_01_50: Housing cost overburden rate by poverty status
- tipsho20: Housing index
- nama_10_lp_ulc: Unit labour costs
- tipsun20: Unemployment
- lfsa_eoqgan: Overqualification

### Requirements

To run the script, you’ll need R and the following packages:

- tidyverse
- ggplot2
- eurostat
- dplyr
- showtext
-  tidyr
- scales

Install them with:

install.packages(c("tidyverse", "ggplot2", "eurostat", "dplyr", "showtext", "ggtext", "tidyr", "scales"))

Ensure you have an internet connection to fetch Eurostat data and Google Fonts (Roboto and Roboto Condensed) for the visualizations’ typography.

Note: To run the income distribution analysis, you will need to download the SILC (Survey on Income and Living Conditions) and HBS (Household Budget Survey) microdata databases separately. These databases are not included in this repository and must be obtained from here: https://www.statistics.gr/public-use-files

### Visualizations

The script produces the following visualizations:

- Real Gross Disposable Income Per Capita
- Real GDP Per Capita (PPS and Constant Prices)
- Government Debt: Debt-to-GDP ratio
- Current Account Balance: Percentage of GDP
- Sectoral Investment: Capital stock across the top 7 industries
- Unemployment Rates: Youth (15-24) and total (15-74)
- Overqualification Rates: Share of tertiary-educated workers in low/mid-skill jobs
- Poverty or Social Exclusion: Percentage of the population at poverty risk
- Labor Productivity: Nominal compensation per hour worked

### Data: 

Eurostat (https://ec.europa.eu/eurostat)


### Author: 

Achilleas Mantes

### Platform: 

Developed for Greekonomics (https://www.youtube.com/@Greekonomics)

### License

This project is licensed under the MIT License. You are free to use, modify, and distribute the code, provided you credit the original author and data sources.

#### Contact

Have questions or feedback? Feel free to reach out.

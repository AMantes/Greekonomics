# Greekonomics: The True State of the Greek Economy

### Overview

This is a data-driven exploration of the Greek economy (Greekonomics: Episode 51) compared to the EU27 and the 10 poorest EU countries. This repository contains an R script that generates visualizations for key economic indicators, including real disposable income, GDP per capita, unemployment, government debt, current account balance, sectoral investment, overqualification rates, and poverty risk.

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

### Usage

- Clone or Download: Clone this repository or download the Greekonomics_51_public.R script.
- Install Packages: Ensure all required R packages are installed (see above).
- Run the Script: Execute the script in R or RStudio. It will:

  - Fetch data from Eurostat for the specified indicators.
  - Process data to compare Greece, EU27, and the Bottom 10 EU countries.
  - Generate visualizations. The script displays plots directly.
- Run the script with:
  - source("Greekonomics_51_public.R")

### Visualizations

The script produces the following visualizations, as they appear in the Greekonomics episode 51:

- Real Gross Disposable Income Per Capita
- Real GDP Per Capita (PPS and Constant Prices)
- Government Debt: Debt-to-GDP ratio
- Current Account Balance: Percentage of GDP
- Sectoral Investment: Capital stock across the top 7 industries
- Unemployment Rates: Youth (15-24) and total (15-74)
- Overqualification Rates: Share of tertiary-educated workers in low/mid-skill jobs
- Poverty or Social Exclusion: Percentage of the population at poverty risk
- Labor Productivity: Nominal compensation per hour worked

### Data: Eurostat (https://ec.europa.eu/eurostat)


### Author: 

Achilleas Mantes

### Platform: 

Developed for Greekonomics (https://www.youtube.com/@Greekonomics)

### License

This project is licensed under the MIT License. You are free to use, modify, and distribute the code, provided you credit the original author and data sources.

#### Contact

Have questions or feedback? Feel free to reach out.

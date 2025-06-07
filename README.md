# Greekonomics

Greekonomics: Economic Analysis of the Greek Economy (Episode 51)

Overview

This is a data-driven exploration of the Greek economy compared to the EU27 and the 10 poorest EU countries. This repository contains an R script that generates visualizations for key economic indicators, including real disposable income, GDP per capita, unemployment, government debt, current account balance, sectoral investment, overqualification rates, and poverty risk. These visualizations support a presentation for a popular science YouTube channel, focusing on labor market dynamics and economic hysteresis in Greece.

The script was developed for a high-profile event and complements two YouTube videos (one already trending with over 330,000 views). It aims to make complex economic data accessible and engaging while maintaining academic rigor.

Purpose

This project analyzes Greece's economic performance post-2008 crisis, highlighting persistent challenges like high youth unemployment, low-skill job dominance, and stagnant productivity. The visualizations compare Greece with the EU27 average and a cluster of the 10 poorest EU countries, providing insights into structural economic issues.

Data Sources

All data is sourced from Eurostat, the statistical office of the European Union. Specific datasets used include:





tepsr_wc310: Real gross disposable income per capita



sdg_10_10: Real GDP per capita (PPS)



tipsna40: Real GDP per capita (constant prices)



tipsgo10: Government debt as a percentage of GDP



tipsbp20: Current account balance as a percentage of GDP



nama_10_a64_p5: Sectoral investment (NACE Rev.2)



tipsun20: Unemployment rates (youth and total)



lfsa_eoqgan: Overqualification rates



tipslc10: People at risk of poverty or social exclusion



nama_10_lp_ulc: Compensation of employees per hour worked

Requirements

To run the script, you need the following R packages:

library(tidyverse)
library(ggplot2)
library(eurostat)
library(dplyr)
library(paletteer)
library(grid)
library(gridExtra)
library(ggpubr)
library(showtext)
library(ggtext)
library(tidyr)
library(scales)

Install them using:

install.packages(c("tidyverse", "ggplot2", "eurostat", "dplyr", "paletteer", "grid", "gridExtra", "ggpubr", "showtext", "ggtext", "tidyr", "scales"))

Additionally, ensure you have internet access to fetch data from Eurostat and Google Fonts for typography (Roboto and Roboto Condensed).

Usage





Clone this repository or download the greekonomics.R script.



Ensure all required R packages are installed.



Run the script in R or RStudio. It will:





Fetch data from Eurostat for the specified indicators.



Process the data to compare Greece, EU27, and the average of the 10 poorest EU countries (Bulgaria, Hungary, Latvia, Croatia, Poland, Lithuania, Slovakia, Estonia, Czechia, Romania).



Generate visualizations with a custom theme_greekonomics for consistent styling.



The script produces plots for each indicator, which can be saved or displayed directly.

Example:

source("greekonomics.R")

To save plots, add ggsave() after each plot, e.g.:

ggsave("real_gdp_per_capita.png", plot_real_GDP_per_capita, width = 8, height = 6, dpi = 300)

Visualizations

The script generates the following plots:





Real Gross Disposable Income Per Capita: Index (2008 = 100), showing Greece's stagnation compared to EU27 and Bottom 10.



Real GDP Per Capita (PPS and Constant Prices): Trends in purchasing power and constant price terms.



Government Debt: Debt-to-GDP ratio, highlighting Greece's high debt burden.



Current Account Balance: As a percentage of GDP, showing trade and financial flows.



Sectoral Investment: Capital stock across top 7 industries (e.g., real estate, manufacturing).



Unemployment Rates: Youth (15-24) and total (15-74) unemployment trends.



Overqualification Rates: Share of workers with tertiary education in low/mid-skill jobs.



Poverty or Social Exclusion: Percentage of the population at risk.



Labor Productivity: Compensation per hour worked as a proxy for productivity trends.

Each plot uses a custom color palette and the theme_greekonomics for a professional, visually appealing style.

Credits





Data: Eurostat (https://ec.europa.eu/eurostat)



Typography: Google Fonts (Roboto, Roboto Condensed)



Inspiration: This project was developed for [Influencer's YouTube Channel Name], a popular science platform dedicated to making economics accessible.



Author: [Your Name], economic analyst for Greekonomics project.

License

This project is licensed under the MIT License. Feel free to use, modify, and distribute the code, provided you give credit to the original author and data sources.

Contact

For questions or feedback, reach out via [Your Contact Info, e.g., email or GitHub profile]. Follow [Influencer's YouTube Channel Name] for updates on the Greekonomics project!

# Author: Achilleas Mantes

# Created for: https://greekonomics.gr

# Data Source: Eurostat (https://ec.europa.eu/eurostat)

# License: MIT License

# Note: SILC & HBS are microdata and can be accessed upon request here: https://www.statistics.gr/public-use-files

# Load necessary libraries
library(tidyverse)
library(ggplot2)
library(eurostat)
library(dplyr)
library(showtext)
library(tidyr)
library(scales)
library(readxl)     
library(survey)     
library(tidyverse)  
library(gridExtra) 
library(sysfonts) 



################################################################################
######################## Custom theme for plots ################################
################################################################################

# Google Fonts for typography
font_add_google("Roboto", "roboto")
font_add_google("Roboto Condensed", "roboto_condensed")
showtext_auto()

# Custom theme for Greekonomics
theme_greekonomics <- function() {
  theme_minimal(base_family = "roboto") +
    theme(
      # Background and panel
      panel.background = element_rect(fill = "#F7F7F7", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      panel.grid.major = element_line(color = "#D3D3D3", size = 0.3),
      panel.grid.minor = element_blank(),
      
      # Title and subtitle
      plot.title = element_text(
        family = "roboto_condensed",
        size = 16,
        face = "bold",
        hjust = 0,
        margin = margin(b = 10)
      ),
      plot.subtitle = element_text(
        family = "roboto",
        size = 12,
        hjust = 0,
        margin = margin(b = 10)
      ),
      
      # Axis styling
      axis.title = element_text(size = 12, face = "bold"),
      axis.text = element_text(size = 10, color = "grey30"),
      axis.line = element_line(color = "grey30", size = 0.5),
      axis.ticks = element_line(color = "grey30", size = 0.5),
      
      # Legend styling
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.text = element_text(size = 10, family = "roboto"),
      legend.background = element_rect(fill = "white", color = NA),
      legend.key = element_rect(fill = "white", color = NA),
      
      # Plot margins
      plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
    )
}

# Define a color palette
colors_financial <- c(
  "EL" = "#1B3C69",            # Greece
  "EU27_2020" = "#4A4A4A",     # EU27
  "Bottom_10_Avg" = "#901628"  # Bottom 10
)



################################################################################
########################### List of Countries ##################################
################################################################################

# List of countries: Greece and EU 27 (average)
list_of_countries <- c("EL", "EU27_2020")

# Bottom 10 cluster
selected_countries <- c("BG", # Bulgaria
                        "HU", # Hungary
                        "LV", #Lativa
                        "HR", # Croatia
                        "PL", #Poland
                        "LT", # Lithuania 
                        "SK", # Slovakia
                        "EE", # Estonia
                        "CZ", # Czechia
                        "RO"  # Romania
)





################################################################################
########################## Real GDP per capita (PPS) ###########################
################################################################################

# Fetch Eurostat data
id <- "sdg_10_10"
data <- get_eurostat(id, time_format = "num")

# Filter data
filtered_data <- data %>%
  rename(time = TIME_PERIOD) %>%
  filter(
    na_item == "EXP_PPS_EU27_2020_HAB"
  ) %>%
  select(geo, time, values)


# Filter Greece and EU27
greece_eu_data <- filtered_data %>%
  filter(geo %in% c("EL", "EU27_2020"))

# Calculate average for bottom 10 countries
bottom_10_data <- filtered_data %>%
  filter(geo %in% selected_countries) %>%
  group_by(time) %>%
  summarise(values = mean(values, na.rm = TRUE), .groups = "drop") %>%
  mutate(geo = "Bottom_10_Avg")

# Combine datasets
comparison_data <- bind_rows(greece_eu_data, bottom_10_data)


# Create the plot
plot_gdp_per_capita <- ggplot(
  comparison_data,
  aes(x = time, y = values, color = geo)
) +
  geom_line(size = 1, alpha = 0.9) +
  geom_point(size = 2, shape = 21, fill = "white", stroke = 0.5) +
  scale_color_manual(
    values = colors_financial,
    labels = c(
      "EL" = "Ελλάδα",
      "EU27_2020" = "ΕΕ27 (2020)",
      "Bottom_10_Avg" = "Μέσος Όρος Bottom 10"),
    drop = FALSE
  ) +
  scale_y_continuous(
    breaks = seq(0, ceiling(max(comparison_data$values, na.rm = TRUE) / 5000) * 5000, by = 5000),
    expand = c(0.05, 0.05),
    labels = scales::comma
  ) +
  scale_x_continuous(
    breaks = seq(min(comparison_data$time, na.rm = TRUE), max(comparison_data$time, na.rm = TRUE), by = 2),
    expand = c(0.05, 0.05)
  ) +
  labs(
    title = "Kατά κεφαλήν ΑΕΠ προσαρμοσμένο σε Ισοτιμία Αγοραστικής Δύναμης",
    subtitle = "PPS (έτος βάσης 2020)",
    x = "Έτος",
    y = "Ευρώ (PPS)",
    caption = "Πηγή: Eurostat (sdg_10_10)"
  ) +
  theme_financial() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5)
  )

# Display the plot
print(plot_gdp_per_capita)


################################################################################
############################ Real Disposable Income ############################
################################################################################


id <- "tepsr_wc310"

# Retrieve and prepare the data
data <- get_eurostat(id, time_format = "num") %>%
  rename(time = TIME_PERIOD)



# Filter data, rename TIME_PERIOD, and select relevant columns
filtered_data <- data %>%
  #  rename(time = TIME_PERIOD) %>%
  filter(geo %in% list_of_countries, unit == "CP_MNAC") %>%
  select(geo, time, values, unit)

# bottom 10
Bottom_10_Avg <- data %>%
  filter(geo %in% selected_countries, unit == "CP_MNAC") %>%
  group_by(time) %>%
  summarise(values = mean(values, na.rm = TRUE), .groups = "drop") %>%
  mutate(geo = "Bottom_10_Avg", unit = "CP_MNAC")





combined_data <- bind_rows(filtered_data, Bottom_10_Avg)


# Create the enhanced plot
plot_real_gross_YD_per_capita_tepsr_wc310 <- ggplot(
  combined_data, 
  aes(x = time, y = values, color = geo, linetype = geo)
)  +
  geom_line(size = 1, alpha = 0.9) +
  geom_point(size = 2, shape = 21, fill = "white", stroke = 0.5) +
  scale_color_manual(
    values = colors_financial,
    labels = c(
      "EL" = "Ελλάδα",
      "EU27_2020" = "ΕΕ27 (2020)",
      "Bottom_10_Avg" = "Μέσος Όρος Bottom 10"),
    drop = FALSE
  ) +
  scale_y_continuous(
    breaks = seq(floor(min(combined_data$values, na.rm = TRUE)), 
                 ceiling(max(combined_data$values, na.rm = TRUE)), 
                 by = 10),
    expand = expansion(mult = c(0.1, 0.1)),
    labels = function(x) paste0(x)
  ) +
  scale_x_continuous(
    breaks = seq(min(combined_data$time, na.rm = TRUE), 
                 max(combined_data$time, na.rm = TRUE), 
                 by = 2),
    expand = c(0.05, 0.05)
  ) +
  labs(
    title = "Κατά κεφαλήν πραγματικό YD",
    subtitle = "Δείκτης (2008=100), Επιλεγμένες Χώρες και ΕΕ27",
    x = "Έτος",
    y = "Δείκτης (2008=100)",
    caption = "Πηγή: Eurostat (tepsr_wc310)"
  ) +
  theme_financial()

print(plot_real_gross_YD_per_capita_tepsr_wc310)


################################################################################
############################## Overqualification ###############################
################################################################################

id <- "lfsa_eoqgan"
data <- get_eurostat(id, time_format = "num")

# Filter data
filtered_data <- data %>%
  rename(time = TIME_PERIOD) %>%
  filter(
    age == "Y20-64",
    citizen == "TOTAL",
    sex == "T"
  ) %>%
  select(geo, time, values)

# filter Greece and EU27 data
greece_eu_data <- filtered_data %>%
  filter(geo %in% c("EL", "EU27_2020"))

# Calculate average for bottom 10 countries
bottom_10_data <- filtered_data %>%
  filter(geo %in% selected_countries) %>%
  group_by(time) %>%
  summarise(values = mean(values, na.rm = TRUE), .groups = "drop") %>%
  mutate(geo = "Bottom_10_Avg")


# Combine datasets
comparison_data <- bind_rows(greece_eu_data, bottom_10_data)


# Plot: Historical overqualification rates by geo
plot_overqualification <- ggplot(
  comparison_data,
  aes(x = time, y = values, color = geo)
) +
  geom_line(size = 1, alpha = 0.9) +
  geom_point(size = 2, shape = 21, fill = "white", stroke = 0.5) +
  scale_color_manual(
    values = colors_financial,
    labels = c(
      "EL" = "Ελλάδα",
      "EU27_2020" = "ΕΕ27 (2020)",
      "Bottom_10_Avg" = "Μέσος Όρος Bottom 10"
    ),
    drop = FALSE
  ) +
  scale_y_continuous(
    breaks = seq(0, ceiling(max(comparison_data$values, na.rm = TRUE)), by = 5),
    expand = c(0.05, 0.05),
    labels = scales::percent_format(scale = 1)
  ) +
  scale_x_continuous(
    breaks = seq(min(comparison_data$time, na.rm = TRUE), max(comparison_data$time, na.rm = TRUE), by = 2),
    expand = c(0.05, 0.05)
  ) +
  labs(
    title = "Ποσοστό Υπερπροσοντούχων στην απαπασχόληση",
    subtitle = "Ποσοστό Εργαζομένων με Τριτοβάθμια Εκπαίδευση σε Χαμηλής/Μεσαίας Δεξιότητας Θέσεις",
    x = "Έτος",
    y = "% Εργαζομένων",
    caption = "Πηγή: Eurostat (lfsa_eoqgan)"
  ) +
  theme_financial() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5)
  )

# Display the plot
print(plot_overqualification)

################################################################################
################################ Unemployment ##################################
################################################################################


############################## Youth Unemployment ############################## 

id <- "tipsun20"


# Retrieve and prepare the data
data <- get_eurostat(id, time_format = "num") %>%
  rename(time = TIME_PERIOD)


# Filter data, rename TIME_PERIOD, and select relevant columns
filtered_data <- data %>%
  # rename(time = TIME_PERIOD) %>%
  filter(geo %in% list_of_countries, age == "Y15-24") %>%
  select(geo, time, values)


bottom_10_data <- data %>%
  filter(geo %in% selected_countries, age == "Y15-24") %>%
  group_by(time) %>%
  summarise(values = mean(values, na.rm = TRUE), .groups = "drop") %>%
  mutate(geo = "Bottom_10_Avg")


# Combine all data
combined_data <- bind_rows(filtered_data, bottom_10_data)

plot_unemp_1524_tipsun20 <- ggplot(
  combined_data, 
  aes(x = time, y = values, color = geo, linetype = geo)
) +
  geom_line(size = 1, alpha = 0.9) +
  geom_point(size = 2, shape = 21, fill = "white", stroke = 0.5) +
  scale_color_manual(
    values = colors_financial,
    labels = c(
      "EL" = "Ελλάδα",
      "EU27_2020" = "ΕΕ27 (2020)",
      "Bottom_10_Avg" = "Μέσος Όρος Bottom 10"
    ),
    drop = FALSE
  ) +
  scale_y_continuous(
    breaks = seq(0, ceiling(max(combined_data$values, na.rm = TRUE)), by = 5),
    expand = c(0.05, 0.05),
    labels = function(x) paste0(x, "%")
  ) +
  scale_x_continuous(
    breaks = seq(min(combined_data$time, na.rm = TRUE), 
                 max(combined_data$time, na.rm = TRUE), 
                 by = 2),
    expand = c(0.05, 0.5)
  ) +
  labs(
    title = "Νεανική Ανεργία (15-24 ετών)",
    subtitle = "Ποσοστό του Εργατικού Δυναμικού, Επιλεγμένες Χώρες και ΕΕ27",
    x = "Έτος",
    y = "% Εργατικού Δυναμικού",
    caption = "Πηγή: Eurostat"
  ) +
  theme_financial()


# Optional: Display the plot
print(plot_unemp_1524_tipsun20)


############################## Total Unemployment ############################## 


# Filter data, rename TIME_PERIOD, and select relevant columns
filtered_data <- data %>%
  # rename(time = TIME_PERIOD) %>%
  filter(geo %in% list_of_countries, age == "Y15-74") %>%
  select(geo, time, values)

bottom_10_data <- data %>%
  filter(geo %in% selected_countries, age == "Y15-74") %>%
  group_by(time) %>%
  summarise(values = mean(values, na.rm = TRUE), .groups = "drop") %>%
  mutate(geo = "Bottom_10_Avg")



combined_data <- bind_rows(filtered_data, bottom_10_data)


plot_unemp_1574 <- ggplot(
  combined_data,
  aes(x = time, y = values, color = geo, linetype = geo)
) +
  geom_line(size = 1, alpha = 0.9) +
  geom_point(size = 2, shape = 21, fill = "white", stroke = 0.5) +
  scale_color_manual(
    values = colors_financial,
    labels = c(
      "EL" = "Ελλάδα",
      "EU27_2020" = "ΕΕ27 (2020)",
      "Bottom_10_Avg" = "Μέσος Όρος Bottom 10"
    ),
    drop = FALSE
  ) +
  scale_y_continuous(
    breaks = seq(0, ceiling(max(combined_data$values, na.rm = TRUE)), by = 5),
    expand = c(0.05, 0.05),
    labels = function(x) paste0(x, "%")
  ) +
  scale_x_continuous(
    breaks = seq(min(combined_data$time, na.rm = TRUE), 
                 max(combined_data$time, na.rm = TRUE), 
                 by = 2),
    expand = c(0.05, 0.5)
  ) +
  labs(
    title = "Ανεργία (15-74 ετών)",
    subtitle = "Ποσοστό του Εργατικού Δυναμικού, Επιλεγμένες Χώρες και ΕΕ27",
    x = "Έτος",
    y = "% Εργατικού Δυναμικού",
    caption = "Πηγή: Eurostat"
  ) +
  theme_financial()

print(plot_unemp_1574)

################################################################################
########################### Real hourly wage rate ##############################
################################################################################


# Retrieve Eurostat data
id <- "nama_10_lp_ulc"
data <- get_eurostat(id, time_format = "num")

# Process productivity data
ULC_data <- data %>%
  rename(time = TIME_PERIOD) %>%
  filter(
    geo %in% c("EL", "EU27_2020", selected_countries),
    #   na_item == "D1_SAL_HW",
    na_item == "D1_SAL_HW",  # Compensation of employees per hour worked
    unit == 'EUR'#"I20"#"PPS_EU27_2020" #"I20"          # Values in euros
  ) %>%
  select(geo, time, values,unit ) %>%
  distinct()  # Remove any duplicate rows
  



# Calculate average for selected countries
Bottom_10_Avg <- ULC_data %>%
  filter(geo %in% selected_countries) %>%
  group_by(time) %>%
  summarise(values = mean(values, na.rm = TRUE)) %>%
  mutate(geo = "Bottom_10_Avg")

# Combine Greece, EU27_2020, and selected countries' average
plot_data <- bind_rows(
  ULC_data %>% filter(geo == "EL"),
  ULC_data %>% filter(geo == "EU27_2020"),
  Bottom_10_Avg
)


# Create the plot
plot_hourly_wage_rate_nama_10_lp_ulc_D1_SAL_HW <- ggplot(plot_data, aes(x = time, y = values, color = geo, linetype = geo)) +
  geom_point(size = 2, shape = 21, fill = "white", stroke = 0.5) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = seq(1995, 2024, by = 4)) +
  geom_line(size = 1) +
  scale_color_manual(
    values = colors_financial,
    labels = c(
      "EL" = "Ελλάδα",
      "EU27_2020" = "ΕΕ27 (2020)",
      "Bottom_10_Avg" = "Μέσος Όρος Bottom 10"
    ),
    drop = FALSE
  ) +
  
  scale_linetype_manual(values = c(
    "EL" = "solid",
    "Bottom_10_Avg" = "dashed",
    "EU27_2020" = "dotted"
  )) +
  labs(
    title = "Πραγματικό ωρομίσθιο σε PPS",
    subtitle = " ",
    x = "Έτος",
    y = "EUR "
  ) +
  theme_financial()

plot(plot_hourly_wage_rate_nama_10_lp_ulc_D1_SAL_HW)


################################################################################
############################## Housing indexes #################################
################################################################################

id <- "tipsho20"

# Retrieve and prepare the data
data <- get_eurostat(id, time_format = "num") %>%
  rename(time = TIME_PERIOD)


# Filter data, rename TIME_PERIOD, and select relevant columns
filtered_data <- data %>%
  filter(geo %in% list_of_countries, unit == "I15_A_AVG") %>%
  select(geo, time, values, unit)

Bottom_10_Avg <- data %>%
  filter(geo %in% selected_countries, unit == "I15_A_AVG") %>%
  group_by(time) %>%
  summarise(values = mean(values, na.rm = TRUE), .groups = "drop") %>%
  mutate(geo = "Bottom_10_Avg", unit = "I15_A_AVG")




combined_data <- bind_rows(filtered_data, Bottom_10_Avg)


# Create the enhanced plot
plot_House_Price_Index_tipsho20 <- ggplot(
  combined_data, 
  aes(x = time, y = values, color = geo, linetype = geo)
) +
  geom_line(size = 1, alpha = 0.9) +
  geom_point(size = 2, shape = 21, fill = "white", stroke = 0.5) +
  scale_color_manual(
    values = colors_financial,
    labels = c(
      "EL" = "Ελλάδα",
      "EU27_2020" = "ΕΕ27 (2020)",
      "Bottom_10_Avg" = "Μέσος Όρος Bottom 10"
    )
  ) +
  scale_linetype_manual(
    values = c(
      "EL" = "solid",
      "EU27_2020" = "solid",
      "Bottom_10_Avg" = "dashed"
    ),
    labels = c(
      "EL" = "Ελλάδα",
      "EU27_2020" = "ΕΕ27 (2020)",
      "Bottom_10_Avg" = "Μέσος Όρος Bottom 10"
    )
  ) +
  scale_y_continuous(
    breaks = seq(floor(min(combined_data$values, na.rm = TRUE)), 
                 ceiling(max(combined_data$values, na.rm = TRUE)), 
                 by = 10),
    expand = expansion(mult = c(0.05, 0.05)),
    labels = function(x) paste0(x)
  ) +
  scale_x_continuous(
    breaks = seq(min(combined_data$time, na.rm = TRUE), 
                 max(combined_data$time, na.rm = TRUE), 
                 by = 2),
    expand = c(0.05, 0.05)
  ) +
  labs(
    title = "Δείκτης Τιμών Κατοικίας",
    subtitle = "Δείκτης (2015=100), Επιλεγμένες Χώρες και ΕΕ27",
    x = "Έτος",
    y = "Δείκτης (2015=100)",
    caption = "Πηγή: Eurostat (tipsho20)"
  ) +
  theme_financial()


print(plot_House_Price_Index_tipsho20)

################# Housing cost overburden rate by poverty status ###############

id <- "sdg_01_50"


# Retrieve and prepare the data
data <- get_eurostat(id, time_format = "num") %>%
  rename(time = TIME_PERIOD)

# Filter for Greece and EU average
filtered_data <- data %>%
  filter(geo %in% list_of_countries, unit == "PC", incgrp == "A_MD60") %>%
  select(geo, time, values, unit)

# Compute average for selected countries
Bottom_10_Avg <- data %>%
  filter(geo %in% selected_countries, unit == "PC", incgrp == "A_MD60") %>%
  group_by(time) %>%
  summarise(values = mean(values, na.rm = TRUE), .groups = "drop") %>%
  mutate(geo = "Bottom_10_Avg", unit = "PC")


# Combine all data
combined_data <- bind_rows(filtered_data,Bottom_10_Avg)

# Create the enhanced plot
plot_housing_cost_overburden <- ggplot(
  combined_data, 
  aes(x = time, y = values, color = geo, linetype = geo)
) +
  geom_line(size = 1, alpha = 0.9) +
  geom_point(size = 2, shape = 21, fill = "white", stroke = 0.5) +
  scale_color_manual(
    values = colors_financial,
    labels = c(
      "EL" = "Ελλάδα",
      "EU27_2020" = "ΕΕ27 (2020)",
      "Bottom_10_Avg" = "Μέσος Όρος Bottom 10"
    )
  ) +
  scale_linetype_manual(
    values = c(
      "EL" = "solid",
      "EU27_2020" = "solid",
      "Bottom_10_Avg" = "dashed"
    ),
    labels = c(
      "EL" = "Ελλάδα",
      "EU27_2020" = "ΕΕ27 (2020)",
      "Bottom_10_Avg" = "Μέσος Όρος Bottom 10"
    )
  ) +
  scale_y_continuous(
    breaks = seq(0, ceiling(max(combined_data$values, na.rm = TRUE)), by = 2),
    expand = expansion(mult = c(0.1, 0.1)),
    labels = function(x) paste0(x, "%")
  ) +
  scale_x_continuous(
    breaks = seq(min(combined_data$time, na.rm = TRUE), 
                 max(combined_data$time, na.rm = TRUE), 
                 by = 2),
    expand = c(0.05, 0.05)
  ) +
  labs(
    title = "Ποσοστό Υπερβολικού Κόστους Στέγασης",
    subtitle = "Ποσοστό του Πληθυσμού (Εισόδημα >60% Διάμεσου), Επιλεγμένες Χώρες και ΕΕ27",
    x = "Έτος",
    y = "% Πληθυσμού",
    caption = "Πηγή: Eurostat (sdg_01_50)"
  ) +
  theme_financial()

print(plot_housing_cost_overburden)


#### Below

# Filter for Greece and EU average
filtered_data <- data %>%
  filter(geo %in% list_of_countries, unit == "PC", incgrp == "A_MD60") %>%
  select(geo, time, values, unit)

# Compute average for selected countries
Bottom_10_Avg <- data %>%
  filter(geo %in% selected_countries, unit == "PC", incgrp == "A_MD60") %>%
  group_by(time) %>%
  summarise(values = mean(values, na.rm = TRUE), .groups = "drop") %>%
  mutate(geo = "Bottom_10_Avg", unit = "PC")

# Combine all data
combined_data <- bind_rows(filtered_data, Bottom_10_Avg)

# Create the enhanced plot
plot_housing_cost_overburden <- ggplot(
  combined_data, 
  aes(x = time, y = values, color = geo, linetype = geo)
) +
  geom_line(size = 1, alpha = 0.9) +
  geom_point(size = 2, shape = 21, fill = "white", stroke = 0.5) +
  scale_color_manual(
    values = colors_financial,
    labels = c(
      "EL" = "Ελλάδα",
      "EU27_2020" = "ΕΕ27 (2020)",
      "Bottom_10_Avg" = "Μέσος Όρος Bottom 10"
    )
  ) +
  scale_linetype_manual(
    values = c(
      "EL" = "solid",
      "EU27_2020" = "solid",
      "Bottom_10_Avg" = "dashed"
    ),
    labels = c(
      "EL" = "Ελλάδα",
      "EU27_2020" = "ΕΕ27 (2020)",
      "Bottom_10_Avg" = "Μέσος Όρος Bottom 10"
    )
  ) +
  scale_y_continuous(
    breaks = seq(0, ceiling(max(combined_data$values, na.rm = TRUE)), by = 2),
    expand = expansion(mult = c(0.1, 0.1)),
    labels = function(x) paste0(x, "%")
  ) +
  scale_x_continuous(
    breaks = seq(min(combined_data$time, na.rm = TRUE), 
                 max(combined_data$time, na.rm = TRUE), 
                 by = 2),
    expand = c(0.05, 0.05)
  ) +
  labs(
    title = "Ποσοστό Υπερβολικού Κόστους Στέγασης",
    subtitle = "Ποσοστό του Πληθυσμού (Εισόδημα >60% Διάμεσου), Επιλεγμένες Χώρες και ΕΕ27",
    x = "Έτος",
    y = "% Πληθυσμού",
    caption = "Πηγή: Eurostat (sdg_01_50)"
  ) +
  theme_financial()

print(plot_housing_cost_overburden)


########### Self-reported unmet need for medical examination and care ##########
id <- "sdg_03_60"

# Retrieve and prepare the data
data <- get_eurostat(id, time_format = "num") %>%
  rename(time = TIME_PERIOD)

# Filter data for Greece and EU
filtered_data <- data %>%
  filter(geo %in% list_of_countries, unit == "PC", sex == "T") %>%
  select(geo, time, values, sex, unit)

# Compute average for selected countries
Bottom_10_Avg <- data %>%
  filter(geo %in% selected_countries, unit == "PC", sex == "T") %>%
  group_by(time, sex) %>%
  summarise(values = mean(values, na.rm = TRUE), .groups = "drop") %>%
  mutate(geo = "Bottom_10_Avg", unit = "PC")

# Combine all data
combined_data <- bind_rows(filtered_data, Bottom_10_Avg)

# Create the enhanced plot
plot_self_reported_unmet_medical_need <- ggplot(
  combined_data, 
  aes(x = time, y = values, color = geo)
) +
  geom_line(size = 1, alpha = 0.9) +
  geom_point(size = 2, shape = 21, fill = "white", stroke = 0.5) +
  scale_color_manual(
    values = colors_financial,
    labels = c(
      "EL" = "Ελλάδα",
      "EU27_2020" = "ΕΕ27 (2020)",
      "Bottom_10_Avg" = "Μέσος Όρος Bottom 10"
    )
  ) +
  scale_y_continuous(
    breaks = seq(0, ceiling(max(combined_data$values, na.rm = TRUE)), by = 1),
    expand = expansion(mult = c(0.1, 0.1)),
    labels = function(x) paste0(x, "%")
  ) +
  scale_x_continuous(
    breaks = seq(min(combined_data$time, na.rm = TRUE), 
                 max(combined_data$time, na.rm = TRUE), 
                 by = 2),
    expand = c(0.05, 0.05)
  ) +
  labs(
    title = "Αυτοαναφερόμενη ανεκπλήρωτη ανάγκη για ιατρική εξέταση",
    subtitle = "Ποσοστό του Πληθυσμού, Επιλεγμένες Χώρες και ΕΕ27",
    x = "Έτος",
    y = "% Πληθυσμού",
    caption = "Πηγή: Eurostat (sdg_03_60)"
  ) +
  theme_financial()

print(plot_self_reported_unmet_medical_need)

# population reporting occurrence of crime, violence or vandalism in their area 

id <- "sdg_16_20"

# Retrieve and prepare data
data <- get_eurostat(id, time_format = "num") %>%
  rename(time = TIME_PERIOD)

# Filter for EL and EU27
filtered_data <- data %>%
  filter(geo %in% list_of_countries, unit == "PC",incgrp == "TOTAL") %>%
  select(geo, time, values, incgrp, unit)

# Compute average for selected countries
Bottom_10_Avg <- data %>%
  filter(geo %in% selected_countries, unit == "PC", incgrp == "TOTAL") %>%
  group_by(time, incgrp) %>%
  summarise(values = mean(values, na.rm = TRUE), .groups = "drop") %>%
  mutate(geo = "Bottom_10_Avg", unit = "PC")

# Combine all into one dataframe
combined_data <- bind_rows(filtered_data, Bottom_10_Avg)

# Create the enhanced plot
plot_occurence_of_crime_violence_vandalism <- ggplot(
  combined_data, 
  aes(x = time, y = values, color = geo)
) +
  geom_line(size = 1, alpha = 0.9) +
  geom_point(size = 2, shape = 21, fill = "white", stroke = 0.5) +
  scale_color_manual(
    values = colors_financial,
    labels = c(
      "EL" = "Ελλάδα",
      "EU27_2020" = "ΕΕ27 (2020)",
      "Bottom_10_Avg" = "Μέσος Όρος Επιλεγμένων"
    )
  ) +
  scale_y_continuous(
    breaks = seq(0, ceiling(max(combined_data$values, na.rm = TRUE)), by = 2),
    expand = expansion(mult = c(0.1, 0.1)),
    labels = function(x) paste0(x, "%")
  ) +
  scale_x_continuous(
    breaks = seq(min(combined_data$time, na.rm = TRUE), 
                 max(combined_data$time, na.rm = TRUE), 
                 by = 2),
    expand = c(0.05, 0.05)
  ) +
  labs(
    title = "Αναφορά Εγκλημάτων, Βίας ή Βανδαλισμών",
    subtitle = "Ποσοστό Πληθυσμού, Επιλεγμένες Χώρες και ΕΕ27",
    x = "Έτος",
    y = "% Πληθυσμού",
    caption = "Πηγή: Eurostat (sdg_16_20)"
  ) +
  theme_financial() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    legend.position = "bottom",
    legend.box = "vertical",
    legend.text = element_text(size = 9),
    legend.title = element_blank(),
    legend.key.width = unit(1.5, "cm")
  ) +
  guides(
    color = guide_legend(nrow = 2, byrow = TRUE),
    linetype = guide_legend(nrow = 2, byrow = TRUE)
  )

print(plot_occurence_of_crime_violence_vandalism)

############### Severely materially and socially deprived people ###############


id <- "tipslc30"

# Retrieve and prepare the data
data <- get_eurostat(id, time_format = "num") %>%
  rename(time = TIME_PERIOD)

# Filter data, rename TIME_PERIOD, and select relevant columns
filtered_data <- data %>%
  #  rename(time = TIME_PERIOD) %>%
  filter(geo %in% list_of_countries, unit == "PC") %>%
  select(geo, time, values, unit)

Bottom_10_Avg <- data %>%
  filter(geo %in% selected_countries, unit == "PC") %>%
  group_by(time) %>%
  summarise(values = mean(values, na.rm = TRUE), .groups = "drop") %>%
  mutate(geo = "Bottom_10_Avg", unit = "PC")


combined_data <- bind_rows(filtered_data, Bottom_10_Avg)


# Create the enhanced plot
plot_severely_materially_deprived_tipslc30 <- ggplot(
  combined_data, 
  aes(x = time, y = values, color = geo, linetype = geo)
) +
  geom_line(size = 1, alpha = 0.9) +
  geom_point(size = 2, shape = 21, fill = "white", stroke = 0.5) +
  scale_color_manual(
    values = colors_financial,
    labels = c(
      "EL" = "Ελλάδα",
      "EU27_2020" = "ΕΕ27 (2020)",
      "Bottom_10_Avg" = "Μέσος Όρος Bottom 10"
    )
  ) +
  scale_linetype_manual(
    values = c(
      "EL" = "solid",
      "EU27_2020" = "solid",
      "Bottom_10_Avg" = "dashed"
    ),
    labels = c(
      "EL" = "Ελλάδα",
      "EU27_2020" = "ΕΕ27 (2020)",
      "Bottom_10_Avg" = "Μέσος Όρος Bottom 10"
    )
  ) +
  scale_y_continuous(
    breaks = seq(0, ceiling(max(combined_data$values, na.rm = TRUE)), by = 5),
    expand = expansion(mult = c(0.1, 0.1)),
    labels = function(x) paste0(x, "%")
  ) +
  scale_x_continuous(
    breaks = seq(min(combined_data$time, na.rm = TRUE), 
                 max(combined_data$time, na.rm = TRUE), 
                 by = 2),
    expand = c(0.05, 0.05)
  ) +
  labs(
    title = "Άτομα με σοβαρές υλικές και κοινωνικές στερήσεις",
    subtitle = "Ποσοστό του Πληθυσμού, Επιλεγμένες Χώρες και ΕΕ27",
    x = "Έτος",
    y = "% Πληθυσμού",
    caption = "Πηγή: Eurostat (tipslc30)"
  ) +
  theme_financial()

print(plot_severely_materially_deprived_tipslc30)

################################# Price indexes ################################

################################################################################

################################################################################
############################# Income inequality ################################
################################################################################


colors_financial <- c(
  "2015" = "#4A4A4A", 
  "2019" = "#901628",
  "2023" = "#1B3C69",
  "2015-2019" = "#901628",
  "2019-2023" = "#1B3C69"
)

# Define official CPIs
official_cpis <- list(
  "2015" = 100.00,
  "2019" = 102.46,
  "2023" = 115.84
)

# Function to load and prepare SILC data
load_silc_data <- function(household_file, year, official_cpi) {
  cat("Loading SILC data for year", year, "from", household_file, "\n")
  
  if (grepl("\\.xlsb$", household_file)) {
    tryCatch({
      workbook <- wb_load(household_file)
      household <- wb_to_df(workbook, sheet = "H_FILE")
      person <- wb_to_df(workbook, sheet = "P_FILE")
    }, error = function(e) {
      stop("Error reading .xlsb file ", household_file, ": ", e$message)
    })
  } else {
    tryCatch({
      household <- read_excel(household_file, sheet = "H_FILE")
      person <- read_excel(household_file, sheet = "P_FILE")
    }, error = function(e) {
      sheets <- try(excel_sheets(household_file), silent = TRUE)
      if (inherits(sheets, "try-error")) {
        stop("Error reading ", household_file, ": ", e$message)
      } else {
        stop("Error reading sheets from ", household_file, ": ", e$message, 
             "\nAvailable sheets: ", paste(sheets, collapse = ", "))
      }
    })
  }
  
  names(household) <- make.names(names(household), unique = TRUE)
  names(person) <- make.names(names(person), unique = TRUE)
  
  required_cols <- c("HY020", "HB030", "PB030", "PB040")
  if (!all(required_cols %in% c(names(household), names(person)))) {
    stop("Missing required columns in SILC data: ", 
         paste(setdiff(required_cols, c(names(household), names(person))), collapse = ", "))
  }
  
  person <- person %>% mutate(HB030 = substr(as.character(PB030), 1, 6))
  household <- household %>% mutate(HB030 = as.character(HB030))
  
  data <- person %>% left_join(household, by = "HB030")
  
  winsorize_99 <- function(x) {
    q99 <- quantile(x, 0.99, na.rm = TRUE)
    ifelse(x > q99, q99, x)
  }
  
  data <- data %>%
    mutate(
      HY020_nominal_w = winsorize_99(HY020),
      HY020_real_official = HY020_nominal_w / (official_cpi / 100)
    )
  
  cat("SILC data loaded for year", year, ": ", nrow(data), "rows\n")
  return(data)
}

# Function to load and prepare HBS data
load_hbs_data <- function(file_path, year) {
  cat("Loading HBS data for year", year, "from", file_path, "\n")
  data <- read.csv2(file_path)
  
  expenditure_vars <- c("HE01", "HE02", "HE03", "HE04", "HE05", "HE06", 
                        "HE07", "HE08", "HE09", "HE10", "HE11", "HE12")
  key_vars <- c(expenditure_vars, "HA10", "HH095")
  
  missing_vars <- setdiff(key_vars, names(data))
  if (length(missing_vars) > 0) {
    stop("Missing columns in HBS data: ", paste(missing_vars, collapse = ", "))
  }
  
  for (var in key_vars) {
    if (!is.numeric(data[[var]])) {
      data[[var]] <- as.numeric(as.character(data[[var]]))
    }
  }
  
  data$sum_HE <- rowSums(data[, expenditure_vars], na.rm = TRUE)
  
  cat("HBS data loaded for year", year, ": ", nrow(data), "rows\n")
  return(data)
}

# Function to create survey design
create_survey_design <- function(data, weight_var = "PB040") {
  if (weight_var == "PB040") {
    design <- svydesign(ids = ~1, weights = ~PB040, data = data)
  } else {
    design <- svydesign(ids = ~1, weights = ~HA10, data = data)
  }
  return(design)
}

# Function to compute deciles
compute_deciles <- function(design, income_var = "HY020_nominal_w") {
  deciles <- svyquantile(as.formula(paste("~", income_var)), design, 
                         quantiles = seq(0.1, 0.9, 0.1), na.rm = TRUE)
  return(as.numeric(coef(deciles)))
}

# Function to compute consumption baskets by decile
compute_consumption_baskets <- function(design, deciles, expenditure_vars) {
  ratios <- list()
  for (d in 1:9) {
    if (d == 1) {
      subset_design <- subset(design, HH095 <= deciles[1])
    } else {
      subset_design <- subset(design, HH095 > deciles[d-1] & HH095 <= deciles[d])
    }
    decile_ratios <- numeric(length(expenditure_vars))
    for (i in seq_along(expenditure_vars)) {
      ratio <- svyratio(as.formula(paste("~", expenditure_vars[i])), ~sum_HE, 
                        design = subset_design)
      decile_ratios[i] <- coef(ratio)
    }
    ratios[[paste0("D", d)]] <- decile_ratios
  }
  subset_design <- subset(design, HH095 > deciles[9])
  decile_ratios <- numeric(length(expenditure_vars))
  for (i in seq_along(expenditure_vars)) {
    ratio <- svyratio(as.formula(paste("~", expenditure_vars[i])), ~sum_HE, 
                      design = subset_design)
    decile_ratios[i] <- coef(ratio)
  }
  ratios[["D10"]] <- decile_ratios
  return(ratios)
}

# Function to load Eurostat price index data
load_price_indices <- function(years) {
  cat("Loading Eurostat price indices for years:", paste(years, collapse = ", "), "\n")
  id <- "prc_hicp_aind"
  data_prices <- tryCatch({
    get_eurostat(id, time_format = "num", cache = FALSE)
  }, error = function(e) {
    stop("Error fetching Eurostat data: ", e$message)
  })
  
  time_col <- if ("TIME_PERIOD" %in% names(data_prices)) "TIME_PERIOD" else "time"
  data_prices[[time_col]] <- as.numeric(as.character(data_prices[[time_col]]))
  
  data_prices_GR <- data_prices[data_prices$geo == "EL" & data_prices[[time_col]] %in% years, ]
  data_prices_GR_index <- data_prices_GR[data_prices_GR$unit == "INX_A_AVG", ]
  
  coicop_values <- c("CP01", "CP02", "CP03", "CP04", "CP05", "CP06", 
                     "CP07", "CP08", "CP09", "CP10", "CP11", "CP12")
  filtered_data <- data_prices_GR_index[data_prices_GR_index$coicop %in% coicop_values, ]
  
  missing_coicop <- setdiff(coicop_values, filtered_data$coicop)
  if (length(missing_coicop) > 0) {
    warning("Missing COICOP codes: ", paste(missing_coicop, collapse = ", "))
  }
  
  cat("Eurostat data loaded:", nrow(filtered_data), "rows\n")
  return(filtered_data)
}

# Function to compute decile-specific price indexes
compute_price_indexes <- function(baskets, price_data, years) {
  price_indexes <- list()
  time_col <- if ("TIME_PERIOD" %in% names(price_data)) "TIME_PERIOD" else "time"
  
  for (year in years) {
    price_year <- price_data[price_data[[time_col]] == year, ]
    if (nrow(price_year) == 0) {
      warning("No price data for year ", year)
      year_indexes <- rep(NA, 10)
    } else {
      year_indexes <- numeric(10)
      for (d in 1:10) {
        year_indexes[d] <- sum(baskets[[paste0("D", d)]] * price_year$values, na.rm = TRUE)
      }
    }
    price_indexes[[as.character(year)]] <- year_indexes
  }
  return(price_indexes)
}

# Function to compute inflation rates
compute_inflation_rates <- function(price_indexes, years) {
  inflation_data <- list()
  for (i in 2:length(years)) {
    inflation <- (price_indexes[[as.character(years[i])]] / 
                    price_indexes[[as.character(years[i-1])]] - 1) * 100
    inflation_data[[paste(years[i-1], years[i], sep = "-")]] <- data.frame(
      Decile = paste0("D", 1:10),
      Inflation = inflation,
      Period = paste(years[i-1], years[i], sep = "-")
    )
  }
  return(do.call(rbind, inflation_data))
}

# Function to deflate SILC incomes using decile-specific price indexes
deflate_incomes <- function(silc_data, deciles, price_indexes, year) {
  silc_data <- silc_data %>%
    mutate(
      Decile = cut(HY020_nominal_w, 
                   breaks = c(-Inf, deciles, Inf), 
                   labels = paste0("D", 1:10), 
                   include.lowest = TRUE),
      Price_Index = price_indexes[[as.character(year)]][as.numeric(Decile)],
      HY020_real_decile = HY020_nominal_w / (Price_Index / 100)
    )
  return(silc_data)
}

# Function to load and process all data
load_and_process_data <- function() {
  # Load SILC data with official CPIs
  silc_data15 <- load_silc_data("SILC_2015_Public Use Files_2.xlsx", 2015, official_cpis[["2015"]])
  silc_data19 <- load_silc_data("SILC_2019_Public Use Files.xlsb", 2019, official_cpis[["2019"]])
  silc_data23 <- load_silc_data("SILC_2023_Public Use Files.xlsx", 2023, official_cpis[["2023"]])
  
  # Create survey designs for SILC
  silc_design15 <- create_survey_design(silc_data15, "PB040")
  silc_design19 <- create_survey_design(silc_data19, "PB040")
  silc_design23 <- create_survey_design(silc_data23, "PB040")
  
  # Compute deciles
  deciles_15 <- compute_deciles(silc_design15, "HY020_nominal_w")
  deciles_19 <- compute_deciles(silc_design19, "HY020_nominal_w")
  deciles_23 <- compute_deciles(silc_design23, "HY020_nominal_w")
  
  # Load HBS data
  hbs_data19 <- load_hbs_data("hbs_hh_basic_2019_gr.csv", 2019)
  hbs_data23 <- load_hbs_data("hbs_hh_basic_2023_gr.csv", 2023)
  
  # Create survey designs for HBS
  hbs_design19 <- create_survey_design(hbs_data19, "HA10")
  hbs_design23 <- create_survey_design(hbs_data23, "HA10")
  
  expenditure_vars <- c("HE01", "HE02", "HE03", "HE04", "HE05", "HE06", 
                        "HE07", "HE08", "HE09", "HE10", "HE11", "HE12")
  
  # Compute consumption baskets
  baskets_19 <- compute_consumption_baskets(hbs_design19, deciles_19, expenditure_vars)
  baskets_23 <- compute_consumption_baskets(hbs_design23, deciles_23, expenditure_vars)
  
  # Load Eurostat price indices
  price_data <- load_price_indices(c(2015, 2019, 2023))
  
  # Compute price indexes
  price_indexes_15 <- compute_price_indexes(baskets_19, price_data, c(2015))
  price_indexes_19 <- compute_price_indexes(baskets_19, price_data, c(2019))
  price_indexes_23 <- compute_price_indexes(baskets_23, price_data, c(2023))
  price_indexes <- c(price_indexes_15, price_indexes_19, price_indexes_23)
  
  # Compute inflation rates
  inflation_data <- compute_inflation_rates(price_indexes, c(2015, 2019, 2023))
  
  # Deflate incomes with decile-specific CPIs
  silc_data15 <- deflate_incomes(silc_data15, deciles_15, price_indexes, 2015)
  silc_data19 <- deflate_incomes(silc_data19, deciles_19, price_indexes, 2019)
  silc_data23 <- deflate_incomes(silc_data23, deciles_23, price_indexes, 2023)
  
  # Recreate survey designs with updated data
  silc_design15 <- create_survey_design(silc_data15, "PB040")
  silc_design19 <- create_survey_design(silc_data19, "PB040")
  silc_design23 <- create_survey_design(silc_data23, "PB040")
  
  # Compute mean incomes by decile
  mean_nominal_15 <- svyby(~HY020_nominal_w, ~Decile, silc_design15, svymean, na.rm = TRUE)
  mean_nominal_19 <- svyby(~HY020_nominal_w, ~Decile, silc_design19, svymean, na.rm = TRUE)
  mean_nominal_23 <- svyby(~HY020_nominal_w, ~Decile, silc_design23, svymean, na.rm = TRUE)
  
  mean_real_decile_15 <- svyby(~HY020_real_decile, ~Decile, silc_design15, svymean, na.rm = TRUE)
  mean_real_decile_19 <- svyby(~HY020_real_decile, ~Decile, silc_design19, svymean, na.rm = TRUE)
  mean_real_decile_23 <- svyby(~HY020_real_decile, ~Decile, silc_design23, svymean, na.rm = TRUE)
  
  mean_real_official_15 <- svyby(~HY020_real_official, ~Decile, silc_design15, svymean, na.rm = TRUE)
  mean_real_official_19 <- svyby(~HY020_real_official, ~Decile, silc_design19, svymean, na.rm = TRUE)
  mean_real_official_23 <- svyby(~HY020_real_official, ~Decile, silc_design23, svymean, na.rm = TRUE)
  
  # Combine mean incomes and price indices
  mean_income_data <- data.frame(
    Decile = paste0("D", 1:10),
    Nominal_Mean_2015 = mean_nominal_15$HY020_nominal_w,
    Nominal_Mean_2019 = mean_nominal_19$HY020_nominal_w,
    Nominal_Mean_2023 = mean_nominal_23$HY020_nominal_w,
    Real_Mean_2015_Decile_CPI = mean_real_decile_15$HY020_real_decile,
    Real_Mean_2019_Decile_CPI = mean_real_decile_19$HY020_real_decile,
    Real_Mean_2023_Decile_CPI = mean_real_decile_23$HY020_real_decile,
    Real_Mean_2015_Official_CPI = mean_real_official_15$HY020_real_official,
    Real_Mean_2019_Official_CPI = mean_real_official_19$HY020_real_official,
    Real_Mean_2023_Official_CPI = mean_real_official_23$HY020_real_official,
    Price_Index_2015_Decile_CPI = price_indexes[["2015"]],
    Price_Index_2019_Decile_CPI = price_indexes[["2019"]],
    Price_Index_2023_Decile_CPI = price_indexes[["2023"]],
    Price_Index_2015_Official_CPI = official_cpis[["2015"]],
    Price_Index_2019_Official_CPI = official_cpis[["2019"]],
    Price_Index_2023_Official_CPI = official_cpis[["2023"]]
  )
  
  # Calculate growth rates
  mean_income_data <- mean_income_data %>%
    mutate(
      Nominal_Growth_2015_2019 = ((Nominal_Mean_2019 / Nominal_Mean_2015) - 1) * 100,
      Nominal_Growth_2019_2023 = ((Nominal_Mean_2023 / Nominal_Mean_2019) - 1) * 100,
      Real_Growth_2015_2019_Decile_CPI = ((Real_Mean_2019_Decile_CPI / Real_Mean_2015_Decile_CPI) - 1) * 100,
      Real_Growth_2019_2023_Decile_CPI = ((Real_Mean_2023_Decile_CPI / Real_Mean_2019_Decile_CPI) - 1) * 100,
      Real_Growth_2015_2019_Official_CPI = ((Real_Mean_2019_Official_CPI / Real_Mean_2015_Official_CPI) - 1) * 100,
      Real_Growth_2019_2023_Official_CPI = ((Real_Mean_2023_Official_CPI / Real_Mean_2019_Official_CPI) - 1) * 100
    )
  
  # Save mean income and growth rates
  write.csv(mean_income_data, "decile_income_growth_rates.csv", row.names = FALSE)
  
  # Save modified SILC data
  write.csv(silc_data15, "silc_2015_real_incomes.csv", row.names = FALSE)
  write.csv(silc_data19, "silc_2019_real_incomes.csv", row.names = FALSE)
  write.csv(silc_data23, "silc_2023_real_incomes.csv", row.names = FALSE)
  
  return(list(
    silc_data15 = silc_data15,
    silc_data19 = silc_data19,
    silc_data23 = silc_data23,
    price_indexes = price_indexes,
    inflation_data = inflation_data,
    deciles_15 = deciles_15,
    deciles_19 = deciles_19,
    deciles_23 = deciles_23,
    mean_income_data = mean_income_data,
    silc_design15 = silc_design15,
    silc_design19 = silc_design19,
    silc_design23 = silc_design23
  ))
}

# Function to generate plots
generate_plots <- function(processed_data) {
  silc_data15 <- processed_data$silc_data15
  silc_data19 <- processed_data$silc_data19
  silc_data23 <- processed_data$silc_data23
  price_indexes <- processed_data$price_indexes
  inflation_data <- processed_data$inflation_data
  mean_income_data <- processed_data$mean_income_data
  silc_design15 <- processed_data$silc_design15
  silc_design19 <- processed_data$silc_design19
  silc_design23 <- processed_data$silc_design23
  
  # Prepare data for price index plot
  price_data_plot <- data.frame(
    Decile = rep(paste0("D", 1:10), 3),
    Year = rep(c("2015", "2019", "2023"), each = 10),
    PriceIndex = c(price_indexes[["2015"]], price_indexes[["2019"]], price_indexes[["2023"]])
  )
  
  # Prepare data for real income distributions
  real_income_decile <- rbind(
    data.frame(Real_Income = silc_data15$HY020_real_decile, Year = "2015", Weight = silc_data15$PB040),
    data.frame(Real_Income = silc_data19$HY020_real_decile, Year = "2019", Weight = silc_data19$PB040),
    data.frame(Real_Income = silc_data23$HY020_real_decile, Year = "2023", Weight = silc_data23$PB040)
  )
  real_income_decile <- real_income_decile[!is.na(real_income_decile$Real_Income) & !is.na(real_income_decile$Weight), ]
  
  real_income_official <- rbind(
    data.frame(Real_Income = silc_data15$HY020_real_official, Year = "2015", Weight = silc_data15$PB040),
    data.frame(Real_Income = silc_data19$HY020_real_official, Year = "2019", Weight = silc_data19$PB040),
    data.frame(Real_Income = silc_data23$HY020_real_official, Year = "2023", Weight = silc_data23$PB040)
  )
  real_income_official <- real_income_official[!is.na(real_income_official$Real_Income) & !is.na(real_income_official$Weight), ]
  
  # Compute weighted medians for each year
  median_decile_15 <- svyquantile(~HY020_real_decile, silc_design15, quantiles = 0.5, na.rm = TRUE)
  median_decile_19 <- svyquantile(~HY020_real_decile, silc_design19, quantiles = 0.5, na.rm = TRUE)
  median_decile_23 <- svyquantile(~HY020_real_decile, silc_design23, quantiles = 0.5, na.rm = TRUE)
  
  median_official_15 <- svyquantile(~HY020_real_official, silc_design15, quantiles = 0.5, na.rm = TRUE)
  median_official_19 <- svyquantile(~HY020_real_official, silc_design19, quantiles = 0.5, na.rm = TRUE)
  median_official_23 <- svyquantile(~HY020_real_official, silc_design23, quantiles = 0.5, na.rm = TRUE)
  
  medians_decile <- data.frame(
    Year = c("2015", "2019", "2023"),
    Median = c(coef(median_decile_15), coef(median_decile_19), coef(median_decile_23))
  )
  
  medians_official <- data.frame(
    Year = c("2015", "2019", "2023"),
    Median = c(coef(median_official_15), coef(median_official_19), coef(median_official_23))
  )
  
  # Prepare data for growth rates
  growth_decile_cpi <- mean_income_data %>%
    select(Decile, Real_Growth_2015_2019_Decile_CPI, Real_Growth_2019_2023_Decile_CPI) %>%
    pivot_longer(cols = starts_with("Real_Growth"), names_to = "Period", values_to = "Growth") %>%
    mutate(Period = recode(Period, 
                           "Real_Growth_2015_2019_Decile_CPI" = "2015-2019", 
                           "Real_Growth_2019_2023_Decile_CPI" = "2019-2023"))
  
  growth_official_cpi <- mean_income_data %>%
    select(Decile, Real_Growth_2015_2019_Official_CPI, Real_Growth_2019_2023_Official_CPI) %>%
    pivot_longer(cols = starts_with("Real_Growth"), names_to = "Period", values_to = "Growth") %>%
    mutate(Period = recode(Period, 
                           "Real_Growth_2015_2019_Official_CPI" = "2015-2019", 
                           "Real_Growth_2019_2023_Official_CPI" = "2019-2023"))
  
  
  
  # Create plots with Greek translations
  price_plot <- ggplot(price_data_plot, aes(x = Decile, y = PriceIndex, fill = Year)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = "Δείκτης Τιμών ανά Δεκατημόριο (2015, 2019, 2023)",
         y = "Δείκτης Τιμών",
         x = "Δεκατημόριο Εισοδήματος",
         fill = "Έτος") +
    scale_fill_manual(values = colors_financial[c("2015", "2019", "2023")], 
                      labels = c("2015", "2019", "2023")) +
    theme_financial()
  
  inflation_plot <- ggplot(inflation_data, aes(x = as.numeric(gsub("D", "", Decile)), y = Inflation, color = Period)) +
    geom_point(size = 4) +
    labs(title = "Ποσοστά Πληθωρισμού ανά Δεκατημόριο (2015-2019, 2019-2023)",
         y = "Ποσοστό Πληθωρισμού (%)",
         x = "Δεκατημόριο Εισοδήματος",
         color = "Περίοδος") +
    scale_color_manual(values = colors_financial[c("2015-2019", "2019-2023")], 
                       labels = c("2015-2019", "2019-2023")) +
    theme_financial() +
    scale_x_continuous(breaks = 1:10)
  
  income_decile_plot <- ggplot(real_income_decile, aes(x = Real_Income, weight = Weight, fill = Year, color = Year)) +
    geom_density(alpha = 0.4, size = 0.8, adjust = 1) +
    geom_vline(data = medians_decile, aes(xintercept = Median, color = Year), 
               linetype = "dashed", size = 0.6, alpha = 0.8) +
    labs(title = "Κατανομή Πραγματικού Εισοδήματος με Δεκατημοριακό ΔΤΚ (2015, 2019, 2023)",
         x = "Πραγματικό Εισόδημα (Αποπληθωρισμένο με Δεκατημοριακό ΔΤΚ)",
         y = "Πυκνότητα",
         fill = "Έτος",
         color = "Έτος",
         caption = "Κατανομή με βάση σταθμισμένα δεδομένα. Οι διακεκομμένες γραμμές δείχνουν τη διάμεσο.") +
    scale_fill_manual(values = colors_financial[c("2015", "2019", "2023")], 
                      labels = c("2015", "2019", "2023")) +
    scale_color_manual(values = colors_financial[c("2015", "2019", "2023")], 
                       labels = c("2015", "2019", "2023")) +
    theme_financial() +
    # coord_cartesian(xlim = c(0, quantile(real_income_decile$Real_Income + 10000, 1, na.rm = TRUE)))
    coord_cartesian(xlim = c(0, max(real_income_decile$Real_Income, na.rm = TRUE) * 1.05))
  income_official_plot <- ggplot(real_income_official, aes(x = Real_Income, weight = Weight, fill = Year, color = Year)) +
    geom_density(alpha = 0.4, size = 0.8, adjust = 1) +
    geom_vline(data = medians_official, aes(xintercept = Median, color = Year), 
               linetype = "dashed", size = 0.6, alpha = 0.8) +
    labs(title = "Κατανομή Πραγματικού Εισοδήματος με Επίσημο ΔΤΚ (2015, 2019, 2023)",
         x = "Πραγματικό Εισόδημα (Αποπληθωρισμένο με Επίσημο ΔΤΚ)",
         y = "Πυκνότητα",
         fill = "Έτος",
         color = "Έτος",
         caption = "Κατανομή με βάση σταθμισμένα δεδομένα. Οι διακεκομμένες γραμμές δείχνουν τη διάμεσο.") +
    scale_fill_manual(values = colors_financial[c("2015", "2019", "2023")], 
                      labels = c("2015", "2019", "2023")) +
    scale_color_manual(values = colors_financial[c("2015", "2019", "2023")], 
                       labels = c("2015", "2019", "2023")) +
    theme_financial() +
    coord_cartesian(xlim = c(0, quantile(real_income_official$Real_Income, 0.99, na.rm = TRUE)))
  
  growth_decile_plot <- ggplot(growth_decile_cpi, aes(x = as.numeric(gsub("D", "", Decile)), y = Growth, color = Period, group = Period)) +
    geom_line(size = 1) +
    geom_point(size = 3) +
    labs(title = "Ρυθμός Μεταβολής Μέσου Πραγματικού Εισοδήματος",
         # labs(title = "Ρυθμός Μεταβολής Μέσου Πραγματικού Εισοδήματος με Δεκατημοριακό ΔΤΚ",
         y = "Ρυθμός Μεταβολής (%)",
         x = "Δεκατημόριο Εισοδήματος",
         color = "Περίοδος") +
    scale_color_manual(values = colors_financial[c("2015-2019", "2019-2023")], 
                       labels = c("2015-2019", "2019-2023")) +
    theme_financial() +
    scale_x_continuous(breaks = 1:10)
  
  growth_official_plot <- ggplot(growth_official_cpi, aes(x = as.numeric(gsub("D", "", Decile)), y = Growth, color = Period, group = Period)) +
    geom_line(size = 1) +
    geom_point(size = 3) +
    labs(title = "Ρυθμός Μεταβολής Μέσου Πραγματικού Εισοδήματος με Επίσημο ΔΤΚ",
         y = "Ρυθμός Μεταβολής (%)",
         x = "Δεκατημόριο Εισοδήματος",
         color = "Περίοδος") +
    scale_color_manual(values = colors_financial[c("2015-2019", "2019-2023")], 
                       labels = c("2015-2019", "2019-2023")) +
    theme_financial() +
    scale_x_continuous(breaks = 1:10)
  
  # Save plots
  ggsave("price_index_evolution.png", price_plot, width = 8, height = 4)
  ggsave("inflation_evolution.png", inflation_plot, width = 8, height = 4)
  ggsave("real_income_density_decile_cpi.png", income_decile_plot, width = 8, height = 4)
  ggsave("real_income_density_official_cpi.png", income_official_plot, width = 8, height = 4)
  ggsave("growth_rates_decile_cpi.png", growth_decile_plot, width = 8, height = 4)
  ggsave("growth_rates_official_cpi.png", growth_official_plot, width = 8, height = 4)
  
  # Print plots
  print(price_plot)
  print(inflation_plot)
  print(income_decile_plot)
  print(income_official_plot)
  print(growth_decile_plot)
  print(growth_official_plot)
  
  return(list(
    price_plot = price_plot,
    inflation_plot = inflation_plot,
    income_decile_plot = income_decile_plot,
    income_official_plot = income_official_plot,
    growth_decile_plot = growth_decile_plot,
    growth_official_plot = growth_official_plot
  ))
}

# Run data processing
processed_data <- load_and_process_data()

# Generate plots
plots <- generate_plots(processed_data)

# Indicate completion
cat("Analysis complete. Data, results, and plots saved.\n")

# Author: Achilleas Mantes

# Created for: https://greekonomics.gr

# Data Source: Eurostat (https://ec.europa.eu/eurostat)

# License: MIT License



# Load necessary libraries
library(tidyverse)
library(ggplot2)
library(eurostat)
library(dplyr)
library(showtext)
library(tidyr)
library(scales)



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
  "EL" = "#1B3C69",           # Deep navy for Greece
  "EU27_2020" = "#4A4A4A",    # Dark grey for EU27
  "Bottom_10_Avg" = "#901628"  # Muted red for selected countries (Bottom 10)
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
################# Real gross disposable income per capita  #####################
################################################################################

# Real gross disposable income of households per capita
id <- "tepsr_wc310"


# Retrieve
data <- get_eurostat(id, time_format = "num") %>%
  rename(time = TIME_PERIOD) # rename time

# Filter data and select relevant columns
filtered_data <- data %>%
  filter(geo %in% list_of_countries, unit == "CP_MNAC") %>%
  select(geo, time, values, unit)

avg_data <- data %>%
  filter(geo %in% selected_countries, unit == "CP_MNAC") %>%
  group_by(time) %>%
  summarise(values = mean(values, na.rm = TRUE), .groups = "drop") %>%
  mutate(geo = "Bottom_10_Avg", unit = "CP_MNAC")

combined_data <- bind_rows(filtered_data, avg_data)

# Create the plot
plot_real_gross_YD_per_capita_tepsr_wc310 <- ggplot(
  combined_data, 
  aes(x = time, y = values, color = geo, linetype = geo)
) +
  geom_line(size = 1, alpha = 0.9) +
  geom_point(size = 2, shape = 21, fill = "white", stroke = 0.5) +
  scale_color_manual(
    values = colors_financial,
    labels = c(
      "EL" = "Greece",
      "EU27_2020" = "EU27 (2020)",
      "Bottom_10_Avg" = "Bottom 10 Avg"
    )
  ) +
  scale_linetype_manual(
    values = c(
      "EL" = "solid",
      "EU27_2020" = "solid",
      "Bottom_10_Avg" = "dashed"
    ),
    labels = c(
      "EL" = "Greece",
      "EU27_2020" = "EU27 (2020)",
      "Bottom_10_Avg" = "Bottom 10 Avg"
    )
  ) +
  scale_y_continuous(
    breaks = seq(0, max(combined_data$values, na.rm = TRUE), by = 10),
    expand = c(0.05, 0.05)
  ) +
  scale_x_continuous(
    breaks = seq(min(combined_data$time, na.rm = TRUE), max(combined_data$time, na.rm = TRUE), by = 2),
    expand = c(0.05, 0.5)
  ) +
  labs(
    title = "Πραγματικό κατά κεφαλήν διαθέσιμιο εισόδημα",
    subtitle = "Δείκτης (2008 = 100), Επιλεγμένες χώρες και EU27",
    x = "Έτος",
    y = "Δείκτης (2008 = 100)",
    caption = "Source: Eurostat (tepsr_wc310)"
  ) +
  theme_greekonomics()

#Display the plot
print(plot_real_gross_YD_per_capita_tepsr_wc310)

################################################################################
########################## Real GDP per capita   ###############################
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


# Select Greece and EU27
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

# Plot
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
      "Bottom_10_Avg" = "Μέσος Όρος Bottom 10"
    ),
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
    title = "Kατά κεφαλήν ΑΕΠ προσαρμοσμένο σε PPS",
    subtitle = "Ισοτιμία Αγοραστικής Δύναμης (έτος βάσης 2020)",
    x = "Έτος",
    y = "PPS",
    caption = "Source: Eurostat (sdg_10_10)"
  ) +
  theme_greekonomics() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5)
  )

# Display the plot
print(plot_gdp_per_capita)

################################################################################
############################# Real GDP per capita ##############################
################################################################################

id <- "tipsna40"

# Retrieve
data <- get_eurostat(id, time_format = "num") %>%
  rename(time = TIME_PERIOD)

# Filter data, rename TIME_PERIOD, and select relevant columns
filtered_data <- data %>%
  filter(geo %in% list_of_countries, unit == "CLV15_EUR_HAB") %>%
  select(geo, time, values, unit)

bottom_10_data <- data %>%
  filter(geo %in% selected_countries, unit == "CLV15_EUR_HAB") %>%
  group_by(time) %>%
  summarise(values = mean(values, na.rm = TRUE), .groups = "drop") %>%
  mutate(geo = "Bottom_10_Avg", unit = "CLV15_EUR_HAB")

combined_data <- bind_rows(filtered_data, bottom_10_data)

# plot
plot_real_GDP_per_capita <- ggplot(
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
    breaks = seq(0, ceiling(max(combined_data$values, na.rm = TRUE)), by = 5000),
    expand = expansion(mult = c(0.1, 0.1)),
    labels = function(x) format(x, big.mark = ",", scientific = FALSE)
  ) +
  scale_x_continuous(
    breaks = seq(min(combined_data$time, na.rm = TRUE), 
                 max(combined_data$time, na.rm = TRUE), 
                 by = 2),
    expand = c(0.05, 0.05)
  ) +
  labs(
    title = "Κατά κεφαλήν ΑΕΠ",
    subtitle = "Ευρώ, Σταθερές Τιμές (2015), Επιλεγμένες Χώρες και ΕΕ27",
    x = "Έτος",
    y = "Ευρώ, Σταθερές Τιμές (2015)",
    caption = "Source: Eurostat (nama_10_pc)"
  ) +
  theme_greekonomics()

print(plot_real_GDP_per_capita)

################################################################################
######################## Government Debt to GDP ################################
################################################################################

# government debt (gross)
id <- "tipsgo10"

# Retrieve data
data <- get_eurostat(id, time_format = "num")

# Filter data, rename TIME_PERIOD, and select relevant columns
filtered_data <- data %>%
  rename(time = TIME_PERIOD) %>%
  filter(geo %in% list_of_countries, unit == "PC_GDP") %>%
  select(geo, time, values)

# Filter bottom 10 and select relevant columns
Bottom_10_Avg <- data %>%
  rename(time = TIME_PERIOD) %>%
  filter(geo %in% selected_countries, unit == "PC_GDP") %>%
  group_by(time) %>%
  summarise(values = mean(values, na.rm = TRUE), .groups = "drop") %>%
  mutate(geo = "Bottom_10_Avg")

combined_data <- filtered_data %>%
  bind_rows(Bottom_10_Avg)

# plot
plot_government_debt_tipsgo10 <- ggplot(
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
      "Selected_Avg" = "Μέσος Όρος Bottom 10"
    )
  ) +
  scale_y_continuous(
    breaks = seq(floor(min(combined_data$values, na.rm = TRUE)), 
                 ceiling(max(combined_data$values, na.rm = TRUE)), 
                 by = 20),
    expand = c(0.05, 0.05),
    labels = function(x) paste0(x, "%")
  ) +
  scale_x_continuous(
    breaks = seq(min(combined_data$time, na.rm = TRUE), 
                 max(combined_data$time, na.rm = TRUE), 
                 by = 2),
    expand = c(0, 0.5)
  ) +
  labs(
    title = "Χρέος Γενικής Κυβέρνησης",
    subtitle = "Ποσοστό του ΑΕΠ, Επιλεγμένες Χώρες και ΕΕ27",
    x = "Έτος",
    y = "% ΑΕΠ",
    caption = "Source: Eurostat (tipsgo10)"
  ) +
  theme_greekonomics()

print(plot_government_debt_tipsgo10)

################################################################################
################### Current Account Balance - annual data ######################
################################################################################

# CAB
id <- "tipsbp20"

# Retrieve and rename
data <- get_eurostat(id, time_format = "num") %>%
  rename(time = TIME_PERIOD)

# Filter for EL and EU27_2020
filtered_data <- data %>%
  filter(geo %in% list_of_countries, bop_item == "CA", unit == "PC_GDP") %>%
  select(geo, time, values, bop_item, unit)

# Calculate average for bottom 10
Bottom_10_Avg <- data %>%
  filter(geo %in% selected_countries, bop_item == "CA", unit == "PC_GDP") %>%
  group_by(time) %>%
  summarise(values = mean(values, na.rm = TRUE), .groups = "drop") %>%
  mutate(geo = "Bottom_10_Avg", bop_item = "CA", unit = "PC_GDP")

# Combine
combined_data <- bind_rows(filtered_data, Bottom_10_Avg)

#plot
plot_current_account_balance_tipsbp20 <- ggplot(
  combined_data, 
  aes(x = time, y = values, color = geo, linetype = geo)
) +
  geom_line(size = 1, alpha = 0.9) +
  geom_point(size = 2, shape = 21, fill = "white", stroke = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", size = 0.8) +
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
                 by = 2),
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
    title = "Ισοζύγιο Τρεχουσών Συναλλαγών",
    subtitle = "Ποσοστό του ΑΕΠ, Επιλεγμένες Χώρες και ΕΕ27",
    x = "Έτος",
    y = "% ΑΕΠ",
    caption = "Source: Eurostat (tipsbp20)"
  ) +
  theme_greekonomics()



# Display
print(plot_current_account_balance_tipsbp20)


################################################################################
########################### Sectoral Investment ################################
################################################################################


list_of_countries <- c("AT", "BE", "BG", "CY", "CZ", "EE", "EL", "ES", "FI", "FR", "HU", "IE", "IT", "LT", "LV", "NL", "NO", "PL", "RO", "SE", "SI", "SK", "DE", "DK", "HR", "IS", "LU", "MT", "PT", "RS", "UK", "CH", "ME", "AL", "BA")


# Define a muted color palette for top 7 industries (using Greek names)
industry_colors <- c(
  "Ακίνητα" = "#1B3C69",                              # Deep navy
  "Μεταποιητική Βιομηχανία" = "#A6192E",              # Muted red
  "Δημόσια Διοίκηση και Άμυνα" = "#2E7D32",           # Muted green
  "Μεταφορές και Αποθήκευση" = "#4A4A4A",             # Dark grey
  "Χονδρικό και Λιανικό Εμπόριο" = "#6D8299",         # Light blue-grey
  "Γεωργία, Δασοκομία και Αλιεία" = "#D4A017",        # Gold
  "Πληροφορική και Επικοινωνίες" = "#8B5E3C"          # Brown
)

# Define NACE Rev.2 list
nace_r2_list <- c("A", 
                  "B", 
                  "C", 
                  "D", 
                  "E", 
                  "F", 
                  "G", 
                  "H", 
                  "I", 
                  "J", 
                  "K", 
                  "L", 
                  "M", 
                  "N", 
                  "O", 
                  "P", 
                  "Q", 
                  "R", 
                  "S", 
                  "T", 
                  "U")

# Retrieve data from Eurostat
id <- "nama_10_a64_p5"
data <- get_eurostat(id, time_format = "num")

# Function to create a plot for a given country
create_country_plot <- function(country_code) {
  # Filter data, rename TIME_PERIOD, and map NACE codes to industry names
  filtered_data <- data %>%
    rename(time = TIME_PERIOD) %>%
    filter(
      geo == country_code,
      asset10 == "N11G",
      nace_r2 %in% nace_r2_list,
      unit == "CLV15_MEUR"
    ) %>%
    mutate(nace_r2 = case_when(
      nace_r2 == "A" ~ "Γεωργία, Δασοκομία και Αλιεία",
      nace_r2 == "B" ~ "Εξόρυξη και Λατομεία",
      nace_r2 == "C" ~ "Μεταποιητική Βιομηχανία",
      nace_r2 == "D" ~ "Ηλεκτρισμός, Αέριο και Κλιματισμός",
      nace_r2 == "E" ~ "Ύδρευση και Διαχείριση Αποβλήτων",
      nace_r2 == "F" ~ "Κατασκευές",
      nace_r2 == "G" ~ "Χονδρικό και Λιανικό Εμπόριο",
      nace_r2 == "H" ~ "Μεταφορές και Αποθήκευση",
      nace_r2 == "I" ~ "Καταλύματα και Εστίαση",
      nace_r2 == "J" ~ "Πληροφορική και Επικοινωνίες",
      nace_r2 == "K" ~ "Χρηματοπιστωτικές και Ασφαλιστικές",
      nace_r2 == "L" ~ "Ακίνητα",
      nace_r2 == "M" ~ "Επιστημονικές και Τεχνικές Δραστηριότητες",
      nace_r2 == "N" ~ "Διοικητικές και Υποστηρικτικές Υπηρεσίες",
      nace_r2 == "O" ~ "Δημόσια Διοίκηση και Άμυνα",
      nace_r2 == "P" ~ "Εκπαίδευση",
      nace_r2 == "Q" ~ "Υγεία και Κοινωνική Πρόνοια",
      nace_r2 == "R" ~ "Τέχνες και Ψυχαγωγία",
      nace_r2 == "S" ~ "Άλλες Υπηρεσίες",
      nace_r2 == "T" ~ "Οικιακές Δραστηριότητες",
      nace_r2 == "U" ~ "Εξωχώριες Οργανώσεις",
      TRUE ~ nace_r2
    )) %>%
    select(values, time, nace_r2)
  
  # Select top 7 industries by total capital stock
  top_industries <- filtered_data %>%
    group_by(nace_r2) %>%
    summarise(total_stock = sum(values, na.rm = TRUE)) %>%
    top_n(7, total_stock) %>%
    pull(nace_r2)
  
  # Filter data for top industries
  filtered_top <- filtered_data %>%
    filter(nace_r2 %in% top_industries)
  
  # Skip if no data is available for the country
  if (nrow(filtered_top) == 0) {
    message(paste("No data available for country:", country_code))
    return(NULL)
  }
  
  # Calculate dynamic y-axis breaks based on max value
  max_values <- max(filtered_top$values, na.rm = TRUE)
  y_break <- if (max_values < 10000) {
    2000
  } else if (max_values < 50000) {
    5000
  } else if (max_values < 100000) {
    10000
  } else {
    20000
  }
  
  # Create the plot
  plot <- ggplot(
    filtered_top,
    aes(x = time, y = values, color = nace_r2, group = nace_r2)
  ) +
    geom_line(size = 1, alpha = 0.9) +
    geom_point(size = 2, shape = 21, fill = "white", stroke = 0.5) +
    scale_color_manual(
      values = industry_colors,
      labels = c(
        "Ακίνητα" = "Ακίνητα",
        "Μεταποιητική Βιομηχανία" = "Μεταποιητική Βιομηχανία",
        "Δημόσια Διοίκηση και Άμυνα" = "Δημόσια Διοίκηση και Άμυνα",
        "Μεταφορές και Αποθήκευση" = "Μεταφορές και Αποθήκευση",
        "Χονδρικό και Λιανικό Εμπόριο" = "Χονδρικό και Λιανικό Εμπόριο",
        "Γεωργία, Δασοκομία και Αλιεία" = "Γεωργία, Δασοκομία και Αλιεία",
        "Πληροφορική και Επικοινωνίες" = "Πληροφορική και Επικοινωνίες",
        "Κατασκευές" = "Κατασκευές",
        "Υγεία και Κοινωνική Πρόνοια" = "Υγεία και Κοινωνική Πρόνοια"
      )
    ) +
    scale_y_continuous(
      breaks = seq(0, ceiling(max_values), by = y_break),
      expand = expansion(mult = c(0.1, 0.1)),
      labels = scales::comma
    ) +
    scale_x_continuous(
      breaks = seq(min(filtered_top$time, na.rm = TRUE), 
                   max(filtered_top$time, na.rm = TRUE), 
                   by = 4),
      expand = c(0.05, 0.05)
    ) +
    labs(
      title = "Επένδυση ανά Κλάδο (NACE Rev.2) (7 Κορυφαίοι Κλάδοι)",
      subtitle = paste("Σταθερές Τιμές 2015 (Εκατ. Ευρώ), Χώρα:", country_code),
      x = "Έτος",
      y = "Σταθερές τιμές (2015), Εκατ. Ευρώ",
      color = "Κλάδος",
      caption = "Source: Eurostat (nama_10_a64_p5)"
    ) +
    theme_greekonomics() +
    theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5, size = 9),
      legend.position = "bottom",
      legend.text = element_text(size = 9),
      legend.title = element_text(size = 10, face = "bold"),
      legend.key.width = unit(1.2, "cm")
    ) +
    guides(color = guide_legend(nrow = 3, byrow = TRUE))
  
  print(plot)
}

# Loop through each country and plot
for (country in list_of_countries) {
  create_country_plot(country)
}


################################################################################
##################### Unemployment rate - annual data ##########################
################################################################################

# unemployment id
id <- "tipsun20"

list_of_countries <- c("EL", "EU27_2020")

# Retrieve and prepare the data
data <- get_eurostat(id, time_format = "num") %>%
  rename(time = TIME_PERIOD)

# Youth Unemployment (15-24)

# Filter data
filtered_data <- data %>%
  filter(geo %in% list_of_countries, age == "Y15-24") %>%
  select(geo, time, values)

# filter bottom 10
Bottom_10_Avg <- data %>%
  filter(geo %in% selected_countries, age == "Y15-24") %>%
  group_by(time) %>%
  summarise(values = mean(values, na.rm = TRUE), .groups = "drop") %>%
  mutate(geo = "Bottom_10_Avg")

# Combine all data
combined_data <- bind_rows(filtered_data, Bottom_10_Avg)

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
    caption = "Source: Eurostat (tipsun20)"
  ) +
  theme_greekonomics()

# Display the plot
print(plot_unemp_1524_tipsun20)


# Total Unemployment (15-74)

# Filter data and rename
filtered_data <- data %>%
  filter(geo %in% list_of_countries, age == "Y15-74") %>%
  select(geo, time, values)


Bottom_10_Avg <- data %>%
  filter(geo %in% selected_countries, age == "Y15-74") %>%
  group_by(time) %>%
  summarise(values = mean(values, na.rm = TRUE), .groups = "drop") %>%
  mutate(geo = "Bottom_10_Avg")

combined_data <- bind_rows(filtered_data, Bottom_10_Avg)

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
    expand = c(0.05, 0.05),
    labels = function(x) paste0(x, "%")
  ) +
  scale_x_continuous(
    breaks = seq(min(combined_data$time, na.rm = TRUE), 
                 max(combined_data$time, na.rm = TRUE), 
                 by = 2),
    expand = c(0.05, 0.05)
  ) +
  labs(
    title = "Ανεργία (15-74 ετών)",
    subtitle = "Ποσοστό του Εργατικού Δυναμικού, Επιλεγμένες Χώρες και ΕΕ27",
    x = "Έτος",
    y = "% Εργατικού Δυναμικού",
    caption = "Source: Eurostat (tipsun20)"
  ) +
  theme_greekonomics()

print(plot_unemp_1574)

################################################################################
################## Over-qualification rates by citizenship #####################
################################################################################

# Overqualification id
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

# Prepare Greece and EU27 data
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

# Plot
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
    subtitle = "Εργαζόμενοι με Τριτοβάθμια Εκπαίδευση σε Χαμηλής/Μέτριας Δεξιότητας Θέσεις",
    x = "Έτος",
    y = "% Εργαζομένων",
    caption = "Source: Eurostat (lfsa_eoqgan)"
  ) +
  theme_greekonomics() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5)
  )

# Display the plot
print(plot_overqualification)

################################################################################
############## People at Risk of Poverty or Social Exclusion ###################
################################################################################

# id for people at risk of poverty
id <- "tipslc10"

list_of_countries <- c("EL", "EU27_2020")

# Retrieve and rename
data <- get_eurostat(id, time_format = "num") %>%
  rename(time = TIME_PERIOD)

# Filter data and select relevant columns
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
plot_people_risk_of_poverty_tipslc10 <- ggplot(
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
      "Bottom_10_Avg" = "Μέσος Όρος Bottom 10"    )
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
    breaks = seq(max(10, floor(min(combined_data$values, na.rm = TRUE))), 
                 ceiling(max(combined_data$values, na.rm = TRUE)), 
                 by = 5),
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
    title = "Άτομα σε κίνδυνο φτώχειας ή κοινωνικού αποκλεισμού",
    subtitle = "Ποσοστό του Πληθυσμού, Επιλεγμένες Χώρες και ΕΕ27",
    x = "Έτος",
    y = "% Πληθυσμού",
    caption = "Source: Eurostat (tipslc10)"
  ) +
  theme_greekonomics()

print(plot_people_risk_of_poverty_tipslc10)

################################################################################
################## Compensation of employees per hour worked ###################
################################################################################

# Retrieve Eurostat data
id <- "nama_10_lp_ulc"
data <- get_eurostat(id, time_format = "num")

# Filter productivity data
ULC_data <- data %>%
  rename(time = TIME_PERIOD) %>%
  filter(
    geo %in% c("EL", "EU27_2020", selected_countries),
    na_item == "D1_SAL_HW",  # Compensation of employees per hour worked (D1: wages + ESC)
    unit == 'EUR'   # Nominal values
  ) %>%
  select(geo, time, values,unit)


# Filter data and select relevant columns
filtered_data <- ULC_data %>%
  filter(geo %in% list_of_countries) %>%
  select(geo, time, values, unit)


# Calculate average for bottom 10
Bottom_10_Avg <- ULC_data %>%
  filter(geo %in% selected_countries) %>%
  group_by(time) %>%
  summarise(values = mean(values, na.rm = TRUE)) %>%
  mutate(geo = "Bottom_10_Avg")

# Combine Greece, EU27_2020, and selected countries' average
combined_data <- bind_rows(filtered_data,Bottom_10_Avg)

# Create the plot
plot_unit_labour_cost_nominal_nama_10_lp_ulc <- ggplot(
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
      "Bottom_10_Avg" = "Μέσος Όρος Bottom 10"    )
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
    breaks = seq(max(10, floor(min(combined_data$values, na.rm = TRUE))), 
                 ceiling(max(combined_data$values, na.rm = TRUE)), 
                 by = 5),
    expand = expansion(mult = c(0.1, 0.1)),
    labels = function(x) paste0(x, " ")
  ) +
  scale_x_continuous(
    breaks = seq(min(combined_data$time, na.rm = TRUE), 
                 max(combined_data$time, na.rm = TRUE), 
                 by = 2),
    expand = c(0.05, 0.05)
  ) +
  labs(
    title = "Αποζημίωση εργαζομένων ανά ώρα εργασίας",
    subtitle = "Ονομαστικές τιμές",
    x = "Έτος",
    y = "Ευρώ",
    caption = "Source: Eurostat (nama_10_lp_ulc)"
  ) +
  theme_greekonomics()

plot(plot_unit_labour_cost_nominal_nama_10_lp_ulc)
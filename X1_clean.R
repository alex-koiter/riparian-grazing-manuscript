library(tidyverse)
library(readxl)
library(janitor)

# Meta data
site_meta <- tribble(
  ~site, ~plot, ~treatment,
  1,     "a",    "High Graze",
  1,     "b",    "Control",
  1,     "c",    "Regular Graze",
  1,     "d",    "Mow",
  2,     "a",    "Regular Graze",
  2,     "b",    "High Graze",
  2,     "c",    "Mow",
  2,     "d",    "Control",
  3,     "a",    "Regular Graze",
  3,     "b",    "Control",
  3,     "c",    "High Graze",
  3,     "d",    "Mow",
  4,     "a",    "Regular Graze",
  4,     "b",    "High Graze",
  4,     "c",    "Control",
  4,     "d",    "Mow",) %>%
  mutate(site = as.factor(site), plot = as.factor(plot), treatment = as.factor(treatment))

# Soil and other data

data_2019 <- read_excel("../Data/Raw/1 Bio litter bef & after 19 y 20.xlsx", sheet = "2019 Bio Litte Befo & aft  r") %>%
  clean_names() %>%
  mutate(year = "2019") %>%
  rename(sample_type = psource) %>%
  select(sample_type, site, timing, plot,  location, dryweight, year)

data_2020 <- read_excel("../Data/Raw/1 Bio litter bef & after 19 y 20.xlsx", sheet = "2020 all Bio Litte Befo&aft r") %>%
  clean_names() %>%
  mutate(year = "2020") %>%
  rename(sample_type = psource) %>%
  select(sample_type, site, timing, plot,  location, dryweight, year)

data_2021 <- read_excel("../Data/Raw/Soil Sample Spreadsheets_2.xlsx", sheet = "Adriannas Biomass 2021") %>%
  clean_names() %>%
  rename(sample_type = type, timing = time, dryweight = total_biomas_weight_g, location = landscape) %>%
  mutate(site = as.numeric(str_extract(site_id, "[0-9]+")),
         plot = as.factor(str_extract(site_id, "[A-Z]$")),
         year = "2021") %>%
  select(sample_type, site, timing, plot,  location, dryweight, year)

mass_data <- data_2019 %>%
  bind_rows(data_2020, data_2021) %>%
  mutate(sample_type = as.factor(str_to_title(sample_type)),
         location = as.factor(str_to_title(location)),
         timing = as.factor(str_to_title(timing)),
         plot = as.factor(tolower(plot)),
         year = as.factor(year),
         site = as.factor(site)) %>%
  mutate(sample_type = recode(sample_type, Bi = "Biomass", Li = "Litter", Lit = "Litter"),
         location = recode(location, L = "Lower", M = "Middle", U = "Upper", Up = "Upper")) %>%
  right_join(site_meta)
summary(mass_data)

#filter(mass_data, plot == "a", site == "1", year == "2021", sample_type == "Litter")

# Get sheet names
sheet_names <- excel_sheets("../Data/Raw/Phosphorus_final_2.xlsx") %>%
  str_subset("Run")

# Functions
## calculate lm 
get_strd <-  function(f) {
  read_excel("../Data/Raw/Phosphorus_final_2.xlsx", sheet = f, range = "A3:F12") %>%
    clean_names()
}

## get data
get_data <-  function(f) {
  read_excel("../Data/Raw/Phosphorus_final_2.xlsx", sheet = f, skip = 20, na = "MISSING") %>%
    clean_names()
}

## calculate P content
calc_fun <- function(strd, data) {
  data %>%
    mutate(ak_dilute = strd$coefficients[2] * absorbance + strd$coefficients[1]) %>%
    mutate(ak_actual = (ak_dilute * ((extract_vol_m_l + added_h2o_m_l)/1000))/(extract_vol_m_l/1000)) %>%
    mutate(ak_content = ((ak_actual * (extract_vol_m_l/1000))/1000)/(mass_g/1000))
}

# Data analysis
## Create list of lm's
test <- map(sheet_names, get_strd) %>%
  map(~ lm(conc_ug_l ~ abs_885_nm, data = .x))

## Create list of dataframes
test1 <- map(sheet_names, get_data)

## Final calculations
final_conc_data <- tibble(run = sheet_names, standard = test, data = test1) %>%
  mutate(final = map2(standard, data, calc_fun)) %>%
  unnest(final) %>%
  filter(!(sample_id == "Lit_2021_Bef_1" & run == "Run 1")) %>%
  filter(!(sample_id == "Lit_2021_Bef_13" & run == "Run 2")) %>%
  filter(!(sample_id == "Lit_2021_Bef_24" & run == "Run 3")) %>%
  mutate(sample_type = as.factor(str_to_title(sample_type)),
         location = as.factor(str_to_title(location)),
         timing = as.factor(str_to_title(timing)),
         plot = as.factor(tolower(plot)),
         year = as.factor(year),
         site = as.factor(site)) %>%
  left_join(site_meta) %>%
  select(sample_type, site, timing, plot,  location, ak_content, year, treatment) 

bd <- read_excel("../Data/Raw/BD.xlsx") %>%
  clean_names() %>%
  mutate(bd = (dry_mass/1000) / (pi*0.01905^2 * length_cm/100)*2 ) %>%
  mutate(site = as.factor(site),
         plot = tolower(plot)) %>%
  mutate(sample_type = recode(layer, lfh = "LFH"),
        location = recode(location, low = "Lower", mid = "Middle", up = "Upper")) %>%
  left_join(site_meta) %>%
  select(site, plot, location, sample_type, length_cm, bd, treatment)


write_csv(final_conc_data, "Data/P_concentration.csv")
write_csv(mass_data, "Data/mass.csv")
write_csv(bd, "Data/bulk density.csv")




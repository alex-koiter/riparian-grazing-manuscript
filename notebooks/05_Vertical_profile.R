# Load Libraries ----

library(tidyverse)
library(viridis)
library(patchwork)

# Read in clean data  ----
conc <- read_csv("Data/Datasets/final.csv") %>%
  rename("conc" = "ak_content") %>%
  filter(timing == "Before") %>%
  mutate(sample_type = fct_recode(sample_type, "LFH" = "Organic", "Ah" = "Soil")) %>%
  mutate(sample_type = fct_relevel(sample_type, c("Ah", "LFH", "Litter","Biomass"))) %>%
  mutate(location = fct_relevel(location, c("Upper", "Middle", "Lower")))

mass_data <- read_csv("Data/Datasets/mass.csv") %>%
  filter(timing == "Before")

bd_data <- read_csv("Data/Datasets/bd.csv") %>%
  mutate(mass = bd * length_cm/100 * 1)

ggplot(data = bd_data, aes(y = sample_type, x = bd, fill = location)) + 
  geom_boxplot() +
  theme_bw(base_size = 12) +
  labs(x = expression(paste("Bulk Density (", kg~m^{-3}, ")")), y = "P Source") +
  scale_fill_viridis_d(begin = 0.3, end = 1) +
  theme(legend.position = "bottom", 
        legend.title = element_blank())

## plots ----

p1 <-ggplot(data = conc, aes(y = sample_type, x = conc, fill = location)) + 
  geom_boxplot() + 
  #scale_x_log10() +
  theme_bw(base_size = 12) +
  theme(legend.position = c(0.2, 0.8),
        legend.title = element_blank()) +
  labs(x = expression(paste("WEP Concentration (", mg~kg^{-1}, ")")), y = "P Source") +
  scale_fill_viridis_d(name = "Location", begin = 0.3, end = 1)
p1

## Merge data ----
total_data <- conc %>%
  left_join(mass_data) %>%
  left_join(bd_data) %>%
  mutate(p_total = case_when(sample_type == "Biomass" ~ conc * dryweight/1000 /0.25,
                             sample_type == "Litter" ~ conc * dryweight/1000 /0.25,
                             sample_type == "LFH" ~ conc *mass,
                             sample_type == "Ah"  ~ conc *mass)) %>%
  mutate(sample_type = fct_relevel(sample_type, c("Ah", "LFH", "Litter","Biomass"))) %>%
  mutate(location = fct_relevel(location, c("Upper", "Middle", "Lower")))

p2 <- ggplot(data = total_data, aes(y = sample_type, x = p_total, fill = location)) + 
  geom_boxplot() + 
  #scale_x_log10() +
  theme_bw(base_size = 12) +
  theme(axis.title.y = element_blank(),
        legend.position = "none") +
  labs(x = expression(paste("WEP Total (", mg~m^{-2}, ")"))) +
  scale_fill_viridis_d(name = "Location", begin = 0.3, end = 1)
p2

p3 <- p1+p2 + plot_layout(guides = 'collect') & theme(legend.position = 'bottom', legend.title = element_blank())  

p3
ggsave(plot = p3, filename = "Figures/Profile P.png", width = 174, height = 100, units = "mm", dpi = 600) 


# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(stringr)
library(tidyverse)

# Directories
indir <- "data/california/raw"
outdir <- "data/california/processed"

# Read data
data_fin_orig <- readRDS(file=file.path(outdir, "CDPH_1999_2025_finfish_domoic_data.Rds")) 
data_biv_orig <- readRDS(file=file.path(outdir, "CDPH_1991_2025_bivalve_domoic_data.Rds"))


# Merge data
################################################################################

# Column names
colnames(data_biv_orig)
colnames(data_fin_orig)

# Prep for merge
data_fin <- data_fin_orig %>% 
  mutate(class="Finfish",
         source="wild",
         source_use="wild")

# Prep for merge
data_biv <- data_biv_orig %>% 
  mutate(class="Bivalves")

# Merge data
data <- bind_rows(data_biv, data_fin) %>% 
  # Remove some
  select(-c(agency_code, sample_type_code, notes)) %>% 
  # Arrange
  select(sample_id, year, month, date, county, site, lat_dd, long_dd,
         class, comm_name, species, tissue, tissue_use, source, source_use, sample_type, 
         nindiv, modifier, toxicity_ug_g, everything())

# Inspect
str(data)
freeR::complete(data)

# Species key
spp_key <- data %>% 
  count(class, comm_name, species)
freeR::which_duplicated(spp_key$comm_name)
freeR::which_duplicated(spp_key$species)

# Inspect
table(data$county)
table(data$modifier)
table(data$tissue)
table(data$source)

# Plot
ggplot(data, aes(x=date, y=lat_dd, color=comm_name, size=toxicity_ug_g)) +
  geom_point() + 
  theme_bw()


# Export data
################################################################################

# Export
saveRDS(data, file=file.path(outdir, "CDPH_1991_2025_all_domoic_data.Rds"))



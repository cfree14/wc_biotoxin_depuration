
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
data_fin <- readRDS(file=file.path(outdir, "CDPH_1999_2025_finfish_domoic_data.Rds")) %>% 
  mutate(class="Finfish",
         source="wild")
data_biv <- readRDS(file=file.path(outdir, "CDPH_1991_2025_bivalve_domoic_data.Rds")) %>% 
  mutate(class="Bivalves")


# Merge data
################################################################################

# Column names
colnames(data_biv)
colnames(data_fin)

# Merge data
data <- bind_rows(data_biv, data_fin) %>% 
  # Remove some
  select(-c(agency_code, sample_type_code, notes)) %>% 
  # Arrange
  select(sample_id, year, month, date, county, site, lat_dd, long_dd,
         class, comm_name, species, tissue, source, sample_type, 
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


# Export data
################################################################################

# Export
saveRDS(data, file=file.path(outdir, "CDPH_1991_2025_all_domoic_data.Rds"))



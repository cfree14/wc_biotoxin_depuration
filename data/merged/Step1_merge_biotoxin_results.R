
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(stringr)
library(tidyverse)

# Directories
cadir <- "data/california/processed"
ordir <- "data/oregon/processed"
wadir <- "data/washington/processed"
outdir <- "data/merged/processed"

# Read data
ca_biv_orig <- readRDS(file=file.path(cadir, "CDPH_1991_2024_bivalve_domoic_data.Rds"))


# Format
################################################################################

# Format
ca_biv <- ca_biv_orig %>% 
  # Add
  mutate(state="California") %>% 
  # Simplify
  select(state, sample_id, year, month, date, 
         site, lat_dd, long_dd, 
         comm_name, species, tissue, source, 
         nindiv, mod_asp, asp_ug_g, notes)


# Merge
################################################################################


# Export
################################################################################

# Export
# save(data, file=file.path(outdir, "WC_domoic_acid_data.Rdata"))



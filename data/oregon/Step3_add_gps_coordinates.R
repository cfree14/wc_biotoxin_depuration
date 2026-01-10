
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(stringr)
library(tidyverse)

# Directories
intdir <- "data/oregon/intermediate"
outdir <- "data/oregon/processed"

# Read data
data_psp_orig <- readRDS(file=file.path(outdir, "ODA_1999_2025_psp_data.Rds"))
data_da_orig <- readRDS(file=file.path(outdir, "ODA_1999_2025_domoic_data.Rds"))
data_dsp_orig <- readRDS(file=file.path(outdir, "ODA_1999_2025_dsp_data.Rds"))

# Read site key
site_key_orig <- readxl::read_excel(file.path(intdir, "site_key_final.xlsx"))


# Add coordinates
################################################################################

# DA
data_da <- data_da_orig %>% 
  rename(site_orig=site) %>% 
  left_join(site_key_orig) %>% 
  select(sample_id:site_orig, site, lat_dd, long_dd, everything())

# PSP
data_psp <- data_psp_orig %>% 
  rename(site_orig=site) %>% 
  left_join(site_key_orig) %>% 
  select(sample_id:site_orig, site, lat_dd, long_dd, everything())

# DSP
data_dsp <- data_dsp_orig %>% 
  rename(site_orig=site) %>% 
  left_join(site_key_orig) %>% 
  select(sample_id:site_orig, site, lat_dd, long_dd, everything())

# Which missing lat/long?
data_da %>% filter(is.na(lat_dd)) %>% pull(site) %>% unique()
data_psp %>% filter(is.na(lat_dd)) %>% pull(site) %>% unique()
data_dsp %>% filter(is.na(lat_dd)) %>% pull(site) %>% unique()


# Export data
################################################################################

# Exort
saveRDS(data_psp, file=file.path(outdir, "ODA_1999_2025_psp_data_gps.Rds"))
saveRDS(data_da, file=file.path(outdir, "ODA_1999_2025_domoic_data_gps.Rds"))
saveRDS(data_dsp, file=file.path(outdir, "ODA_1999_2025_dsp_data_gps.Rds"))



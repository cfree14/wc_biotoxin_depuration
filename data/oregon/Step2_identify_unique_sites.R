
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
data_psp <- readRDS(file=file.path(outdir, "ODA_1999_2025_psp_data.Rds"))
data_da <- readRDS(file=file.path(outdir, "ODA_1999_2025_domoic_data.Rds"))
data_dsp <- readRDS(file=file.path(outdir, "ODA_1999_2025_dsp_data.Rds"))


# Identify sites
################################################################################

# Unadulterated site names
sites_orig <- sort(unique(c(data_dsp$site,
                            data_psp$site,
                            data_da$site)))

# Build key
key <- tibble(site_orig=sites_orig) %>% 
  # Simple formatting
  mutate(site=stringr::str_to_title(site_orig))

# Export
write.csv(key, file.path(intdir, "site_key_temp.csv"), row.names = F)

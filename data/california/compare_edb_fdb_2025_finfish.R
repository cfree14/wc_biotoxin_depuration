

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(stringr)
library(tidyverse)
library(plotly)

# Directories
indir <- "data/california/raw/fdb_christina"
outdir <- "data/california/processed"
intdir <- "data/california/intermediate"

# Read data
data_fdb_orig <- readRDS(file=file.path(outdir, "CDPH_FDB_2025_finfish_domoic_data.Rds"))
data_emb_orig <- readRDS(file=file.path(outdir, "CDPH_EMB_1999_2025_finfish_domoic_data.Rds"))


# Compare
################################################################################

# Reduce EMB to 2025
data_emb <- data_emb_orig %>% 
  mutate(year=lubridate::year(date)) %>% 
  filter(year==2025)

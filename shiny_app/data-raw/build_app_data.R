
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(stringr)
library(tidyverse)

# Directories
outdir <- "data/merged/processed"
tabledir <- "tables"
plotdir <- "figures"

# Read data
dsp_orig <- readRDS(file=file.path(outdir, "WC_dsp_data.Rds"))
psp_orig <- readRDS(file=file.path(outdir, "WC_psp_data.Rds"))
asp_orig <- readRDS(file=file.path(outdir, "WC_domoic_acid_data.Rds"))


# Prep data
################################################################################

# Prep DA
asp <- asp_orig %>% 
  # Add
  mutate(toxin="Domoic acid") %>% 
  # Rename
  rename(toxicity=toxicity_ppm) %>% 
  # Reduce
  select(toxin, date, lat_dd, comm_name, species, toxicity)

# Prep PSP
psp <- psp_orig %>% 
  # Add
  mutate(toxin="Paralytic shellfish toxin") %>% 
  # Rename
  rename(toxicity=toxicity_ug_100g) %>% 
  # Reduce
  select(toxin, date, lat_dd, comm_name, species, toxicity)

# Prep DSP
dsp <- dsp_orig %>% 
  # Add
  mutate(toxin="Diarrhetic shellfish toxin") %>% 
  # Rename
  rename(toxicity=toxicity_ug_100g) %>% 
  # Reduce
  select(toxin, date, lat_dd, comm_name, species, toxicity)

# Merge data
################################################################################

# Merge
data <- bind_rows(asp, psp, dsp) %>% 
  na.omit() %>% 
  mutate(year=lubridate::year(date))

freeR::complete(data)


# Export data
################################################################################

# Export
saveRDS(data, file=file.path("shiny_app/data/biotoxin_data.Rds"))

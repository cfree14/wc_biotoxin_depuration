
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
data_psp <- readRDS(file=file.path(outdir, "WC_psp_data.Rds"))
data_asp <- readRDS(file=file.path(outdir, "WC_domoic_acid_data.Rds"))
data_dsp <- readRDS(file=file.path(outdir, "WC_dsp_data.Rds"))


# Build and merge keys
################################################################################

# PSP stats
spp_psp <- data_psp %>% 
  group_by(comm_name, species) %>% 
  summarize(n=n()) %>% 
  ungroup()

# ASP stats
spp_asp <- data_asp %>% 
  group_by(comm_name, species) %>% 
  summarize(n=n()) %>% 
  ungroup()

# DSP stats
spp_dsp <- data_dsp %>% 
  group_by(comm_name, species) %>% 
  summarize(n=n()) %>% 
  ungroup()

# Merge
spp_key <- bind_rows(spp_psp, spp_asp, spp_dsp) %>% 
  select(comm_name, species) %>% 
  unique()

# Check
freeR::which_duplicated(spp_key$comm_name)
freeR::which_duplicated(spp_key$species)

# Look up lat/long
df <- freeR::fishbase(dataset="species", species=spp_key$species, cleaned=F)
colnames(df)
# df1 <- df %>%

df <- rfishbase::distribution(spp_key$species)
df1 <- df %>% 
  # Simplify
  select(Species, SouthernLatitude, SouthernLatitudeNS, NorthernLatitude, NorthernLatitudeNS) %>% 
  filter(!is.na(SouthernLatitude)) %>% 
  # Rename
  rename(species=Species,
         lat_dd_s=SouthernLatitude,
         lat_dd_n=NorthernLatitude)
  

# Look up species
################################################################################

# Species
scinames <- spp_key$species






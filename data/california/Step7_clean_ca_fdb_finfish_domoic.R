

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

# Blocks
blocks <- wcfish::blocks %>% 
  mutate(block_id=as.character(block_id))

# Sheets
sheets <- readxl::excel_sheets(file.path(indir, "PRAR_DA_2015-2025_12_19_25.xlsx"))
sheets

# Read data
seafood24_orig <- readxl::read_excel(file.path(indir, "PRAR_DA_2015-2025_12_19_25.xlsx"), sheet="Seafood 2024-2025") 

# To do
# DONE

# Format seafood data
################################################################################

# Format
seafood <- seafood24_orig %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  rename(sample_id=is_number,
         comm_name=species, 
         block_id=block_number,
         date=date_of_catch,
         toxicity_ppm=result_ppm_fda_action_30) %>% 
  # Format block id
  mutate(block_id=gsub("Block ", "", block_id) ) %>% 
  # Add lat/long
  left_join(blocks %>% select(block_id, block_lat_dd, block_long_dd), by="block_id") %>% 
  rename(lat_dd=block_lat_dd,
         long_dd=block_long_dd) %>% 
  # Format common name
  mutate(comm_name=stringr::str_to_sentence(comm_name),
         comm_name=recode(comm_name,
                          "Anchovies" = "Northern anchovy",
                          "M. squid" = "Market squid",
                          "N. anchovies" = "Northern anchovy",
                          "P. mackerel" = "Pacific mackerel",
                          "Pa. Sardines" = "Pacific sardine")) %>% 
  # Add species
  mutate(species=recode(comm_name,
                        "Jack mackerel" = "Trachurus symmetricus",       
                        "Jacksmelt" = "Atherinopsis californiensis",   
                        "Market squid" = "Doryteuthis opalescens",  
                        "Northern anchovy" = "Engraulis mordax",  
                        "Pacific mackerel" = "Scomber japonicus",   
                        "Pacific sardine" = "Sardinops sagax")) %>% 
  # Format toxicity
  mutate(modifier=ifelse(grepl("<", toxicity_ppm), "<", "="),
         toxicity_ppm=gsub("<", "", toxicity_ppm) %>% as.numeric(.)) %>% 
  # Arrange
  select(sample_id, date, 
         block_id, lat_dd, long_dd,
         comm_name, species, number_of_samples, modifier, toxicity_ppm, everything())

# Inspect
str(seafood)
freeR::complete(seafood)

# Inspect more
range(seafood$date)
table(seafood$block_id)
table(seafood$comm_name)
#freeR::check_names(seafood$species)

# Export
saveRDS(seafood, file=file.path(outdir, "CDPH_FDB_2025_finfish_domoic_data.Rds"))


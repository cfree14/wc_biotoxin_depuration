

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(stringr)
library(tidyverse)

# Directories
indir <- "data/california/raw/fdb_christina"
outdir <- "data/california/processed"

# Read data
data_orig <- readxl::read_excel(file.path(indir, "PRAR_DA_2000-2015.xlsx")) 

# To do
# DONE: Filling missing coordinates appears impossible

# Format data
################################################################################

# Format data
data <- data_orig %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  rename(date=date_of_catch,
         site=collection_site,
         toxicity_ppm=result_ppm_fda_action_30,
         coords=lat_long_coordinates) %>% 
  # Separate coordinates
  separate(coords, sep=" ", into=c("lat_dd", "long_dd")) %>% 
  mutate_at(vars(lat_dd, long_dd), as.numeric) %>% 
  # Add tissue
  mutate(comm_name="Dungeness crab",
         species="Metacarcinus magister") %>% 
  # Add tissue
  mutate(tissue=gsub("Crab, Dungeness, ", "", sample_type)) %>% 
  # Arrange
  select(date, 
         county, site, lat_dd, long_dd, 
         sample_type, comm_name, species, tissue, 
         toxicity_ppm, notes,
         everything())

# Inspect
str(data)
freeR::complete(data)

# County
table(data$county)

# Tissue
table(data$tissue)

# Site
site_key <- data %>% 
  count(county, site)

# Sample type
table(data$sample_type)

# Date
range(data$date)


# Export data
################################################################################

saveRDS(data, file=file.path(outdir, "CDPH_FDB_2000_2015_crab_domoic_data.Rds"))

 
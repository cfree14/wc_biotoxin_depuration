
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(stringr)
library(tidyverse)

# Directories
indir <- "data/washington/raw"
outdir <- "data/washington/processed"

# Read data
data_orig <- readxl::read_excel(file.path(indir, "Biotoxin_Data_1957_to_September_2025.xlsx"), na="NULL")

# Read grid key
grid_orig <- readxl::read_excel(file.path(indir, "Grid_Codes_2025.xlsx"), na="Null")

# Read species key
spp_orig <- readxl::read_excel(file.path(indir, "WDOH_species_key.xlsx"))

# THERE IS A LOT TO DO HERE


# Format grid key
################################################################################

# Format grid
grid <- grid_orig %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  rename(lat_dd=lat,
         long_dd=lon,
         waterbody_id=wbid,
         waterbody_use=waterbody_name_use,
         county_id=county_code) %>% 
  # Format longitude
  mutate(long_dd=abs(long_dd)*-1) %>% 
  # Arrange
  select(site_id, waterbody_id, waterbody, waterbody_use, everything())

# Unique id?
freeR::which_duplicated(grid$site_id)

# Inspect
str(grid)
freeR::complete(grid)

# Plot map
ggplot() +
  geom_point(data=grid, mapping=aes(x=long_dd, y=lat_dd, color=county))

# Waterbody key
waterbody_key <- grid %>% 
  count(waterbody_id, waterbody, waterbody_use)
freeR::which_duplicated(waterbody_key$waterbody_id)

# County key
county_key <- grid %>% 
  count(county_id, county)
freeR::which_duplicated(county_key$county_id)


# Format data
################################################################################

# Format data
data <- data_orig %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  rename(psp_id=psp_number,
         da_id=da_number,
         dsp_id=dsp_number,
         date_collected=collect_date,
         date_submitted=submit_date,
         organization=org,
         site=site_name,
         comm_name=species,
         da_result=domoic_results,
         da_date=domoic_date,
         psp_date=lab_psp_date,
         date_lab_received=lab_receive_date_time) %>% 
  # Format species
  mutate(comm_name=stringr::str_to_sentence(comm_name)) %>% 
  # Add species
  left_join(spp_orig %>% select(comm_name, species)) %>% 
  # Add lat/long
  left_join(grid %>% select(site_id, lat_dd, long_dd)) %>% 
  # Arrange
  select(county, waterbody, site, subsite, site_id, lat_dd, long_dd,
         organization, cert_number, 
         date_collected, date_submitted, date_lab_received,
         comm_name, species, sample_type, shell_shucked, fresh_frozen, monitoring_type,
         da_id, da_date, da_tissue, da_result,
         psp_id, psp_date, psp_tissue, psp_result,
         dsp_id, dsp_date, dsp_tissue, dsp_result,
         everything())

# Inspect
colnames(data)
str(data)
freeR::complete(data)

# Location
sort(unique(data$county))
sort(unique(data$waterbody))
sort(unique(data$site))
sort(unique(data$subsite))
sort(unique(data$site_id))

# Site key - clean this up
# Adding waterbody duplicates 1
# Adding site adds a few more
site_key <- data %>% 
  count(site_id, county, waterbody, site)

# Organizatin key - learn more about this, not 1:1
org_key <- data %>% 
  count(organization, cert_number)
freeR::which_duplicated(org_key$organization)
freeR::which_duplicated(org_key$cert_number)

# Species
spp_key <- data %>% 
  count(comm_name, species)

# Sample info
table(data$sample_type)
table(data$shell_shucked)
table(data$fresh_frozen)
table(data$monitoring_type)


# Export data
################################################################################

# Export data
saveRDS(data, file=file.path(outdir, "WDOH_1957_2025_biotoxin_data.Rds"))


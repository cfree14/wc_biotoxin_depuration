
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
data_orig <- readxl::read_excel(file.path(indir, "Biotoxin_Data_1957_to_September_2025.xlsx"), na="NULL", col_types = "text")

# Read grid key
grid_orig <- readxl::read_excel(file.path(indir, "Grid_Codes_2025.xlsx"), na="Null")

# Read species key
spp_orig <- readxl::read_excel(file.path(indir, "WDOH_species_key.xlsx"))

# Check species names
# freeR::check_names(spp_orig$species)

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
  # Format dates
  mutate(date_collected=as.numeric(date_collected) %>% as.Date(., origin = "1899-12-30") %>% lubridate::ymd(.),
         date_submitted=as.numeric(date_submitted) %>% as.Date(., origin = "1899-12-30") %>% lubridate::ymd(.),
         date_lab_received=as.numeric(date_lab_received) %>% as.Date(., origin = "1899-12-30") %>% lubridate::ymd(.),
         da_date=as.numeric(da_date) %>% as.Date(., origin = "1899-12-30") %>% lubridate::ymd(.),
         psp_date=as.numeric(psp_date) %>% as.Date(., origin = "1899-12-30") %>% lubridate::ymd(.),
         dsp_date=as.numeric(dsp_date) %>% as.Date(., origin = "1899-12-30") %>% lubridate::ymd(.)) %>% 
  # Add year/month
  mutate(year_collected=lubridate::year(date_collected),
         month_collected=lubridate::month(date_collected)) %>% 
  # Format species
  mutate(comm_name=stringr::str_to_sentence(comm_name)) %>% 
  # Add species
  left_join(spp_orig %>% select(comm_name, species), by="comm_name") %>% 
  # Add lat/long
  left_join(grid %>% select(site_id, lat_dd, long_dd), by="site_id") %>% 
  # Recode DA: <1, NoTest, NTD, UNSAT
  # Recode PSP: <38, UNSAT, NTD, NoTest
  # Recode DSP: <1, UNSAT, NTD, No Test, 
  # Arrange
  select(county, waterbody, site, subsite, site_id, lat_dd, long_dd,
         organization, cert_number, 
         year_collected, month_collected, date_collected, 
         date_submitted, date_lab_received,
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

# Results
sort(unique(data$da_result))
sort(unique(data$psp_result)) %>% rev()
sort(unique(data$dsp_result)) %>% rev()


# Site key - clean this up
# Adding waterbody duplicates 1
# Adding site adds a few more
site_key <- data %>% 
  count(site_id, county, waterbody, site)

# Organization key - learn more about this, not 1:1
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


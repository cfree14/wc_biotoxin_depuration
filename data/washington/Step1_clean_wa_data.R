
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

# You can tell the county from the site code
# Can monitoring type be turned into "source/origin?"


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
  # Fix county code
  # This isn't official - just your judgement
  mutate(county_id=case_when(county=="Grays Harbor" ~ "24", 
                             is.na(county_id) ~ "00",
                             T ~ county_id)) %>% 
  # Fix county
  mutate(county=case_when(is.na(county) | county=="British Columbia" ~ "Unknown",
                          T ~ county)) %>% 
  # Check waterbody (they only disagree in a few places and waterbody use is better)
  mutate(waterbody_check=waterbody==waterbody_use) %>% 
  select(-c(waterbody, waterbody_check)) %>% 
  rename(waterbody=waterbody_use) %>% 
  # Arrange
  select(site_id, waterbody_id, waterbody, everything())

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
  count(waterbody_id, waterbody)
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
  # Update common names
  mutate(comm_name=recode(comm_name,
                          "Bent nose clam"="Bent-nose macoma clam",
                          "Cockle"="Nuttall's cockle",
                          "Abalone"="Pinto abalone",
                          "Varnish clam"="Purple varnish clam",
                          "Kelp crab"="Northern kelp crab")) %>% 
  # Fix a CA site that incorrectly has a WA site id
  # mutate(site_id=ifelse(subsite=="2-CSR-PSP-NE50-  8/13/18" & !is.na(subsite), "USCA001", site_id),
  #        site=ifelse(subsite=="2-CSR-PSP-NE50-  8/13/18" & !is.na(subsite), "CSR-US-Pacific 100-000-1590", site),
  #        waterbody=ifelse(subsite=="2-CSR-PSP-NE50-  8/13/18" & !is.na(subsite), "California", waterbody)) %>%
  # Fix an incorrect site
  mutate(site=recode(site,
                     "Mukkaw Bay" = "Makah Bay",
                     "Sunny Shore Acres" = "Tillicum Beach")) %>%
  # Add lat/long
  left_join(grid %>% select(site_id, lat_dd, long_dd), by="site_id") %>% 
  # Recode DA: <1, NoTest, NTD, UNSAT
  mutate(da_modifier=ifelse(da_result %in% c("<1", "NTD"), "<", "="),
         da_result=recode(da_result, 
                          "NoTest"="",
                          "NTD"="1",
                          "UNSAT"="",
                          "<1"="1") %>% as.numeric(.)) %>% 
  # Recode PSP: <38, UNSAT, NTD, NoTest
  mutate(psp_modifier=ifelse(psp_result %in% c("<38", "NTD"), "<", "="),
         psp_result=recode(psp_result, 
                          "NoTest"="",
                          "NTD"="38",
                          "UNSAT"="",
                          "<38"="38") %>% as.numeric(.)) %>% 
  # Recode DSP: <1, UNSAT, NTD, No Test
  mutate(dsp_modifier=ifelse(dsp_result %in% c("<1", "NTD"), "<", "="),
         dsp_result=recode(dsp_result, 
                          "No Test"="",
                          "NTD"="1",
                          "UNSAT"="",
                          "<1"="1") %>% as.numeric(.)) %>% 
  # Add state
  mutate(state="Washington",
         state=case_when(waterbody %in% c("Oregon", "California", "Alaska", "Canada") ~ waterbody, 
                         waterbody == "Out of Country" ~ "British Columbia",
                         waterbody %in% c("Out of State", "Other", "Unknown Location") ~ "Unknown",
                         T ~ state),
         
         state=recode(state, "Canada"="British Columbia")) %>% 
  # Fill missing counties
  mutate(county=case_when(organization=="Catalina Sea Ranch" ~ "Los Angeles", 
                          T ~ county)) %>% 
  # Mark coastal (yes/no)
  mutate(outer_yn=ifelse(county %in% c("Pacific", "Grays Harbor", "Jefferson"), "yes", "no")) %>% 
  # Fill shell shucked
  mutate(shell_shucked=ifelse(is.na(shell_shucked), "Unknown", shell_shucked)) %>% 
  # Fill fresh/frozen
  mutate(fresh_frozen=ifelse(is.na(fresh_frozen), "Unknown", fresh_frozen)) %>% 
  # Fill monitoring type
  mutate(monitoring_type=ifelse(is.na(monitoring_type), "Unknown", monitoring_type)) %>% 
  # Format tissue
  mutate_at(vars(da_tissue, dsp_tissue, psp_tissue), tolower) %>% 
  # Arrange
  select(state, outer_yn, county, waterbody, site, subsite, site_id, lat_dd, long_dd,
         organization, cert_number, 
         year_collected, month_collected, date_collected, 
         date_submitted, date_lab_received,
         comm_name, species, sample_type, shell_shucked, fresh_frozen, monitoring_type,
         da_id, da_date, da_tissue, da_modifier, da_result,
         psp_id, psp_date, psp_tissue, psp_modifier, psp_result,
         dsp_id, dsp_date, dsp_tissue, dsp_modifier, dsp_result,
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
sort(unique(data$da_result)) %>% rev()
sort(unique(data$psp_result)) %>% rev()
sort(unique(data$dsp_result)) %>% rev()


# Site key
site_key <- data %>% 
  count(site_id, state, county, waterbody, site) # lat_dd, long_dd) 
freeR::which_duplicated(site_key$site_id) # don't worry about USCA000 (California code)

# Organization key - learn more about this, not 1:1, messed up but who cares
org_key <- data %>% 
  count(organization, cert_number)
freeR::which_duplicated(org_key$organization)
freeR::which_duplicated(org_key$cert_number)

# Species
spp_key <- data %>% 
  count(comm_name, species)
freeR::which_duplicated(spp_key$comm_name)
freeR::which_duplicated(spp_key$species)

# Sample info
table(data$sample_type)
table(data$shell_shucked)
table(data$fresh_frozen)
table(data$monitoring_type)

# Tissues
table(data$da_tissue)
table(data$psp_tissue)
table(data$dsp_tissue)

# Sites w/out GPS
sites_no_gps <- data %>% 
  filter(is.na(lat_dd)) %>% 
  select(state, county, waterbody, site, site_id) %>% 
  unique() %>% 
  filter(state=="Washington") %>% 
  select(-state) %>% arrange(county)
n_distinct(sites_no_gps$site_id)
sum(sites_no_gps$site_id%in%grid$site_id)
write.csv(sites_no_gps, file="data/washington/intermediate/sites_wout_gps_coords.csv")

# Export data
################################################################################

# Export data
saveRDS(data, file=file.path(outdir, "WDOH_1957_2025_biotoxin_data.Rds"))


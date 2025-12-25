
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(stringr)
library(tidyverse)

# Directories
indir <- "data/california/raw"
outdir <- "data/california/processed"

# Read data
data_orig1 <- readxl::read_excel(file.path(indir, "CDPH_domoic-acid-bivalve_data_121824.xlsx"), col_types = "text") # 1991-2024
data_orig2 <- readxl::read_excel(file.path(indir, "DA_12-2024-2025.xlsx"), col_types = "text") # 2025

# Sample key
type_key <- readxl::read_excel(file.path(indir, "sample_type_key_bivalve_domoic.xlsx"))


# Clean data
################################################################################

# Column names
colnames(data_orig1)
colnames(data_orig2) # No agency code, shellfish code, or notes

# Clean data
data <- bind_rows(data_orig1, data_orig2) %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  rename(sample_id=srl_number,
         site=sample_site,
         date=date_sampled, 
         lat_dd=latitude,
         long_dd=longitude, 
         sample_type_code=shellfish_code, 
         nindiv=number_of_individuals,
         modifier=mod_asp,
         toxicity_ug_g=asp_ug_g) %>% 
  # Convert numbers
  mutate_at(vars(long_dd, lat_dd, toxicity_ug_g, nindiv), as.numeric) %>% 
  # Convert date
  mutate(date=as.numeric(date) %>% as.Date(., origin = "1899-12-30") %>% lubridate::ymd(.)) %>% 
  # Format longitude
  mutate(long_dd=abs(long_dd)*-1) %>% 
  # Format species
  mutate(species=recode(species,
                        "Clinocardium nuttalli" = "Clinocardium nuttallii",        
                        "Magallana sikamea" = "Magallana gigas",             
                        # "Mytilus gallo/trossulus/edulis" = "",
                        "Prototheca staminea" = "Leukoma staminea",   
                        "Tresus nuttalli" = "Tresus nuttallii",
                        "Tapes japonica" = "Ruditapes philippinarum",
                        "Mytilus gallo/trossulus/edulis"="Mytilus galloprovincialis/trossulus/edulis")) %>% 
  # Add sample type info
  left_join(type_key, by="sample_type") %>% 
  # Fix common name
  mutate(comm_name=case_when(species=="Mytilus galloprovincialis/trossulus/edulis" ~ "Sea/blue/bay mussels",
                             T ~ comm_name)) %>% 
  # Fill missing tissue/source
  mutate(tissue=ifelse(is.na(tissue), "not specified", tissue)) %>% 
  mutate(source=ifelse(is.na(source), "not specified", source)) %>% 
  # Add tissue/source with assumptions
  mutate(tissue_use=recode(tissue, "not specified"="whole"),
         source_use=recode(source, "not specified"="wild")) %>% 
  # Format scientific name
  mutate(species=case_when(comm_name=="Sea/bay mussels" ~ "Mytilus galloprovincialis/edulis", 
                           comm_name=="Unidentified clam" ~ "Bivalvia spp.",
                           T ~ species)) %>% 
  # Add date info
  mutate(year=lubridate::year(date),
         month=lubridate::month(date)) %>% 
  # Arrange
  select(sample_id, year, month, date, 
         agency_code, county, site, lat_dd, long_dd, 
         species, comm_name, tissue, source, tissue_use, source_use,
         sample_type_code, sample_type, 
         nindiv, modifier, toxicity_ug_g, notes, everything())
  
# Inspect
str(data)
freeR::complete(data)

# Date
range(data$year)

# County
table(data$county)

# Agency
table(data$agency_code)
table(data$source)

# Source
table(data$tissue)

# Sample type
type_key <- data %>% 
  count(sample_type_code, sample_type, comm_name, tissue, source)
freeR::which_duplicated(type_key$sample_type_code)

# Lat/long
range(data$lat_dd, na.rm=T)
range(data$long_dd, na.rm=T)

# Species key
spp_key <- data %>% 
  count(species, comm_name)
freeR::which_duplicated(spp_key$comm_name)
freeR::which_duplicated(spp_key$species)

# Site key
site_key <- data %>% 
  group_by(county, site) %>% 
  summarize(n=n(),
            lat_dd=median(lat_dd, na.rm=T),
            long_dd=median(long_dd, na.rm=T)) %>% 
  ungroup()

# Check species
# freeR::check_names(spp_key$species)


# Plots
################################################################################

ggplot(data, aes(x=date, y=lat_dd, color=source_use, size=toxicity_ug_g)) +
  facet_wrap(~comm_name, ncol=5) +
  geom_point() +
  # Labels
  labs(x="Date", y="Latitude (°N)") +
  # Theme
  theme_bw()


# Export
################################################################################

# Export
range(data$year)
saveRDS(data, file=file.path(outdir, "CDPH_1991_2025_bivalve_domoic_data.Rds"))





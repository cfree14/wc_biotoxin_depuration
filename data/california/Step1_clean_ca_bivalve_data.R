
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
data_orig <- readxl::read_excel(file.path(indir, "CDPH_domoic-acid-bivalve_data_121824.xlsx"), col_types = "text")


# Clean data
################################################################################

# Clean data
data <- data_orig %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  rename(sample_id=srl_number,
         site=sample_site,
         date=date_sampled, 
         lat_dd=latitude,
         long_dd=longitude, 
         sample_type_code=shellfish_code, 
         nindiv=number_of_individuals,
         sample_type_orig=sample_type) %>% 
  # Convert numbers
  mutate_at(vars(long_dd, lat_dd, asp_ug_g, nindiv), as.numeric) %>% 
  # Convert date
  mutate(date=as.numeric(date) %>% as.Date(., origin = "1899-12-30") %>% lubridate::ymd(.)) %>% 
  # mutate(date=lubridate::ymd(date)) %>% 
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
  # Format sample type (to break into common name and other)
  mutate(sample_type_orig=recode(sample_type_orig,
                                 "Clam, razor"="Razor clam",
                                 "Gaper Clam meat" = "Gaper Clam, meat",             
                                 "Gaper Clam siphon" = "Gaper Clam, siphon",              
                                 "Gaper Clam viscera" = "Gaper Clam, viscera",
                                 "Pismo Clam meat" = "Pismo Clam, meat",             
                                 "Pismo Clam viscera" = "Pismo Clam, viscera",  
                                 "Razor Clam meat" = "Razor Clam, meat",              
                                 "Razor Clam viscera" = "Razor Clam, viscera",  
                                 "Rock Scallop adductor" = "Rock Scallop, adductor",   
                                 "Rock Scallop mantle, gills" = "Rock Scallop, mantle/gills", 
                                 "Rock Scallop viscera" = "Rock Scallop, viscera",
                                 "Sentinel Pacific Oyster" = "Pacific Oyster, Sentinel",
                                 "Clam, unidentified" = "Unidentified clam",
                                 "Cultured Rock Scallop,adductor" = "Rock Scallop, cultured/adductor",
                                 "Cultured Rock Scallop, viscera"= "Rock Scallop, cultured/viscera")) %>% 
  # Break into common name and other
  separate(sample_type_orig, into=c("comm_name", "tissue"), sep=", ", remove = F) %>% 
  # Format common names
  mutate(comm_name=stringr::str_to_sentence(comm_name),
         comm_name=case_when(species=="Mytilus galloprovincialis/trossulus/edulis" ~ "Mixed bay/blue/sea mussels",
                             T ~ comm_name)) %>% 
  # Format tissue/source
  mutate(tissue=stringr::str_to_lower(tissue),
         source=case_when(grepl("cultured", tissue) ~ "cultured",
                          grepl("sentinel", tissue) ~ "sentinel",
                          grepl("wild", tissue) ~ "wild",
                          T ~ NA),
         tissue=recode(tissue,
                       "cultured"="",
                       "sentinel"="",
                       "wild"="",
                       "cultured/adductor"="adductor",
                       "cultured/viscera"="viscera"),
         tissue=ifelse(tissue=="", NA, tissue)) %>% 
  # Fill missing tissue (assume NAs = whole)
  mutate(tissue=ifelse(is.na(tissue), "whole", tissue)) %>% 
  # Fill missing source (assume NAs = wild)
  mutate(source=ifelse(is.na(source), "wild", source)) %>% 
  # Format scientific name
  mutate(species=case_when(comm_name=="Mixed sea/bay mussels" ~ "Mytilus galloprovincialis/edulis", 
                           comm_name=="Unidentified clam" ~ "Bivalvia spp.",
                           T ~ species)) %>% 
  # Add date info
  mutate(year=lubridate::year(date),
         month=lubridate::month(date)) %>% 
  # Arrange
  select(sample_id, year, month, date, 
         agency_code, county, site, lat_dd, long_dd, 
          species, comm_name, tissue, source, 
          sample_type_code, sample_type_orig, 
          nindiv, mod_asp, asp_ug_g, notes, everything())
  
# Inspect
str(data)
freeR::complete(data)

# Date
range(data$year)

# County
table(data$county)

# Agency
table(data$agency_code)

# Sample type
type_key <- data %>% 
  count(sample_type_code, sample_type_orig, comm_name, tissue, source)
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

ggplot(data, aes(x=date, y=lat_dd, color=source, size=asp_ug_g)) +
  facet_wrap(~comm_name, ncol=5) +
  geom_point() +
  # Labels
  labs(x="Date", y="Latitude (°N)") +
  # Theme
  theme_bw()


# Export
################################################################################

# Export
saveRDS(data, file=file.path(outdir, "CDPH_1991_2024_bivalve_domoic_data.Rds"))





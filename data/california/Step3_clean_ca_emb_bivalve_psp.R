
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(stringr)
library(tidyverse)

# Directories
indir <- "data/california/raw/emb_vanessa"
outdir <- "data/california/processed"
intdir <- "data/california/intermediate"

# Read data
data_orig1 <- readxl::read_excel(file.path(indir, "PSP_2020-2025-todate.xlsx"), na="")
data_orig2 <- readxl::read_excel(file.path(indir, "PSP_shellfish_EMB_2000-2010.xlsx"), na="")
data_orig3 <- readxl::read_excel(file.path(indir, "PSP_shellfish_EMB_2010-2019.xlsx"), na="")
data_orig4 <- readxl::read_excel(file.path(indir, "PSP_shellfish_EMB_before2000.xlsx"), na="")

# Read key
type_key <- readxl::read_excel(file.path(intdir, "sample_type_key_bivalve_psp.xlsx"))

# To do list:
# 1) Update based on Vanessa feedback
# 2) Develop GPS coordinates for sites missing GPS coordinates

# Setup
################################################################################

# Wild species
wild_species <- c("Basket cockle", "Bent nose clam", "Fat gaper clam", "Gaper clam", 
                  "Littleneck clam", "Manila clam", "Pismo clam", "Purple clam", 
                  "Razor clam", "Rock scallop", "Unidentified clam", "Washington clam")

# Inspect column names
colnames(data_orig1)
colnames(data_orig2)
colnames(data_orig3)
colnames(data_orig4)

# Merge
data_merged <- bind_rows(data_orig1, data_orig2, data_orig3, data_orig4)

# Format
data <- data_merged %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  rename(sample_id=srl_number,
         date=date_sampled,
         site=sample_site,
         lat_dd=latitude,
         long_dd=longitude,
         modifier=mod_psp_median,
         toxicity_ug_100g=psp_ug_100_g) %>% 
  # Format species
  mutate(species=recode(species,
                        "Clinocardium nuttalli" = "Clinocardium nuttallii", 
                        "Crassostrea sikamea" = "Magallana sikamea",
                        "Crassostrea gigas" = "Magallana gigas",
                        "Mytilus gallo/trossulus/edulis" = "Mytilus galloprovincialis/trossulus/edulis",
                        "Prototheca staminea" = "Leukoma staminea", 
                        "Sanguinolaria nuttallii" = "Nuttallia nuttallii",
                        "Tapes japonica" = "Ruditapes philippinarum",    
                        "Tresus nuttalli" = "Tresus nuttallii",       
                        "Unknown" = "Unknown spp.")) %>% 
  # Format longitude
  mutate(long_dd=abs(long_dd)*-1,
         long_dd=ifelse(long_dd<=-180, NA, long_dd)) %>% 
  # Add year and month
  mutate(year=lubridate::year(date),
         month=lubridate::month(date)) %>% 
  # Add common name, tissue, source
  left_join(type_key, by="sample_type") %>% 
  # Fill empty tissue
  mutate(tissue=ifelse(is.na(tissue), "not specified", tissue),
         tissue_use=recode(tissue, "not specified"="whole")) %>%
  # Fill some species names based on common names
  mutate(species=case_when(comm_name == "Unidentified clam" ~ "Clam spp.",
                           comm_name == "Unidentified mussel" ~ "Mussel spp.",
                           comm_name == "Unidentified oyster" ~ "Oyster spp.",
                           comm_name == "Sea/bay mussels" ~ "Mytilus galloprovincialis/edulis",
                           T ~ species)) %>% 
  # Fix some common names based on species names
  mutate(comm_name=case_when(species == "Mytilus galloprovincialis" ~ "Sea mussel",
                             species== "Mytilus galloprovincialis/trossulus/edulis" ~ "Sea/blue/bay mussel",
                             species == "Mytilus californianus" ~ "California mussel",
                             T ~ comm_name)) %>% 
  # Fill empty source
  mutate(source=ifelse(is.na(source), "not specified", source)) %>% 
  # Set source as wild for species without cultured/sentinel
  mutate(source_use=ifelse(source=="not specified" & comm_name %in% wild_species, "wild", source)) %>%
  # Format modifier
  # Do D and J mean N b/c all they toxicities are blank?
  # Does > actually mean < because all smallish toxicities (42, 43, 80)?
  # Is A a typo because all have toxicities?
  mutate(modifier=toupper(modifier),
         modifier=ifelse(is.na(modifier), "=", modifier)) %>% 
  # Recode "not detected" ("N") at limit of detection
  mutate(toxicity_ug_100g=ifelse(modifier=="N", 38, toxicity_ug_100g),
         modifier=recode(modifier, "N"="<")) %>%
  # Three sample ids (M12P00973, 81-0967-00, 96-0509-00) are duplicated for every county
  # Set to correct county then eliminate duplicates
  mutate(county=case_when(sample_id=="M12P00973" ~ "Humboldt", # 40.8202, -124.1331 (Humboldt)
                          sample_id=="81-0967-00" ~ "Marin", # 38.227, -122.9605 (Marin)
                          sample_id=="96-0509-00" ~ "San Diego", # 33.00587, -117.2753 (San Diego)
                          T ~ county)) %>% 
  unique() %>% 
  # Update common names for coastwide harmonization
  mutate(comm_name=recode(comm_name,
                          "Basket cockle"="Nuttall's cockle",
                          "Bent nose clam"="Bent-nose clam",
                          "Gaper clam"="Pacific gaper clam",
                          "Bay mussel"="Blue mussel",
                          "Sea mussel"="Mediterranean mussel", # In this file, sea mussel are Mytilus galloprovincialis
                          "Sea/bay mussels"="Mediterranean/blue mussels", 
                          "Sea/blue/bay mussel"="Mediterranean/Pacific blue/blue mussels")) %>% 
  # Arrange
  select(sample_id, year, month, date, 
         county, site, lat_dd, long_dd, 
         comm_name, species, sample_type, 
         tissue, tissue_use, source, source_use, 
         modifier, toxicity_ug_100g,
         everything()) %>% 
  # Remove blanks
  filter(!is.na(sample_id))

# Inspect
str(data)
freeR::complete(data)

# Sample id
freeR::which_duplicated(data$sample_id)

# Check species
#freeR::check_names(data$species)

# Sample type
table(data$sample_type)

# Modifiers - something crazy here
table(data$modifier)

# Average LOD - but you have to run without recoding N  
data %>% 
  filter(modifier=="<") %>% 
  pull(toxicity_ug_100g) %>% 
  mean()

# County
table(data$county)

# Species key
spp_key <- data %>% 
  count(species, comm_name)
freeR::which_duplicated(spp_key$species)
freeR::which_duplicated(spp_key$comm_name)

# Type key
sample_type_key <- data %>% 
  count(sample_type, comm_name, tissue, source)

table(data$source)
table(data$tissue)

# Site key
site_key <- data %>% 
  group_by(county, site) %>% 
  summarize(source=paste(sort(unique(source)), collapse=","),
            lat_dd=mean(lat_dd, na.rm=T),
            long_dd=mean(long_dd, na.rm=T)) %>% 
  ungroup()

sum(is.na(site_key$lat_dd))


# Plot data
################################################################################

# ggplot(data, aes(x=long_dd, y=lat_dd)) +
#   geom_point()

ggplot(data, aes(y=lat_dd,
                 x=date,
                 color=source, 
                 size=toxicity_ug_100g)) +
  geom_point()

ggplot(data, aes(y=lat_dd,
                 x=date,
                 color=comm_name, 
                 size=toxicity_ug_100g)) +
  geom_point()


# Export data
################################################################################

# Export
range(data$year, na.rm=T)
saveRDS(data, file=file.path(outdir, "CDPH_EMB_1962_2025_bivalve_psp_data.Rds"))


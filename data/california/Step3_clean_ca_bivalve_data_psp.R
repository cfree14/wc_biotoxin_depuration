
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
data_orig1 <- readxl::read_excel(file.path(indir, "PSP_2020-2025-todate.xlsx"), na="")
data_orig2 <- readxl::read_excel(file.path(indir, "PSP_shellfish_EMB_2000-2010.xlsx"), na="")
data_orig3 <- readxl::read_excel(file.path(indir, "PSP_shellfish_EMB_2010-2019.xlsx"), na="")
data_orig4 <- readxl::read_excel(file.path(indir, "PSP_shellfish_EMB_before2000.xlsx"), na="")

# Read key
type_key <- readxl::read_excel(file.path(indir, "sample_type_key_bivalve_psp.xlsx"))

# Looks like some blank species names
# Seperate sample tupe into tissue/source/common name
# Maybe more to do

# Setup
################################################################################

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
                        "Magallana sikamea" = "Magallana gigas", 
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
  # Fill empty source
  mutate(source=ifelse(is.na(source), "not specified", source)) %>%
  # Fill empty tissue
  mutate(tissue=ifelse(is.na(tissue), "not specified", tissue)) %>%
  # Fill some species names based on common names
  mutate(species=case_when(comm_name == "Unidentified clam" ~ "Clam spp.",
                           comm_name == "Unidentified mussel" ~ "Mussel spp.",
                           comm_name == "Unidentified oyster" ~ "Oyster spp.",
                           comm_name == "Sea/bay mussels" ~ "Mytilus galloprovincialis/edulis",
                           T ~ species)) %>% 
  # Fix some common names based on species names
  # mutate(comm_name=case_when(species == "Mytilus galloprovincialis" ~ "Sea mussel",
  #                            T ~ species))
  # Format modifier
  # Do D and J mean N b/c all they toxicities are blank?
  # Does > actually mean < because all smallish toxicities (42, 43, 80)?
  # Is A a typo because all have toxicities?
  mutate(modifier=toupper(modifier)) %>% 
  # Arrange
  select(sample_id, year, month, date, 
         county, site, lat_dd, long_dd, 
         comm_name, species, sample_type, tissue, source,
         modifier, toxicity_ug_100g,
         everything()) %>% 
  # Remove blanks
  filter(!is.na(sample_id))

# Inspect
str(data)
freeR::complete(data)

# Check species
#freeR::check_names(data$species)

# Sample type
table(data$sample_type)

# Modifiers - something crazy here
table(data$modifier)

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

# Plot data
################################################################################

# ggplot(data, aes(x=long_dd, y=lat_dd)) +
#   geom_point()

ggplot(data, aes(y=lat_dd,
                 x=date,
                 color=comm_name, 
                 size=toxicity_ug_100g)) +
  geom_point()


# Export data
################################################################################

# Export
range(data$year, na.rm=T)
saveRDS(data, file=file.path(outdir, "CDPH_1962_2025_bivalve_psp_data.Rds"))


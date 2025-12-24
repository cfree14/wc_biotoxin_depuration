
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
data_orig <- readxl::read_excel(file.path(indir, "DA_other_finfish.xlsx"))

# Read sample type key
type_key <- readxl::read_excel(file.path(indir, "sample_type_key_finfish.xlsx"))


# Format data
################################################################################

# Format data
data <- data_orig %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  rename(sample_id=srl_number,
         date=date_sampled,
         site=sample_site,
         lat_dd=latitude,
         long_dd=longitude, 
         nindiv=number_of_individuals,
         modifier=mod_asp,
         toxicity_ug_g=asp_ug_g) %>% 
  # Fix scientific names
  mutate(species=recode(species,
                        "Lampetra tridentata" = "Entosphenus tridentatus", 
                        "Seriola lalandi dorsalis" = "Seriola lalandi")) %>% 
  # Fill missing scientific names
  mutate(species=case_when(is.na(species) & grepl("sardine", tolower(sample_type)) ~ "Sardinops sagax",
                           is.na(species) & grepl("mackeral", tolower(sample_type)) ~ "Scomber japonicus",
                           is.na(species) & grepl("mackeral", tolower(sample_type)) ~ "Scomber japonicus",
                           is.na(species) & grepl("salmon", tolower(sample_type)) ~ "Oncorhynchus tshawytscha",
                           is.na(species) & grepl("shrimp", tolower(sample_type)) ~ "Pandalus jordani",
                           is.na(species) & grepl("squid", tolower(sample_type)) ~ "Doryteuthis opalescens",
                           T ~ species)) %>% 
  # Add year and month
  mutate(year=lubridate::year(date),
         month=lubridate::month(date)) %>% 
  # Format sample type
  left_join(type_key, by="sample_type") %>% 
  # Fill missing tissues
  mutate(tissue=ifelse(is.na(tissue), "not specified", tissue)) %>% 
  # Arrange
  select(sample_id, year, month, date, 
         county, site, lat_dd, long_dd, 
         comm_name, species, sample_type, tissue, nindiv,
         modifier, toxicity_ug_g,
         everything())
  
# Inspect
str(data)
freeR::complete(data)

# Check names
#freeR::check_names(data$species)

# Inspect more
table(data$county)
table(data$nindiv)
table(data$modifier)

# Species key
spp_key <- data %>% 
  count(comm_name, species)
freeR::which_duplicated(spp_key$comm_name)
freeR::which_duplicated(spp_key$species)


# Plot data
################################################################################

# Plot map
ggplot(data, aes(x=long_dd, y=lat_dd)) +
  geom_point()

# Plot time series
ggplot(data, aes(y=lat_dd,
                 x=date,
                 color=comm_name, 
                 size=toxicity_ug_g)) +
  geom_point()


# Export data
################################################################################

# Export
range(data$year)
saveRDS(data, file=file.path(outdir, "CDPH_1999_2025_finfish_domoic_data.Rds"))





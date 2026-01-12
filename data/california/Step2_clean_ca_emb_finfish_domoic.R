
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
data_orig <- readxl::read_excel(file.path(indir, "DA_other_finfish.xlsx"))

# Read sample type key
type_key <- readxl::read_excel(file.path(intdir, "sample_type_key_finfish.xlsx"))

# To do list
# 1) Fill missing coordinates based on block id
# 2) Understand if missing tissues can be filled
# 3) Understand species that aren't species-specific
# 4) Understand how these relate to the FDB data


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
                           is.na(species) & grepl("mackeral", tolower(sample_type)) ~ "Mackerel spp.", # Jack mackerel or chub mackerel?
                           is.na(species) & grepl("mackeral", tolower(sample_type)) ~ "Mackerel spp.",
                           is.na(species) & grepl("salmon", tolower(sample_type)) ~ "Salmon spp.", # Oncorhynchus tshawytscha
                           is.na(species) & grepl("shrimp", tolower(sample_type)) ~ "Shrimp spp.", # Pandalus jordani
                           is.na(species) & grepl("squid", tolower(sample_type)) ~ "Squid spp.", # Doryteuthis opalescens
                           T ~ species)) %>% 
  # Add year and month
  mutate(year=lubridate::year(date),
         month=lubridate::month(date)) %>% 
  # Format modifier
  mutate(modifier=ifelse(is.na(modifier), "=", modifier)) %>% 
  # Format sample type
  left_join(type_key, by="sample_type") %>% 
  # Fill missing tissues
  mutate(tissue=ifelse(is.na(tissue), "not specified", tissue)) %>% 
  # mutate(tissue_use=ifelse(tissue=="not specifided", "muscle", tissue)) %>% 
  # Update common names
  mutate(comm_name=recode(comm_name, 
                          "Sardine"="Pacific sardine",
                          "Grunion"="California grunion",
                          "Thornback ray"="Thornback guitarfish")) %>% 
  # Add source
  mutate(source="wild") %>% 
  # Arrange
  select(sample_id, year, month, date, 
         county, site, lat_dd, long_dd, 
         comm_name, species, sample_type, source, tissue, nindiv,
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
table(data$tissue)

# Tissue stats
tissue_stats <- data %>%
  count(comm_name, tissue) %>% 
  group_by(comm_name) %>% 
  mutate(perc=n/sum(n)) %>% 
  ungroup() %>% 
  mutate(tissue=factor(tissue, levels=c("head",
                                 "liver",
                                 "muscle",
                                 "viscera",
                                 "meat", 
                                 "whole", 
                                 "not specified")))

# Species key
spp_key <- data %>% 
  count(comm_name, species)
freeR::which_duplicated(spp_key$comm_name)
freeR::which_duplicated(spp_key$species)

ggplot(tissue_stats, aes(x=perc, y=comm_name, fill=tissue)) +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Percent of tests", y="") +
  scale_x_continuous(labels=scales::percent_format()) +
  # Legend
  scale_fill_manual(name="Tissue",
                    values=c(RColorBrewer::brewer.pal(6, "Set2"), "grey80")) +
  # Theme
  theme_bw()


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
saveRDS(data, file=file.path(outdir, "CDPH_EMB_1999_2025_finfish_domoic_data.Rds"))





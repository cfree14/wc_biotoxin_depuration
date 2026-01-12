
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

# Read EMB data
emb_fin_orig <- readRDS(file=file.path(outdir, "CDPH_EMB_1999_2025_finfish_domoic_data.Rds")) 
emb_biv_orig <- readRDS(file=file.path(outdir, "CDPH_EMB_1991_2025_bivalve_domoic_data.Rds"))

# Read FDB data
fdb_crab1_orig <- readRDS(file=file.path(outdir, "CDPH_FDB_2000_2015_crab_domoic_data.Rds")) 
fdb_crab2_orig <- readRDS(file=file.path(outdir, "CDPH_FDB_2015_2025_crab_domoic_data.Rds"))
fdb_lobster_orig <- readRDS(file=file.path(outdir, "CDPH_FDB_2023_lobster_domoic_data.Rds")) 
fdb_seafood_orig <- readRDS(file=file.path(outdir, "CDPH_FDB_2025_finfish_domoic_data.Rds"))

# TO DO
# Some are missing counties
# Some are missing lat/longs


# Prep data
################################################################################

# Column names
colnames(emb_biv_orig)
colnames(emb_fin_orig)
colnames(fdb_crab1_orig)

# EMB finfish
colnames(emb_fin_orig)
emb_fin <- emb_fin_orig %>% 
  # Rename
  rename(domoic_ppm=toxicity_ug_g) %>% 
  # Add
  mutate(source="wild",
         source_use="wild",
         tissue_use=tissue) %>% 
  # Select
  select(sample_id, date, 
         county, site, lat_dd, long_dd, 
         comm_name, species, 
         source, source_use, tissue, tissue_use,
         modifier, domoic_ppm)

# EMB bivalves
emb_biv <- emb_biv_orig %>% 
  # Rename
  rename(domoic_ppm=toxicity_ug_g) %>% 
  # Select
  select(sample_id, date, 
         county, site, lat_dd, long_dd, 
         comm_name, species, 
         source, source_use, tissue, tissue_use,
         modifier, domoic_ppm)

# FDB crabs, old
# NEEDS A REAL SAMPLE ID
fdb_crab1 <- fdb_crab1_orig %>% 
  # Rename
  rename(domoic_ppm=toxicity_ppm) %>% 
  # Add
  mutate(sample_id=1:nrow(.) %>% as.character(),
         source="wild",
         source_use=source,
         tissue_use=tissue,
         modifier="=") %>% 
  # Select
  select(sample_id, date, 
         county, site, lat_dd, long_dd, 
         comm_name, species, 
         source, source_use, tissue, tissue_use,
         modifier, domoic_ppm)

# FDB crabs, new
# NEEDS COUNTY
freeR::complete(fdb_crab2_orig)
fdb_crab2 <- fdb_crab2_orig %>% 
  # Rename
  rename(domoic_ppm=toxicity_ppm,
         site=block_id,
         modifier=toxicity_mod) %>%
  # Add
  mutate(source="wild",
         source_use=source) %>%
  # Select
  select(sample_id, date, 
         #county, 
         site, lat_dd, long_dd, 
         comm_name, species, 
         source, source_use, tissue, tissue_use,
         modifier, domoic_ppm)

# Lobster
# MISSING COUNTY
freeR::complete(fdb_lobster_orig)
fdb_lobster <- fdb_lobster_orig %>% 
  # Rename
  rename(domoic_ppm=viscera_ppm,
         site=block_id,
         modifier=viscera_mod) %>%
  # Add
  mutate(tissue="viscera",
         source="wild",
         source_use=source,
         tissue_use=tissue,
         site=as.character(site)) %>% 
  # Select
  select(sample_id, date, 
         #county, 
         site, lat_dd, long_dd, 
         comm_name, species, 
         source, source_use, tissue, tissue_use,
         modifier, domoic_ppm)


# Seafood
# MISSING COUNTY AND TISSUE
freeR::complete(fdb_seafood_orig)
fdb_seafood <- fdb_seafood_orig %>% 
  # Rename
  rename(domoic_ppm=toxicity_ppm,
         site=block_id) %>%
  # Add
  mutate(source="wild",
         tissue="not specified",
         source_use=source,
         tissue_use=tissue,
         site=as.character(site)) %>%
  # Select
  select(sample_id, date, 
         #county, 
         site, lat_dd, long_dd, 
         comm_name, species, 
         source, source_use, tissue, tissue_use, 
         modifier, domoic_ppm)


# Merge data
################################################################################

# Merge data
data <- bind_rows(emb_fin, emb_biv, fdb_crab1, fdb_crab2, fdb_lobster, fdb_seafood) %>% 
  mutate(year=lubridate::year(date), 
         month=lubridate::month((date))) %>% 
  relocate(year, .before=date) %>% 
  relocate(month, .after=year)

# Inspect
str(data)
freeR::complete(data)

# Species key
spp_key <- data %>% 
  count(comm_name, species)
freeR::which_duplicated(spp_key$comm_name)
freeR::which_duplicated(spp_key$species)

# Inspect
table(data$county)
table(data$modifier)
table(data$tissue)
table(data$source)

# Plot
ggplot(data, aes(x=date, y=lat_dd, color=comm_name, size=domoic_ppm)) +
  geom_point() + 
  theme_bw()


# Export data
################################################################################

# Export
range(data$year, na.rm = T)
saveRDS(data, file=file.path(outdir, "CDPH_1991_2025_all_domoic_data.Rds"))



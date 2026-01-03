
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(stringr)
library(tidyverse)

# Directories
cadir <- "data/california/processed"
ordir <- "data/oregon/processed"
wadir <- "data/washington/processed"
outdir <- "data/merged/processed"

# Read CA data
ca_orig <- readRDS(file=file.path(cadir, "CDPH_1991_2025_all_domoic_data.Rds"))

# Read WA data
wa_orig <- readRDS(file=file.path(wadir, "WDOH_1957_2025_biotoxin_data.Rds"))

# Read OR data
or_da_orig <- readRDS(file=file.path(ordir, "ODA_1999_2025_domoic_data.Rds"))
or_psp_orig <- readRDS(file=file.path(ordir, "ODA_1999_2025_psp_data.Rds"))


# Format data
################################################################################

# Format CA data
ca <- ca_orig %>% 
  # Add state
  mutate(state="California") %>% 
  # Simplify
  select(state, sample_id, year, month, date, 
         site, lat_dd, long_dd, 
         comm_name, species, 
         tissue, source, #tissue_use, source_use,
         modifier, toxicity_ug_g) %>% 
  # Rename
  rename(toxicity_ppm=toxicity_ug_g)

# Format OR data
or <- or_da_orig %>% 
  # Add state
  mutate(state="Oregon",
         source="not specified") %>% 
  # Simplify
  select(state, sample_id, year, month, date, 
         site, #lat_dd, long_dd, 
         comm_name, species, tissue, source, 
         modifier, toxicity_ppm)

# Format WA data
wa <- wa_orig %>% 
  # Add state
  mutate(state="Washington",
         source="not specified") %>% 
  # Reduce to domoic
  filter(!is.na(da_id)) %>% 
  # Simplify
  select(state, da_id, year_collected, month_collected, date_collected, 
         site, lat_dd, long_dd, 
         comm_name, species, 
         da_tissue, source, 
         da_result) %>% 
  # Rename
  rename(sample_id=da_id,
         year=year_collected,
         month=month_collected,
         date=date_collected,
         tissue=da_tissue,
         toxicity_ppm=da_result) %>% 
  # Remove toxicity
  select(-toxicity_ppm)



# Merge
################################################################################

# Merge
data <- bind_rows(ca, or, wa) %>% 
  # Format tissue
  mutate(tissue=tolower(tissue))

# Inspect
str(data)
freeR::complete(data)

# ID unique?
freeR::which_duplicated(data$sample_id)

# Tissue
table(data$tissue)

# Source
table(data$source)

# Modifier
table(data$modifier)

# Species key
spp_key <- data %>% 
  count(comm_name, species)
freeR::which_duplicated(spp_key$comm_name)
freeR::which_duplicated(spp_key$species)

# Plot
ggplot(data, aes(x=date, y=lat_dd, color=comm_name)) +
  geom_point() +
  # Legend
  scale_color_discrete(guide="none") +
  # Theme
  theme_bw()


################################################################################

# Export
# save(data, file=file.path(outdir, "WC_domoic_acid_data.Rdata"))



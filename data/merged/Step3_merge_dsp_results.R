
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

# Read WA data
wa_orig <- readRDS(file=file.path(wadir, "WDOH_1957_2025_biotoxin_data.Rds"))

# Read OR data
or_orig <- readRDS(file=file.path(ordir, "ODA_1999_2025_dsp_data_gps.Rds"))


# Format data
################################################################################

# Format OR data
or <- or_orig %>% 
  # Add state
  mutate(state="Oregon",
         source="not specified", 
         source_use=source,
         tissue_use=tissue) %>% 
  # Simplify
  select(state, sample_id, year, month, date, 
         site, lat_dd, long_dd, 
         comm_name, species, 
         tissue, tissue_use, source, source_use, 
         modifier, toxicity_ug_100g)
freeR::complete(or)

# Format WA data
wa <- wa_orig %>% 
  # Reduce
  filter(state=="Washington" & !is.na(dsp_id)) %>% 
  # Rename
  rename(sample_id=da_id,
         year=year_collected,
         month=month_collected,
         date=date_collected,
         tissue=dsp_tissue,
         modifier=dsp_modifier,
         toxicity_ug_100g=dsp_result) %>% 
  # Add
  mutate(source="not specified",
         source_use=source,
         tissue_use=tissue) %>% 
  # Simplify
  select(state, sample_id, year, month, date, 
         site, lat_dd, long_dd, 
         comm_name, species, 
         tissue, tissue_use, source, source_use, 
         modifier, toxicity_ug_100g)
freeR::complete(wa)


# Merge
################################################################################

# Merge
data <- bind_rows(wa, or)

# Inspect
str(data)
freeR::complete(data)

# ID unique?
freeR::which_duplicated(data$sample_id)

# Tissue
table(data$tissue)
table(data$tissue_use)

# Source
table(data$source)
table(data$source_use)

# Modifier
table(data$modifier)

# Species key
spp_key <- data %>% 
  count(comm_name, species)
freeR::which_duplicated(spp_key$comm_name)
freeR::which_duplicated(spp_key$species)

# Plot
ggplot(data %>% filter(year>=2020), aes(x=date, y=lat_dd, color=comm_name, size=toxicity_ug_100g)) +
  geom_point() +
  # Legend
  scale_color_discrete(guide="none") +
  # Theme
  theme_bw()


################################################################################

# Export
saveRDS(data, file=file.path(outdir, "WC_dsp_data.Rds"))



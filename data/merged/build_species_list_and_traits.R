
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(stringr)
library(tidyverse)

# Directories
outdir <- "data/merged/processed"
tabledir <- "tables"
plotdir <- "figures"
keydir <- "data/merged/species_key"


# Read data
data_psp_orig <- readRDS(file=file.path(outdir, "WC_psp_data.Rds"))
data_asp_orig <- readRDS(file=file.path(outdir, "WC_domoic_acid_data.Rds"))
data_dsp_orig <- readRDS(file=file.path(outdir, "WC_dsp_data.Rds"))


# Merge data to help build key
################################################################################

# PST
data_psp <- data_psp_orig %>% 
  # Add toxin
  mutate(toxin="PST") %>% 
  # Simplify
  select(toxin, state, comm_name, species, source_use)

# Domoic
data_da <- data_asp_orig %>% 
  # Add toxin
  mutate(toxin="DA") %>% 
  # Simplify
  select(toxin, state, comm_name, species, source_use)

# DST
data_dsp <- data_dsp_orig %>% 
  # Add toxin
  mutate(toxin="DST") %>% 
  # Simplify
  select(toxin, state, comm_name, species, source_use)

# Merge
data <- bind_rows(data_psp, data_da, data_dsp)


# Summarize into species key
################################################################################

# Build key
key <- data %>% 
  # Abbreviate states pre-summary
  mutate(state=recode(state,
                      "California" = "CA",
                      "Oregon" = "OR",
                      "Washington" = "WA")) %>% 
  # Summarize
  group_by(comm_name, species) %>% 
  summarize(n=n(), 
            toxins=paste(unique(toxin), collapse=", "),
            states=paste(unique(state), collapse=", "),
            sources=paste(unique(source_use[source_use!="not specified"]), collapse=", ")) %>% 
  ungroup() %>% 
  # Remove blank species
  filter(!is.na(comm_name))

# Export
write.csv(key, file=file.path(keydir, "species_key1.csv"))


# Get lat range info
################################################################################
# 
# # Species
# spp_do <- key$species
# 
# # Retrieve FB info
# fb <- rfishbase::distribution(spp_do)
# 
# # Format FB info
# fb1 <- fb %>% 
#   # Simplify
#   select(Species, SouthernLatitude, SouthernLatitudeNS, NorthernLatitude, NorthernLatitudeNS) %>% 
#   filter(!is.na(SouthernLatitude)) %>% 
#   # Rename
#   rename(species=Species) %>% 
#   # Add
#   mutate(lat_dd_s=ifelse(SouthernLatitudeNS=="S", SouthernLatitude*-1, SouthernLatitude),
#          lat_dd_n=ifelse(NorthernLatitudeNS=="S", NorthernLatitude*-1, NorthernLatitude)) %>% 
#   # Summarize
#   group_by(species) %>% 
#   summarize(lat_dd_s=min(lat_dd_s),
#             lat_dd_n=max(lat_dd_n)) %>% 
#   ungroup()
# 
# # Retrieve SLB info
# slb <- rfishbase::distribution(spp_do, server="sealifebase")
# 
# # Format FB info
# slb1 <- slb %>% 
#   # Simplify
#   select(Species, SouthernLatitude, SouthernLatitudeNS, NorthernLatitude, NorthernLatitudeNS) %>% 
#   filter(!is.na(SouthernLatitude)) %>% 
#   # Rename
#   rename(species=Species) %>% 
#   # Add
#   mutate(lat_dd_s=ifelse(SouthernLatitudeNS=="S", SouthernLatitude*-1, SouthernLatitude),
#          lat_dd_n=ifelse(NorthernLatitudeNS=="S", NorthernLatitude*-1, NorthernLatitude)) %>% 
#   # Summarize
#   group_by(species) %>% 
#   summarize(lat_dd_s=min(lat_dd_s),
#             lat_dd_n=max(lat_dd_n)) %>% 
#   ungroup()
# 
# # Merge
# lats <- bind_rows(fb1, slb1)
# 
# # Add to key
# data <- spp_key %>% 
#   left_join(lats)
# 
# ggplot(data, aes(x=lat_dd_s, xend=lat_dd_n, y=reorder(comm_name, lat_dd_n))) +
#   geom_segment() +
#   # Ref lines
#   geom_vline(xintercept=c(32, 42, 46, 50)) +
#   # Labels
#   labs(x="Latitude (°N)", y="") +
#   # Theme
#   theme_bw()
# 
# write.csv(spp_key, file = "~/Desktop/key.csv")
# 
# # Look up species
# ################################################################################
# 
# # Species
# scinames <- spp_key$species
# 
# 
# 
# 
# 

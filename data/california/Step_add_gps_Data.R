



# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(stringr)
library(tidyverse)

# Directories
indir <- "data/california/raw_christina"
outdir <- "data/california/processed"
intdir <- "data/california/intermediate"

# Read data
data_orig <- readRDS(file=file.path(outdir, "CDPH_2015_2025_crab_domoic_data.Rds"))


# Coordinate key
################################################################################

reorder_coords <- function(x) {
  parts <- str_split(x, "\\s+", simplify = TRUE)
  needs_swap <- str_detect(parts[, 1], "^(122|123|124)")
  parts[needs_swap, ] <- parts[needs_swap, c(3, 4, 1, 2)]
  str_c(parts[,1], parts[,2], parts[,3], parts[,4], sep = " ")
}


# Isolate coordiantes
coords <- data_orig %>% 
  # Count
  count(coords) %>% 
  rename(coords_orig=coords) %>% 
  # Remove spaces
  mutate(coords=stringr::str_squish(coords_orig)) %>%
  # Remove non-coordinats
  mutate(coords=ifelse(coords %in% c("n/a - Block 657", "not avail / SB Operation", "Doran Beach"), NA, coords)) %>% 
  # Pick first of coordinates with multiple
  mutate(coords=recode(coords,
                       "39 00.42 -123 44.772 and   39 01.25 -123 46.779"="39 00.42 -123 44.772",
                       "37 51.760-122 42.785"="37 51.760 -122 42.785")) %>% 
  # Remove letters
  mutate(coords=gsub("N|W|'|’|°", "", coords)) %>% 
  # Move negative up against number
  mutate(coords=gsub("- 120", "-120", coords),
         coords=gsub("- 124", "-124", coords)) %>% 
  # Add in negatives where possible
  mutate(coords=gsub("/ ", "-", coords),
         coords=gsub(", 120", " -120", coords),
         coords=gsub(", 122", " -122", coords),
         coords=gsub(", 123", " -123", coords),
         coords=gsub(" 120", " -120", coords),
         coords=gsub(" 123", " -123", coords),
         coords=gsub(" 124", " -124", coords)) %>% 
  # Split
  separate(coords, into=c("lat_dd", "long_dd"), sep=" -", remove=F)

# Export
openxlsx::write.xlsx(coords, file="~/Desktop/coords.xlsx")
  
  
  
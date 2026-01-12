

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(stringr)
library(tidyverse)
library(plotly)

# Directories
indir <- "data/california/raw/fdb_christina"
outdir <- "data/california/processed"
intdir <- "data/california/intermediate"

# Blocks
blocks <- wcfish::blocks %>% 
  mutate(block_id=as.character(block_id))

# Sheets
sheets <- readxl::excel_sheets(file.path(indir, "PRAR_DA_2015-2025_12_19_25.xlsx"))
sheets

# Read data
lobster23_orig <- readxl::read_excel(file.path(indir, "PRAR_DA_2015-2025_12_19_25.xlsx"), sheet="Lobster 2023-2024", na="n/a") 

# To do
# DONE

# Helper function
################################################################################

# Convert DM to DD
conv_dm_to_dd <- function(x) {
  # Vectorized converter: "DD MM.mmm" -> decimal degrees
  # Handles NAs, extra spaces, and optional leading +/- sign.
  
  x <- trimws(x)
  out <- rep(NA_real_, length(x))
  
  ok <- !is.na(x) & nzchar(x)
  if (!any(ok)) return(out)
  
  # Split on whitespace (one or more spaces/tabs)
  parts <- strsplit(x[ok], "\\s+")
  
  # Expect at least 2 tokens: degrees and minutes
  deg <- suppressWarnings(as.numeric(vapply(parts, `[`, character(1), 1)))
  min <- suppressWarnings(as.numeric(vapply(parts, `[`, character(1), 2)))
  
  # Preserve sign on degrees (e.g., "-33 42.087" or "+33 42.087")
  sign_deg <- ifelse(is.na(deg), NA_real_, ifelse(deg < 0, -1, 1))
  deg_abs  <- abs(deg)
  
  dd <- sign_deg * (deg_abs + (min / 60))
  
  # If parsing failed, dd will be NA
  out[ok] <- dd
  out
}


# Format lobster data
################################################################################

# Format data
lobster <- lobster23_orig %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  rename(sample_id=is_number,
         comm_name=species_viscera, 
         block_id=block_number,
         depth_fa=depth_fathoms,
         coords=lat_long_coordinates,
         area=collection_sites,
         date=date_of_catch) %>% 
  # Format common name
  mutate(comm_name=stringr::str_to_sentence(comm_name),
         comm_name=recode(comm_name, "Spiny lobster"="California spiny lobster"), 
         species="Panulirus interruptus") %>% 
  # Split coordinates
  separate(coords, sep="-", into=c("lat_dd", "long_dd"), remove=F) %>% 
  mutate(lat_dd=conv_dm_to_dd(lat_dd),
         long_dd=conv_dm_to_dd(long_dd)*-1) %>% 
  # Format toxicity
  mutate(viscera_mod=ifelse(grepl("<", viscera_ppm), "<", "="),
         viscera_ppm=gsub("<", "", viscera_ppm) %>% as.numeric(.),
         meat_mod=ifelse(grepl("<", meat_ppm), "<", "="),
         meat_ppm=gsub("<", "", meat_ppm) %>% as.numeric(.),
         roe_mod=ifelse(grepl("<", roe_ppm), "<", "="),
         roe_ppm=gsub("<", "", roe_ppm) %>% as.numeric(.)) %>% 
  # Arrange
  select(sample_id, date,
         port, area, block_id, coords, lat_dd, long_dd, depth_fa,
         comm_name, species, 
         viscera_mod, viscera_ppm, 
         meat_mod, meat_ppm, 
         roe_mod, roe_ppm,
         everything())

# Inspect
str(lobster)
freeR::complete(lobster)

# Inspect more
range(lobster$date)
table(lobster$port)
table(lobster$area)
table(lobster$block_id)
table(lobster$coords)
table(lobster$comm_name)

# Export
saveRDS(lobster, file.path(outdir, "CDPH_FDB_2023_lobster_domoic_data.Rds"))


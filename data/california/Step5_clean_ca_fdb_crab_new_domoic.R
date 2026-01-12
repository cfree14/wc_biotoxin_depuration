

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
crab15_orig <- readxl::read_excel(file.path(indir, "PRAR_DA_2015-2025_12_19_25.xlsx"), sheet="Crab 2015-2017", col_types = "text") 
crab17_orig <- readxl::read_excel(file.path(indir, "PRAR_DA_2015-2025_12_19_25.xlsx"), sheet="Crab 2017-2018", col_types = "text") 
crab18_orig <- readxl::read_excel(file.path(indir, "PRAR_DA_2015-2025_12_19_25.xlsx"), sheet="Crab 2018-2019", col_types = "text") 
crab19_orig <- readxl::read_excel(file.path(indir, "PRAR_DA_2015-2025_12_19_25.xlsx"), sheet="Crab 2019-2020", col_types = "text") 
crab20_orig <- readxl::read_excel(file.path(indir, "PRAR_DA_2015-2025_12_19_25.xlsx"), sheet="Crab 2020-2021", col_types = "text") 
crab21_orig <- readxl::read_excel(file.path(indir, "PRAR_DA_2015-2025_12_19_25.xlsx"), sheet="Crab 2021-2022", col_types = "text") 
crab22_orig <- readxl::read_excel(file.path(indir, "PRAR_DA_2015-2025_12_19_25.xlsx"), sheet="Crab 2022-2023", col_types = "text") 
crab23_orig <- readxl::read_excel(file.path(indir, "PRAR_DA_2015-2025_12_19_25.xlsx"), sheet="Crab 2023-2024", col_types = "text") 
crab24_orig <- readxl::read_excel(file.path(indir, "PRAR_DA_2015-2025_12_19_25.xlsx"), sheet="Crab 2024-2025", col_types = "text") 
crab25_orig <- readxl::read_excel(file.path(indir, "PRAR_DA_2015-2025_12_19_25.xlsx"), sheet="Crab 2025-2026", col_types = "text") 

# To do
# DONE.- unless you ever wanted to clean up depth

# Dcrab coordinate key
################################################################################

# Read Dcrab coordinate key
coord_key_orig <- readxl::read_excel(file.path(intdir, "coord_key_dcrab.xlsx")) 

# Build key
coord_key <- coord_key_orig %>% 
  # Add zeroes
  mutate(lat_m=ifelse(is.na(lat_m), 0, lat_m),
         lat_s=ifelse(is.na(lat_s), 0, lat_s),
         long_m=ifelse(is.na(long_m), 0, long_m),
         long_s=ifelse(is.na(long_s), 0, long_s)) %>% 
  # Calculate lat
  mutate(lat_dd=lat_d+lat_m/60+lat_s/3600,
         long_dd=long_d+long_m/60+long_s/3600,
         long_dd=long_dd*-1)
  
# Inspect
str(coord_key)

# Plot data
p <- ggplot(coord_key, aes(x=long_dd, y=lat_dd, text=coords)) +
  geom_point() +
  theme_bw()
ggplotly(p, tooltip = "text") # can't see why the 37.50.58 isn't 37+50.58/60


# Format crab data
################################################################################

# Merge crab
crab_orig <- bind_rows(crab15_orig,
                       crab17_orig,
                       crab18_orig,
                       crab19_orig,
                       crab20_orig,
                       crab21_orig,
                       crab22_orig,
                       crab23_orig,
                       crab24_orig,
                       crab25_orig)

# Format crab
crab <- crab_orig %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  rename(sample_id=is_number,
         comm_name=species, 
         block_id=block_number,
         depth_fa=depth_fathoms,
         coords=lat_long_coordinates,
         toxicity_ppm=result_ppm,
         toxicity_ppm2=result_ppm_fda_action_30,
         area2=collection_sites,
         date2=date_of_catch, 
         comm_name2=species_viscera) %>% 
  # Merge columns
  mutate(date=ifelse(!is.na(date), date, date2),
         area=ifelse(!is.na(area), area, area2),
         toxicity_ppm=ifelse(!is.na(toxicity_ppm), toxicity_ppm, toxicity_ppm2),
         comm_name=ifelse(!is.na(comm_name), comm_name, comm_name2)) %>% 
  # Remove unnecessary columns
  select(-c(date2, toxicity_ppm2, comm_name2, area2)) %>% 
  # Format date
  mutate(date=as.numeric(date) %>% as.Date(., origin = "1899-12-30") %>% lubridate::ymd(.)) %>% 
  # Fix an incorrect Bodega Bay-Salt Point date in 2016. It just reads "11" in the spreadsheet
  # Its a day in Nov 2016 and sampling occurred on 11/12/2016 elsewhere
  mutate(date=case_when(date=="1900-01-10" ~ ymd("2016-11-12"),
                        T ~ date)) %>% 
  # Format common name
  mutate(comm_name=stringr::str_to_sentence(comm_name),
         comm_name=recode(comm_name,
                          "Spider crab" = "Sheep crab")) %>% 
  # Add species
  mutate(species=recode(comm_name, 
                        "Dungeness crab" = "Metacarcinus magister",
                        "Rock crab" = "Cancer spp.",
                        "Sheep crab" = "Loxorhynchus grandis")) %>% 
  # Format port
  mutate(port=recode(port,
                     "Trindad"="Trinidad",
                     "Ft. Bragg"="Fort Bragg",
                     "Cresecent City"="Crescent City",
                     "Half Moon Bay/SF"="Half Moon Bay/San Francisco")) %>% 
  # Format depth
  mutate(depth_fa=recode(depth_fa,
                         "Unk"="unknown"),
         depth_fa=case_when(is.na(depth_fa) ~ "unknown",
                         T ~ depth_fa)) %>% 
  # Format toxicity
  mutate(toxicity_ppm=toupper(toxicity_ppm) %>% stringr::str_squish(.),
         toxicity_ppm=recode(toxicity_ppm,
                             "< 2.5"="<2.5",
                             "ND"="<2.5"),
         toxicity_mod=ifelse(grepl("<", toxicity_ppm), "<", "="),
         toxicity_ppm=gsub("<", "", toxicity_ppm) %>% as.numeric(.)) %>% 
  # Add lat/long based on blocks
  left_join(blocks %>% select(block_id, block_long_dd, block_lat_dd), by="block_id") %>% 
  # Add lat/long based on coordinate text
  left_join(coord_key %>% select(coords, lat_dd, long_dd), by="coords") %>% 
  # Fill missing coordinates with block centers
  mutate(lat_dd=ifelse(is.na(lat_dd), block_lat_dd, lat_dd),
         long_dd=ifelse(is.na(long_dd), block_long_dd, long_dd)) %>% 
  # Add tissue
  mutate(tissue="not specified",
         tissue_use="viscera") %>% 
  # Unique
  # Lots of records were duplicated when you look at the sample id
  # However, because eliminating duplicates does not address the whole problem
  # I'm going to assume its an error with the sample id rather than perfectly duplicated rows
  # (i.e., a test could have the same results and look identical if the sample ids are sloppy)
  # unique() %>% 
  # Add a truly unique id
  rename(sample_id_orig=sample_id) %>% 
  mutate(sample_id=make.unique(sample_id_orig)) %>% 
  # Because some original sample ids are blank, 
  # the first blank sample id is simply NA, recode it as NA.0
  mutate(sample_id=ifelse(is.na(sample_id), "NA.0", sample_id)) %>% 
  # Arrange
  select(sample_id, sample_id_orig, date,
         port, area, 
         block_id, block_lat_dd, block_long_dd,
         depth_fa, coords, lat_dd, long_dd,
         comm_name, species, tissue, tissue_use, toxicity_mod, toxicity_ppm,
         everything())
  
# Inspect
str(crab)
freeR::complete(crab)

# Sample id unique?
# Original sample id is imperfect
freeR::which_duplicated(crab$sample_id_orig)

# New sample id
freeR::which_duplicated(crab$sample_id)

# Date
range(crab$date)

# Port
table(crab$port)

# Common name
table(crab$comm_name)

# Block id
table(crab$block_id)

# Depth
table(crab$depth_fa) # meh, I don't care that much

# Toxicity
sort(unique(crab$toxicity_ppm))

# Are any sites with coordinate text missing lat/long?
crab_coords <- sort(unique(crab$coords))
crab_coords[!crab_coords %in% coord_key$coords] %>% unique()

# Export
saveRDS(crab, file=file.path(outdir, "CDPH_FDB_2015_2025_crab_domoic_data.Rds"))


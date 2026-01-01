
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(stringr)
library(tidyverse)

# Directories
indir <- "data/oregon/raw"
outdir <- "data/oregon/processed"

# Read data
list.files(indir)
clams_orig1 <- readxl::read_excel(file.path(indir, "clams.xlsx"))
clams_orig2 <- readxl::read_excel(file.path(indir, "Clams.2015 to present.xlsx")) 
crab_orig <- readxl::read_excel(file.path(indir, "Crab.xlsx"))
mussels_orig1 <- readxl::read_excel(file.path(indir, "Mussels.xlsx"))
mussels_orig2 <- readxl::read_excel(file.path(indir, "Mussels.2015 to present.xlsx"))

# Read key
sample_key_crab <- readxl::read_excel(file.path(indir, "sample_key_crab.xlsx"))
site_key_crab <- readxl::read_excel(file.path(indir, "site_key_crab.xlsx"))

# Other Qs for Alex: tissue for bivalves?
# Are the bivalves all wild? Are some famred?

# Clean crab
################################################################################

# To do list:
# Add coordinates

# Questions
# Are coords avaiable?
# What is tissue if not provided?
# What is cooked if not provided?

# Clean crab
crab <- crab_orig %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  rename(site_orig=results_product_date_location_shellfish_extract_location_name,
         subsite=results_product_sublocation_shellfish_extract_station_name,
         sample_type=results_product_shellfish_extract_product_desc,
         date=z_sample_date_d,
         time=results_product_shellfish_extract_time_sampled,
         toxin=analyte_name,
         modifier=quantitative_operator,
         toxicity=quantity,
         toxicity_units=quantitative_unit ,
         toxicity_long=z_analyte_comment_ct) %>% 
  # Remove useless
  select(-subsite) %>% 
  # Convert
  mutate(toxicity=as.numeric(toxicity)) %>% 
  # Format sample type
  mutate(sample_type=toupper(sample_type)) %>% 
  # Add sample key metadata
  left_join(sample_key_crab, by="sample_type") %>% 
  # Use info in TOXIN column to alter tisse
  mutate(tissue=ifelse(toxin=="Domoic Acid in Body Meat", "leg meat", tissue)) %>% 
  # Format toxin
  mutate(toxin=recode(toxin, 
                      "Domoic Acid"="Domoic acid",
                      "Domoic Acid in Body Meat"="Domoic acid",
                      "INELIGIBLE FOR ANALYSIS"="Ineligible")) %>%
  # Recode "ORIGINAL REPORT DATA" with UNITS = "ppm" as domoic acid
  mutate(toxin=case_when(toxin=="Original Report Data" & toxicity_units=="ppm" ~"Domoic acid",
                         T ~ toxin)) %>% 
  # Add site metadata
  left_join(site_key_crab, by="site_orig") %>% 
  # Overwrite site with area (which was extracted from sample column) if site == General History
  mutate(site=ifelse(site=="General History" & !is.na(area), area, site)) %>% 
  select(-area) %>% 
  # Fill modifier
  mutate(modifier=ifelse(is.na(modifier), "=", modifier)) %>% 
  # Add species name
  mutate(comm_name="Dungeness crab",
         species="Metacarcinus magister") %>% 
  # Arrange
  select(site_orig, site, 
         date, time, 
         comm_name, species, sample_type, tissue, cooked_yn, 
         toxin, modifier, toxicity, toxicity_units, toxicity_long, 
         everything())

# Inspect
str(crab)
freeR::complete(crab)

# Sites
table(crab$site)

# Toxin
table(crab$toxin)

# Tissue
table(crab$tissue)

# Modifier
table(crab$modifier)

# Units
table(crab$toxicity_units)


# Clean clams
################################################################################

# Clean clams
clams1 <- clams_orig1 %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  rename(date=sampled_date,
         site=collection_site,
         da_modifier=vari_for_da,
         da=domoic_acid,
         psp_modifier=vari_for_psp,
         psp=psp_toxins) %>% 
  # Add 
  mutate(comm_name="Unspecified clam",
         species="Clam spp.",
         tissue="whole")

# Inspect
str(clams1)
freeR::complete(clams1)

# Modifiers
table(clams1$da_modifier)
table(clams1$psp_modifier)

# Clean clams
clams2 <- clams_orig2 %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  rename(comm_name=species,
         site=location,
         time=results_product_shellfish_extract_time_sampled,
         toxin=analyte_name,
         modifier=quantitative_operator,
         toxicity=quantity,
         toxicity_units=quantitative_unit ,
         toxicity_long=z_analyte_comment_ct) %>% 
  # Convert to numeric
  mutate(toxicity=as.numeric(toxicity)) %>% 
  # Format common name
  mutate(comm_name=stringr::str_to_sentence(comm_name),
         comm_name=recode(comm_name,
                          "Butter clams" = "Butter clam",          
                          "Clams" = "Unspecified clam",                   
                          "Clams, butter" = "Butter clam",           
                          "Clams, cockle" = "Nutall's cockle",           
                          "Clams, gaper" = "Gaper clam",           
                          "Clams, purple varnish" = "Purple varnish clam",   
                          "Clams, razor" = "Razor clam",           
                          "Clams, softshell" = "Softshell clam",        
                          "Clams,razor" = "Razor clam",             
                          "Clams. Razor" = "Razor clam",           
                          "Cockle clams" = "Nutall's cockle",           
                          "Eastern thinshell clams" = "Softshell clam",
                          "Gaper clams" = "Gaper clam",             
                          "Littleneck clams" = "Littleneck clam",        
                          "Purple varnish clams" = "Purple varnish clam",   
                          "Razor clams" = "Razor clam",             
                          "Rzor clams" = "Razor clam",             
                          "Thinshell clams" = "Softshell clam",        
                          "Varnish clams" = "Purple varnish clam")) %>% 
  # Add species
  mutate(species=recode(comm_name,
                        "Butter clam" = "Saxidomus gigantea",       
                        "Gaper clam" = "Tresus capax",                 
                        "Littleneck clam" = "Leukoma staminea",            
                        "Nutall's cockle" = "Clinocardium nuttallii",           
                        "Purple varnish clam" = "Nuttallia obscurata",        
                        "Razor clam" = "Siliqua patula",                
                        "Softshell clam" = "Mya arenaria",            
                        "Unspecified clam" = "Clam spp.")) %>% 
  # Fill modifier
  mutate(modifier=ifelse(is.na(modifier), "=", modifier)) %>% 
  # Format site
  mutate(site=gsub(" Biotoxins", "", site)) %>% 
  # Format toxin
  mutate(toxin=recode(toxin, 
                      "Domoic Acid" = "Domoic acid",
                      "INELIGIBLE FOR ANALYSIS" = "Ineligible",
                      "NSSP Domoic Acid" = "Domoic acid")) %>% 
  # Add tissue
  mutate(tissue="whole") %>% 
  # Arrange
  select(date, time, site, comm_name, species, tissue,
         toxin, modifier, toxicity, toxicity_units, toxicity_long, everything())

# Inspect
str(clams2)
freeR::complete(clams2)

# Q for Alex: NSSP Domoic Acid vs. Domoic acid

# Common name
# https://www.dfw.state.or.us/mrp/shellfish/bayclams/clamid.asp
# I think thinshell are softshell - check with Alex
sort(unique(clams2$comm_name))
#freeR::check_names(clams2$species)

# Site
sort(unique(clams2$site))

# Toxin
table(clams2$toxin)

# Units
table(clams2$toxicity_units)

# Modifier
table(clams2$modifier)


# Clean mussels
################################################################################

# Clean mussels 1
mussels1 <- mussels_orig1 %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  rename(date=sampled_date,
         site=collection_site,
         da_modifier=vari_for_da,
         da=domoic_acid,
         psp_modifier=vari_for_psp,
         psp=psp_toxins) %>% 
  # Add
  mutate(comm_name="Unspecified mussel",
         species="Mussel spp.",
         tissue="whole")

# Inspect
str(mussels1)
freeR::complete(mussels1)
table(mussels1$site)

# Clmussels1# Clean mussels
mussels2 <- mussels_orig2 %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  rename(site=location,
         time=results_product_shellfish_extract_time_sampled,
         toxin=analyte_name,
         modifier=quantitative_operator,
         toxicity=quantity,
         toxicity_units=quantitative_unit ,
         toxicity_long=z_analyte_comment_ct,
         comm_name=species) %>% 
  # Convert
  mutate(toxicity=as.numeric(toxicity)) %>% 
  # Format toxin
  mutate(toxin=recode(toxin,
                      "Domoic Acid"="Domoic acid",
                      "NSSP Domoic Acid"="Domoic acid",
                      "INELIGIBLE FOR ANALYSIS"="Ineligible")) %>% 
  # Clean species
  mutate(comm_name=stringr::str_to_sentence(comm_name),
         comm_name=recode(comm_name, 
                          "Mussels" = "Unspecified mussel",
                          "Ca mussels"="California mussel",
                          "California mussels"="California mussel")) %>%
  # Add species
  mutate(species=recode(comm_name,
                        "California mussel" = "Mytilus californianus",
                        "Unspecified mussel" = "Mussel spp.")) %>% 
  # Clean site
  mutate(site=gsub(" Biotoxins", "", site)) %>% 
  # Add tissue
  mutate(tissue="whole")

# Inspect
str(mussels2)
freeR::complete(mussels2)

# Inspect
sort(unique(mussels2$site))
sort(unique(mussels2$modifier))
table(mussels2$toxin)
table(mussels2$toxicity_units)
sort(unique(mussels2$comm_name))


# Build domoic data
################################################################################

# Clams 1
clams1_da <- clams1 %>% 
  # Select
  select(date, site, comm_name, species, tissue, da_modifier, da) %>% 
  # Rename
  rename(modifier=da_modifier,
         toxicity_ppm=da)

# Clams 1
clams2_da <- clams2 %>% 
  # Filter
  filter(toxin=="Domoic acid") %>% 
  # Select
  select(date, site, comm_name, species, tissue, modifier, toxicity) %>% 
  # Rename
  rename(toxicity_ppm=toxicity)

# Mussels 1
mussels1_da <- mussels1 %>% 
  # Select
  select(date, site, comm_name, species, tissue, da_modifier, da) %>% 
  # Rename
  rename(modifier=da_modifier,
         toxicity_ppm=da)

# Mussels 2
mussels2_da <- mussels2 %>% 
  # Filter
  filter(toxin=="Domoic acid") %>% 
  # Select
  select(date, site, comm_name, species, tissue, modifier, toxicity) %>% 
  # Rename
  rename(toxicity_ppm=toxicity)

# Crabs
crab_da <- crab %>% 
  # Filter
  filter(toxin=="Domoic acid") %>% 
  # Select
  select(date, site, comm_name, species, tissue, modifier, toxicity) %>% 
  # Rename
  rename(toxicity_ppm=toxicity)

# Merge
data_da <- bind_rows(clams1_da, clams2_da,
                     mussels1_da, mussels2_da,
                     crab_da) %>% 
  # Format site
  mutate(site=stringr::str_to_title(site))

# Inspect
str(data_da)
freeR::complete(data_da)

# Insoect more
table(data_da$comm_name)
table(data_da$tissue)
table(data_da$modifier)
table(data_da$site)

# Visualize
ggplot(data_da, aes(y=site, x=date, color=comm_name, size=toxicity_ppm)) +
  geom_point()


# Build PSP data
################################################################################

# Clams 1
clams1_psp <- clams1 %>% 
  # Select
  select(date, site, comm_name, species, tissue, da_modifier, da) %>% 
  # Rename
  rename(modifier=da_modifier,
         toxicity_ppm=da)

# Clams 1
clams2_psp <- clams2 %>% 
  # Filter
  filter(toxin=="PSP") %>% 
  # Select
  select(date, site, comm_name, species, tissue, modifier, toxicity) %>% 
  # Rename
  rename(toxicity_ppm=toxicity)

# Mussels 1
mussels1_psp <- mussels1 %>% 
  # Select
  select(date, site, comm_name, species, tissue, da_modifier, da) %>% 
  # Rename
  rename(modifier=da_modifier,
         toxicity_ppm=da)

# Mussels 2
mussels2_psp <- mussels2 %>% 
  # Filter
  filter(toxin=="PSP") %>% 
  # Select
  select(date, site, comm_name, species, tissue, modifier, toxicity) %>% 
  # Rename
  rename(toxicity_ppm=toxicity)

# Crabs
crab_psp <- crab %>% 
  # Filter
  filter(toxin=="PSP") %>% 
  # Select
  select(date, site, comm_name, species, tissue, modifier, toxicity) %>% 
  # Rename
  rename(toxicity_ppm=toxicity)

# Merge
data_psp <- bind_rows(clams1_psp, clams2_psp,
                     mussels1_psp, mussels2_psp,
                     crab_psp) %>% 
  # Format site
  mutate(site=stringr::str_to_title(site))

# Inspect
str(data_psp)
freeR::complete(data_psp)

# Insoect more
table(data_psp$comm_name)
table(data_psp$tissue)
table(data_psp$modifier)
table(data_psp$site)

# Visualize
ggplot(data_psp, aes(y=site, x=date, color=comm_name, size=toxicity_ppm)) +
  geom_point()


# Export data
################################################################################



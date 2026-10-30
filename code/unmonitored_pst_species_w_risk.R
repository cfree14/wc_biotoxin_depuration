

# Read data
data_orig <- readRDS("/Users/cfree/Library/CloudStorage/Dropbox/Chris/UCSB/projects/hab_depuration/data/toxicities/processed/toxicity_data.Rds")

# Format
data <- data_orig %>% 
  # Paralytic
  filter(syndrome=="Paralytic") %>% 
  # Convert mg/kg to ug/100g
  mutate(toxicity_ug100g=toxicity_mgkg_use*100) %>% 
  # Summarize
  group_by(comm_name) %>% 
  summarize(locations=paste(unique(region), collapse=", "), 
            toxicity_ug100g=max(toxicity_ug100g, na.rm=T)) %>% 
  ungroup() %>% 
  # Above action
  filter(toxicity_ug100g>=80)

# Manual list of species on West Coast
spp <- c("Brown rock crab", "Dungeness crab", "Dire whelk", "File dogwinkle", "Frilled dogwinkle", "Lewis's moon snail", "Northern kelp crab", "Pacific sardine", "Paddle crab", "Purple shore crab", "Red rock crab", "Yellow shore crab")

# Do again to get reference
data <- data_orig %>% 
  # Paralytic
  filter(syndrome=="Paralytic" & comm_name %in% c(spp)) %>% 
  # Convert mg/kg to ug/100g
  mutate(toxicity_ug100g=toxicity_mgkg_use*100) %>% 
  # Above action
  filter(toxicity_ug100g>=80)


# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(ggplot2)
library(tidyverse)
library(RSelenium)

# Directories
outdir <- "data/aquamaps/raw"

# Read species key
spp_key1 <- read.csv("data/merged/species_key/species_key1.csv") %>% 
  mutate(type=ifelse(grepl("spp.|/", species), "general", "species"),
         type=ifelse(species=="????", "general", type))

# Download AquaMaps data
################################################################################

# Download AquaMaps
aquamapsdata::download_db()
aquamapsdata::default_db("sqlite")

# Species to look up
spp_do <- spp_key1 %>% 
  filter(type=="species") %>% 
  pull(species)

# Loop through species
i <- 16
for(i in 1:length(spp_do)){
  
  # Species
  print(i)
  spp_do1 <- spp_do[i]
  
  # AquaMaps key
  key <- aquamapsdata::am_search_fuzzy(search_term = spp_do1)$key
  if(length(key)==0){print(paste(spp_do1, "not found"))}
  if(length(key)>1){print(key)}
  if(length(key)==1){
    
    # Aquamaps raster
    ras <- aquamapsdata::am_raster(key)
    
    # Convert to dataframe
    ras_df <- raster::as.data.frame(ras, xy=T) %>% 
      # Remove empty rows
      filter(!is.na(layer)) %>% 
      # Rename
      rename(long_dd=x, lat_dd=y, poccur=layer) %>% 
      # Add
      mutate(species=spp_do1,
             aquamaps_id=key)
    
    # Export
    write.csv(ras_df, file=file.path(outdir, paste0(spp_do1, ".csv")), row.names=F)
    
    # Plot
    ggplot(ras_df, aes(x=long_dd, y=lat_dd, fill=poccur)) +
      geom_tile() +
      # Labels
      labs(title=paste(i, spp_do1)) +
      # Legend
      scale_fill_gradientn(lim=c(0,1),
                           colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev) +
      # Theme
      theme_bw()
    
  }
  
}

# Build lat range
################################################################################

# Merge rasters
files2merge <- list.files(outdir)
ras_all <- purrr::map_df(files2merge, function(x){
  
  df <- read.csv(file.path(outdir, x))
  
})

lats <- ras_all %>% 
  group_by(species) %>% 
  summarize(lat_dd_s=min(lat_dd),
            lat_dd_n=max(lat_dd)) %>% 
  ungroup()


ggplot(lats, aes(x=pmax(lat_dd_s, 32), xend=lat_dd_n, y=reorder(species, lat_dd_n) )) +
  geom_segment() +
  # Ref line
  geom_vline(xintercept=42) +
  # Theme
  theme_bw()

# Not matched in AquaMaps
# "Monoplex pilearis"
# "Modiolus modiolus"
# "Magallana sikamea"
# "Ruditapes philippinarum"
# "Nuttallia nuttallii"
# "Nuttallia obscurata"
# "Protothaca tenerrima"
# "Saxidomus nuttalli"


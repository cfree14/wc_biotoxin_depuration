
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
tabledir <- "tables"
plotdir <- "figures"

# Read data
data_orig <- readRDS(file="/Users/cfree/Dropbox/Chris/UCSB/projects/domoic_closures/data/calhabmap/processed/calhabmap_data.Rds")

# Build data
################################################################################

# akashiwo_sanguinea_cells_l: no toxin - impacts are ecological
# lingulodinium_polyedra_cells_l: yessotoxin - no real symptoms     
# prorocentrum__cells_l: yessotoxin - no real symptoms (but actually I htink can produce OA)        
# ceratium_cells_l: 
# cochlodinium_cells_l: toxin/mechanism unknown but impacts

# Format data
data <- data_orig %>% 
  # Simplify
  select(dataset_id:time,   
         alexandrium__cells_l,                   
         dinophysis_cells_l,                 
         pseudo_nitzschia_delicatissima_cells_l,
         pseudo_nitzschia_seriata_cells_l, 
         gymnodinium_cells_l) %>% 
  # Gather
  gather(key="hab_species", value="cells_l", 12:ncol(.)) %>% 
  # Add month
  mutate(month=lubridate::month(date)) %>% 
  # Summarize by month
  filter(!is.na(cells_l)) %>% 
  group_by(hab_species, year, month, location, lat_dd) %>% 
  summarize(cells_l_max=max(cells_l)) %>% 
  ungroup() %>% 
  # Build date
  mutate(date=paste(year, month, 1, sep="-") %>% lubridate::ymd()) %>% 
  # Format HAB species
  mutate(hab_species=recode(hab_species,
                            "alexandrium__cells_l" = "Alexandrium spp.",                 
                            "dinophysis_cells_l" = "Dinophysis spp.",                    
                            "gymnodinium_cells_l" = "Gymnodinium spp.",                   
                            "pseudo_nitzschia_delicatissima_cells_l" = "Pseudo-nitzschia delicatissima",
                            "pseudo_nitzschia_seriata_cells_l" = "Pseudo-nitzschia seriata"),
         hab_species=factor(hab_species, levels=c("Alexandrium spp.", "Gymnodinium spp.", "Dinophysis spp.", "Pseudo-nitzschia delicatissima", "Pseudo-nitzschia seriata")))
  
str(data)
sort(unique(data$hab_species))

# Plot data
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=7),
                   axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
                   axis.title=element_text(size=7),
                   legend.text=element_text(size=7),
                   legend.title=element_text(size=8),
                   strip.text=element_text(size=7),
                   plot.title=element_text(size=8),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key = element_rect(fill = NA, color=NA),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g <- ggplot(data, aes(x=date, y=reorder(location, lat_dd), fill=cells_l_max)) +
  facet_wrap(~hab_species) +
  geom_tile() +
  # Labels
  labs(x="Month", y="") +
  # Axes
  scale_x_date(breaks=seq(ymd("1995-01-01"), 
                          ymd("2026-01-01"), by="5 years"),
               date_label="%Y") +
  # Legend
  scale_fill_gradientn(name="Density (cells/L)",
                       na.value="grey80",
                       trans="log10",
                       breaks=c(1, 10, 100, 1000, 10000, 100000, 1000000),
                       labels=c("1", "10", "100", "1,000", "10,000", "100,000", "1,000,000"),
                       colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev()) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", frame.linewidth = 0.2)) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = c(0.8, 0.2))
g

# Export
ggsave(g, filename=file.path(plotdir, "FigSX_hab_species_abundance.png"), 
       width=6.5, height=4.5, units="in", dpi=600)




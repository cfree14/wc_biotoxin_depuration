
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

# Read data
data_orig <- readRDS(file=file.path(outdir, "WC_domoic_acid_data.Rds"))


# Build sites
################################################################################

# Build sites
sites <- data_orig %>% 
  select(state, lat_dd, long_dd) %>% unique()


# Themes
################################################################################

# Base theme
base_theme <- theme(axis.text=element_text(size=7),
                    axis.title=element_text(size=8),
                    legend.text=element_text(size=7),
                    legend.title=element_text(size=8),
                    strip.text=element_text(size=8),
                    plot.title=element_text(size=8),
                    plot.tag=element_text(size=9),
                    # Gridlines
                    panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(), 
                    axis.line = element_line(colour = "black"),
                    # Legend
                    legend.key = element_rect(fill = NA, color=NA),
                    legend.background = element_rect(fill=alpha('blue', 0)))

# Plot figure
################################################################################

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
world <- rnaturalearth::ne_countries(country = c("Mexico", "Canada"), returnclass = "sf", scale="small")

# Plot map
g1 <- ggplot() +
  # Plot land
  geom_sf(data=world, fill="grey80", color="white", lwd=0.2, inherit.aes = F) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.2, inherit.aes = F) +
  # Plot sites
  geom_point(data=sites, aes(x=long_dd, lat_dd, color=state)) +
  # Labels
  labs(y="", x="", tag="A") +
  # Axes
  scale_y_continuous(breaks=seq(32,50, 2)) +
  # Crop
  coord_sf(xlim=c(-125, -116), ylim=c(33, 49)) +
  # Theme
  theme_bw() + base_theme +
  theme(axis.title.y=element_blank(), 
        axis.text.y = element_text(angle = 90, hjust = 0.5),
        legend.position = "none")
g1
  

# Plot monitoring
g2 <- ggplot(data_orig, aes(x=date, y=lat_dd, size=toxicity_ppm, color=comm_name)) +
  geom_point() +
  # Labels
  labs(x="Date", y="Latitude (°N)", tag="B") +
  # Axis
  scale_y_continuous(breaks=seq(32,50, 2)) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = "none",
        axis.text.y = element_text(angle = 90, hjust = 0.5))
g2

# Plot management

# Merge
g <- gridExtra::grid.arrange(g1, g2, widths=c(0.37, 0.63))

# Export
ggsave(g, filename=file.path(plotdir, "Fig3_map_timeline_da.png"), 
       width=6.5, height=5.5, units="in", dpi=600)



# # Merge (three plot layout)
# layout_matrix <- matrix(data=c(1,2,
#                                1,3), ncol=2, byrow=T)
# g <- gridExtra::grid.arrange(g1, g2, g2, 
#                              layout_matrix=layout_matrix, 
#                              widths=c(0.37, 0.63))


  
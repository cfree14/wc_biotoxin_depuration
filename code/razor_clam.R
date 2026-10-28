
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

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
world <- rnaturalearth::ne_countries(country = c("Mexico", "Canada"), returnclass = "sf", scale="large")


# Build data
################################################################################

# Format data
data <- data_orig %>% 
  # Razor cam 
  filter(comm_name=="Razor clam" & !is.na(lat_dd) & !is.na(toxicity_ppm)) %>% 
  # Mark 
  mutate(action_yn=ifelse(toxicity_ppm>20, "yes", "no")) %>% 
  # Arrange
  # desc(comm_name) puts most abundant on bottom
  arrange(desc(comm_name), date)

# Pull out low / high values
data_lo <- data %>% 
  filter(action_yn=="no")
data_hi <- data %>% 
  filter(action_yn=="yes")


# Plot figure
################################################################################

# Base theme
base_theme <- theme(axis.text=element_text(size=8),
                    axis.title=element_text(size=9),
                    axis.title.y=element_blank(),
                    legend.text=element_text(size=8),
                    legend.title=element_text(size=9),
                    strip.text=element_text(size=9),
                    plot.title=element_text(size=9),
                    plot.tag=element_text(size=9),
                    # Gridlines
                    panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(), 
                    axis.line = element_line(colour = "black"),
                    # Legend
                    legend.key = element_rect(fill = NA, color=NA),
                    legend.background = element_rect(fill=alpha('blue', 0)))

# Plot map
g1 <- ggplot() +
  # Plot land
  geom_sf(data=world, fill="grey90", color="white", lwd=0.3, inherit.aes = F) +
  geom_sf(data=usa, fill="grey90", color="white", lwd=0.3, inherit.aes = F) +
  # Labels
  labs(x="", y="", tag="A") +
  # Axis
  scale_x_continuous(breaks=seq(-124, -116, 1)) +
  
  scale_y_continuous(breaks=seq(32, 50, 1)) +
  # Crop
  coord_sf(xlim=c(-124.9, -122.8), 
           ylim=c(40.4, 48.1), 
           expand=F) +
  # Theme
  theme_bw() + base_theme +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5),
        axis.title.y=element_blank())
g1

# Plot monitoring
range(data$lat_dd, na.rm=T)
g2 <- ggplot() +
  # State lines
  geom_hline(yintercept=c(42, 46.25)) +
  # Below 
  geom_point(data_lo, mapping=aes(x=date, 
                       y=lat_dd), alpha=0.2, size=0.7, pch=16) +
  # Above
  geom_point(data_hi, mapping=aes(x=date, 
                               y=lat_dd,
                               size=toxicity_ppm, 
                               fill=toxicity_ppm), pch=21) +
  # Labels
  labs(x="Date", y="Latitude (°N)", tag="B") +
  # Y-axis
  scale_y_continuous(breaks=seq(32, 50, 1), 
                     labels=paste0(seq(32, 50, 1), "°N"),
                     lim=c(40.4, 48.1),
                     expand=F) +
  # X-axis
  scale_x_date(breaks=seq(ymd("1990-01-01"), 
                          ymd("2025-01-01"), by="1 years"),
               date_label="%Y", 
               lim=c(ymd("2014-06-01"), 
                     ymd("2025-01-01"))) +
  # Legend
  scale_size_continuous(name="Toxicity (ppm)") +
  scale_fill_gradientn(name="Toxicity (ppm)", colors=RColorBrewer::brewer.pal(9, "YlOrRd")) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", frame.linewidth = 0.2)) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = "right",
        axis.title.y=element_blank(),
        axis.text.y = element_text(angle = 90, hjust = 0.5))
g2



# Merge
g <- gridExtra::grid.arrange(g1, g2, nrow=1,
                             widths=c(0.22, 0.78))

# Export
ggsave(g, filename=file.path(plotdir, "razor_clam.png"), 
       width=6.5, height=4.5, units="in", dpi=600, bg="white")




  
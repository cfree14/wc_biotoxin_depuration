
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

# Get blocks
blocks_orig <- wcfish::blocks


# Build rock crab landings data
################################################################################

# Raw landing receipts
cdfw_orig <- readRDS("/Users/cfree/Library/CloudStorage/Dropbox/Chris/UCSB/projects/california/cdfw_data/data/confidential/landing_receipts_2024/processed/1987_2025_landings_receipts.Rds")

# Common names
comm_names <- freeR::uniq(cdfw_orig$comm_name)
rcrab_names <- comm_names[grepl("rock crab", comm_names)]

# Numnber of years to average over
nyrs <- sum(unique(cdfw_orig$year) < 2015)

# Build block level data
cdfw <- cdfw_orig %>% 
  # Rcok crab
  filter(comm_name %in% rcrab_names & year<2015) %>% 
  # Summarize
  group_by(block_id) %>% 
  summarize(landings_lbs=sum(landings_lbs, na.rm = T)/nyrs) %>% 
  ungroup() %>% 
  # Convert
  mutate(landings_kg=measurements::conv_unit(landings_lbs, "lbs", "kg"),
         landings_mt=landings_kg/1000)

# Spatialize
blocks <- blocks_orig %>% 
  left_join(cdfw) %>% 
  filter(!is.na(landings_lbs)) %>% 
  filter(block_type!="Offshore")


# Build data
################################################################################

table(data_orig$comm_name)

# Format data
data <- data_orig %>% 
  # Razor cam 
  filter(comm_name=="Rock crab" & !is.na(lat_dd) & !is.na(toxicity_ppm)) %>% 
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
  # Plot blocks
  geom_sf(data=blocks, mapping=aes(fill=landings_lbs)) +
  # Plot land
  geom_sf(data=world, fill="grey90", color="white", lwd=0.3, inherit.aes = F) +
  geom_sf(data=usa, fill="grey90", color="white", lwd=0.3, inherit.aes = F) +
  # Labels
  labs(x="", y="", tag="A") +
  # Axis
  scale_x_continuous(breaks=seq(-124, -116, 2)) +
  
  scale_y_continuous(breaks=seq(32, 50, 2)) +
  # Legend
  scale_fill_gradientn(name="Pre-2015\nAnnual landings (lbs)", 
                       trans="log10",
                       breaks=c(1, 10, 100, 1000, 10000, 100000),
                       labels=c("1", "10", "100", "1,000", "10,000", "100,000"), 
                       colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev()) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", frame.linewidth = 0.2)) +
  # Crop
  coord_sf(xlim=c(-124.8, -117), 
           ylim=c(32, 42), 
           expand=F) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position=c(0.65, 0.8), 
        legend.key.size=unit(0.3, "cm"), 
        axis.text.y = element_text(angle = 90, hjust = 0.5),
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
  scale_y_continuous(breaks=seq(32, 42, 2),
                     labels=paste0(seq(32, 42, 2), "°N"),
                     lim=c(32, 42),
                     expand=F) +
  # X-axis
  scale_x_date(breaks=seq(ymd("1990-01-01"), 
                          ymd("2025-01-01"), by="1 years"),
               date_label="%Y", 
               lim=c(ymd("2015-06-01"), 
                     ymd("2025-01-01"))) +
  # Legend
  scale_size_continuous(name="Toxicity (ppm)") +
  scale_fill_gradientn(name="Toxicity (ppm)", colors=RColorBrewer::brewer.pal(9, "YlOrRd")) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", frame.linewidth = 0.2)) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = c(0.85, 0.6),
        legend.key.size=unit(0.3, "cm"),
        axis.title.y=element_blank(),
        axis.text.y = element_text(angle = 90, hjust = 0.5))
g2

# Merge
g <- gridExtra::grid.arrange(g1, g2, nrow=1,
                             widths=c(0.35, 0.65))

# Export
ggsave(g, filename=file.path(plotdir, "rock_crab.png"), 
       width=6.5, height=3.75, units="in", dpi=600, bg="white")




  

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
data_orig <- readRDS(file=file.path(outdir, "WC_dsp_data.Rds"))

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
world <- rnaturalearth::ne_countries(country = c("Mexico", "Canada"), returnclass = "sf", scale="large")

# Morpho proposal stats
################################################################################

data_orig %>% 
  group_by(state) %>% 
  summarize(nspp=n_distinct(comm_name),
            year1=min(year, na.rm=T),
            year2=max(year, na.rm=T))



# Build data
################################################################################

# Build data
stats <- data_orig %>% 
  # Count and max toxicity
  group_by(comm_name, species) %>% 
  summarize(n=n(),
            toxicity_ug_100g_max=max(toxicity_ug_100g, na.rm = T)) %>% 
  ungroup() %>% 
  # Remove unknown species
  filter(species!="Unknown spp.") %>% 
  # Mark generic species
  mutate(spp_yn=!grepl("spp", species) & !grepl("/", comm_name))  %>% 
  # Arrange
  arrange(n) %>% 
  mutate(comm_name=factor(comm_name, comm_name))

# Common name order
name_order <- levels(stats$comm_name)
name_colors <- c(rep("grey80", 7), 
                 RColorBrewer::brewer.pal(11, "Paired") %>% rev())

# Sites
sites <- data_orig %>% 
  count(lat_dd, long_dd)

# Format data
data <- data_orig %>% 
  # Remove 
  filter(species!="Unknown spp.") %>% 
  # Mark 
  mutate(action_yn=ifelse(toxicity_ug_100g>16, "yes", "no")) %>% 
  # Order species
  mutate(comm_name=factor(comm_name, name_order)) %>% 
  # Arrange
  # desc(comm_name) puts most abundant on bottom
  arrange(desc(comm_name), date)
  
# Pull out low / high values
data_lo <- data %>% 
  filter(action_yn=="no")
data_hi <- data %>% 
  filter(action_yn=="yes")

# Annual max toxicity stats
max_yr <- data %>% 
  # Remove NAs
  filter(!is.na(toxicity_ug_100g)) %>% 
  # Annual max toxicity
  group_by(comm_name, year) %>% 
  summarize(toxicity_ug_100g_max=max(toxicity_ug_100g, na.rm=T)) %>% 
  ungroup() %>% 
  # Order common name
  mutate(comm_name=factor(comm_name, name_order))


# Plot figure
################################################################################

# Base theme
base_theme <- theme(axis.text=element_text(size=8),
                    axis.title=element_text(size=9),
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
  # Plot sites
  geom_point(data=sites, aes(x=long_dd, y=lat_dd), size=1.1, pch=21, fill="skyblue", stroke=0.3) +
  # Labels
  labs(x="", y="", tag="A") +
  # Axis
  scale_x_continuous(breaks=seq(-126, -116, 2)) +
  scale_y_continuous(breaks=seq(36, 50, 2)) +
  # Crop
  coord_sf(xlim=c(-124.9, -122.1), 
           ylim=c(36, 49.1), 
           expand=F) +
  # Theme
  theme_bw() + base_theme +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5),
        axis.title.y=element_blank())
g1

# Plot monitoring
range(data$lat_dd, na.rm=T)
g2 <- ggplot() +
  # Ref line
  geom_hline(yintercept=c(42, 46.25), color="grey90") +
  # Below 
  geom_point(data_lo, mapping=aes(x=date, 
                       y=lat_dd,
                       color=comm_name), alpha=0.2, size=0.7, pch=16) +
  # Above
  geom_point(data_hi, mapping=aes(x=date, 
                               y=lat_dd,
                               size=toxicity_ug_100g, 
                               color=comm_name), pch=16) +
  # Labels
  labs(x="Date", y="Latitude (°N)", tag="B") +
  # Y-axis
  scale_y_continuous(breaks=seq(36, 50, 2), 
                     labels=paste0(seq(36, 50, 2), "°N"),
                     lim=c(36, 49.1),
                     expand=F) +
  # X-axis
  scale_x_date(breaks=seq(ymd("2010-01-01"), 
                          ymd("2025-01-01"), by="2 years"),
               date_label="%Y") +
  # Legend
  # scale_alpha_discrete() +
  scale_color_manual(name="Species", values=name_colors) +
  scale_shape_manual(values=c(4, 16)) +
  scale_size_continuous(name="Toxicity (ug/100g)") +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = "right",
        legend.key.size = unit(0.3, "cm"),
        axis.title.y=element_blank(),
        axis.text.y = element_text(angle = 90, hjust = 0.5))
g2



# Merge
g <- gridExtra::grid.arrange(g1, g2,
                             widths=c(0.15, 0.85))

# Export
ggsave(g, filename=file.path(plotdir, "dsp_for_conference.png"), 
       width=6.5, height=4.0, units="in", dpi=600, bg="white")




  
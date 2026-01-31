
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


# Build data
################################################################################

# Build data
stats <- data_orig %>% 
  group_by(comm_name, species) %>% 
  summarize(n=n(),
            toxicity_ug_100g_max=max(toxicity_ug_100g, na.rm = T)) %>% 
  ungroup() %>% 
  # Remove unknown species
  filter(species!="Unknown spp.") %>% 
  # Mark generic
  mutate(spp_yn=!grepl("spp", species) & !grepl("/", comm_name))  %>% 
  # Record name use
  mutate(comm_name_use1=ifelse(spp_yn==F | n<50, "Other", comm_name),
         comm_name_use2=ifelse(spp_yn==F | n<50, NA, comm_name)) %>% 
  # Arrange
  arrange(desc(n)) %>% 
  mutate(comm_name_use1=factor(comm_name_use1, levels=unique(comm_name_use1)))
levels(stats$comm_name_use1)

# Sites
sites <- data_orig %>% 
  count(lat_dd, long_dd)

# Format data
data <- data_orig %>% 
  # Remove 
  filter(species!="Unknown spp.") %>% 
  # Mark 
  mutate(action_yn=ifelse(toxicity_ug_100g>16, "yes", "no")) %>% 
  # Add common name used for visualization
  left_join(stats %>% select(comm_name, comm_name_use1, comm_name_use2), by=c("comm_name")) %>% 
  # Order
  mutate(comm_name_use1=factor(comm_name_use1, levels=levels(stats$comm_name_use1))) %>% 
  arrange(comm_name_use1, date)

data_lo <- data %>% 
  filter(action_yn=="no")
data_hi <- data %>% 
  filter(action_yn=="yes")

# Toxicity stats
max_yr <- data %>% 
  filter(!is.na(toxicity_ug_100g)) %>% 
  group_by(comm_name, comm_name_use1, comm_name_use2, year) %>% 
  summarize(toxicity_ug_100g_max=max(toxicity_ug_100g, na.rm=T)) %>% 
  ungroup()


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
  scale_y_continuous(breaks=seq(46, 50, 0.5)) +
  # Crop
  coord_sf(xlim=c(-124.9, -122.1), 
           ylim=c(46.2, 49.1), 
           expand=F) +
  # Theme
  theme_bw() + base_theme +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5),
        axis.title.y=element_blank())
g1

# Plot monitoring
range(data$lat_dd, na.rm=T)
g2 <- ggplot() +
  # Below 
  geom_point(data_lo, mapping=aes(x=date, 
                       y=lat_dd,
                       color=comm_name_use2), alpha=0.2, size=0.7, pch=16) +
  # Above
  geom_point(data_hi, mapping=aes(x=date, 
                               y=lat_dd,
                               size=toxicity_ug_100g, 
                               color=comm_name_use2), pch=16) +
  # Labels
  labs(x="Date", y="Latitude (°N)", tag="B") +
  # Y-axis
  scale_y_continuous(breaks=seq(46, 50, 0.5), 
                     labels=paste0(seq(46, 50, 0.5), "°N"),
                     lim=c(46.2, 49.1),
                     expand=F) +
  # X-axis
  scale_x_date(breaks=seq(ymd("2010-01-01"), 
                          ymd("2025-01-01"), by="2 years"),
               date_label="%Y") +
  # Legend
  # scale_alpha_discrete() +
  scale_shape_manual(values=c(4, 16)) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = "none",
        axis.title.y=element_blank(),
        axis.text.y = element_text(angle = 90, hjust = 0.5))
g2

# Plot species toxicity
g3 <- ggplot(max_yr, aes(x=toxicity_ug_100g_max,
                        y=reorder(comm_name, comm_name_use1),
                        fill=comm_name_use1)) +
  geom_violin(color=NA, alpha=0.6) +
  geom_point(pch=21, size=2) +
  # Reference line
  geom_vline(xintercept=16) +
  # Labels
  labs(x="Annual max toxicity (ug/100g)", y="", tag="C") +
  scale_x_continuous(trans="log10") +
  # Legend
  scale_fill_discrete(na.value = "grey80") +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = "none",
        panel.grid.major.y = element_line(color="grey80", linewidth=0.2),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.line.y=element_blank(),
        plot.margin = margin(t=-8),
        panel.border = element_blank())
g3

# Plot species sample size
g4 <- ggplot(stats, aes(x=n, 
                        y=reorder(comm_name, n),
                        fill=comm_name_use2)) +
  geom_col() +
  # Labels
  labs(x="Number of tests", y="", tag="D") +
  scale_x_continuous(trans="log10") +
  # Legend
  scale_fill_discrete(na.value = "grey80") +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = "none",
        panel.grid.major.y = element_line(color="grey80", linewidth=0.2),
        axis.text.y = element_text(hjust = 0.5),
        axis.ticks.y=element_blank(),
        axis.line.y=element_blank(),
        plot.margin = margin(t=-8),
        panel.border = element_blank())
g4


# Merge
layout_matrix <- matrix(data=c(1, 2,
                               3, 4), byrow=T, ncol=2)
g <- gridExtra::grid.arrange(g1, g2, g3, g4,
                             layout_matrix=layout_matrix,
                             heights=c(0.66, 0.34),
                             widths=c(0.44, 0.56))

# Export
ggsave(g, filename=file.path(plotdir, "FigX_dsp_map_timeline_65.png"), 
       width=6.5, height=6.5, units="in", dpi=600, bg="white")




  
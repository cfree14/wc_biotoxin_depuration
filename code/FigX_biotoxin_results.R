
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
data_orig <- readRDS(file=file.path(outdir, "CDPH_1991_2025_all_domoic_data.Rds"))

# Ideas
# Summarize which species sentinel, cultured, wild. etc
# Generate map of sentinel sites
# Generate map of aquaculture sites
# Generate map of common wild sites

# Cultured: Pacific oyster and bay mussel common, kumamoto oyster (rare), Rock scallop (rare, also wilfe)
# Sentinel: sea mussel and mixed bay/blue/sea mussel


# Build data
################################################################################

# Data
data <- data_orig %>% 
  # Recode states
  mutate(state="CA") %>% 
  # Recode sources
  mutate(source_code=recode(source, 
                       "cultured"="C",
                       "wild"="W",
                       "sentinel"="S"))

# Species stats
stats_spp <- data %>% 
  group_by(comm_name, species) %>% 
  summarize(states=paste(unique(state), collapse = ", "),
            sources=paste(unique(source_code), collapse = ", "),
            n=n(),
            nyr=n_distinct(year),
            nsites=n_distinct(site),
            toxicity_ug_g_max=max(toxicity_ug_g)) %>% 
  ungroup() %>% 
  arrange(desc(n))

# Order data by species
data_ordered <- data %>% 
  mutate(comm_name=factor(comm_name, levels=stats_spp$comm_name))

# Export stats
write.csv(stats_spp, file=file.path(tabledir, "TableSX_species_da_statistics.csv"), row.names=F)


# Plot data - all together
################################################################################

# Dates
range(data$date)
date_min_do <- "1991-01-01" %>% lubridate::ymd()
date_max_do <- "2025-01-01" %>% lubridate::ymd()

# Theme
my_theme <-  theme(axis.text=element_text(size=7),
                   axis.title=element_text(size=8),
                   legend.text=element_text(size=7),
                   legend.title=element_text(size=8),
                   strip.text=element_text(size=8),
                   plot.title=element_blank(),
                   plot.tag = element_text(size=9),
                   # Gridlines
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black"))

# Plot data
g1 <- ggplot(data_ordered, aes(x=date, y=lat_dd, color=comm_name, size=toxicity_ug_g)) +
  # Plot data
  geom_point() +
  # Plot state lines
  geom_hline(yintercept=c(42, 46)) +
  # Axes
  scale_y_continuous(lim=c(32, 50), breaks=seq(32, 50, 2)) +
  scale_x_date(breaks=seq(ymd("1990-01-01"), ymd("2025-01-01"), by="5 year"), date_labels="%Y") +
  # Labels
  labs(x="Date", y="Latitude (°N)") +
  # Legend
  scale_size_continuous(name="Toxicity (ug/g)") +
  scale_color_discrete(name="Species") +
  # Theme
  theme_bw() + my_theme +
  theme(legend.key.size = unit(0.3, "cm"))
g1

# Export
ggsave(g1, filename=file.path(plotdir, "FigSX_species_da_spatiotemporal_together.png"), 
       width=6.5, height=6.5, units="in", dpi=600)


# Plot data - facetted
################################################################################

# Plot data
g2 <- ggplot(data_ordered, aes(x=date, y=lat_dd, color=source, size=toxicity_ug_g)) +
  facet_wrap(~comm_name, ncol=5) +
  # Plot data
  geom_point() +
  # Plot state lines
  geom_hline(yintercept=c(42, 46)) +
  # Axes
  scale_y_continuous(lim=c(32, 50), breaks=seq(32, 50, 2)) +
  scale_x_date(breaks=seq(ymd("1990-01-01"), ymd("2025-01-01"), by="5 year"), 
               # lim=c(ymd("2010-01-01"), ymd("2025-01-01")),
               date_labels="%Y") +
  # Labels
  labs(x="Date", y="Latitude (°N)") +
  # Legend
  scale_size_continuous(name="Toxicity (ug/g)") +
  scale_color_discrete(name="Source") +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position=c("top"), 
        legend.key.size=unit(0.3, "cm"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
g2

# Export
ggsave(g2, filename=file.path(plotdir, "FigSX_species_da_spatiotemporal_facetted.png"), 
       width=6.5, height=6.5, units="in", dpi=600)


# Plot data - farmed
################################################################################

# Plot data
g3 <- ggplot(data_ordered %>% filter(source=="cultured"), aes(x=date, y=lat_dd, color=source, size=toxicity_ug_g)) +
  facet_wrap(~comm_name, ncol=5) +
  # Plot data
  geom_point() +
  # Plot state lines
  geom_hline(yintercept=c(42, 46)) +
  # Axes
  scale_y_continuous(lim=c(32, 50), breaks=seq(32, 50, 2)) +
  scale_x_date(breaks=seq(ymd("1990-01-01"), ymd("2025-01-01"), by="5 year"), 
               # lim=c(ymd("2010-01-01"), ymd("2025-01-01")),
               date_labels="%Y") +
  # Labels
  labs(x="Date", y="Latitude (°N)") +
  # Legend
  scale_size_continuous(name="Toxicity (ug/g)") +
  scale_color_discrete(name="Source") +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position=c("top"), 
        legend.key.size=unit(0.3, "cm"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
g3

# Plot data - sentinel
################################################################################

# Plot data
g4 <- ggplot(data_ordered %>% filter(source=="sentinel"), aes(x=date, y=lat_dd, color=source, size=toxicity_ug_g)) +
  facet_wrap(~comm_name, ncol=5) +
  # Plot data
  geom_point() +
  # Plot state lines
  geom_hline(yintercept=c(42, 46)) +
  # Axes
  scale_y_continuous(lim=c(32, 50), breaks=seq(32, 50, 2)) +
  scale_x_date(breaks=seq(ymd("1990-01-01"), ymd("2025-01-01"), by="5 year"), 
               # lim=c(ymd("2010-01-01"), ymd("2025-01-01")),
               date_labels="%Y") +
  # Labels
  labs(x="Date", y="Latitude (°N)") +
  # Legend
  scale_size_continuous(name="Toxicity (ug/g)") +
  scale_color_discrete(name="Source") +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position=c("top"), 
        legend.key.size=unit(0.3, "cm"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
g4


# Plot sites
################################################################################

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
world <- rnaturalearth::ne_countries(country = c("Mexico", "Canada"), returnclass = "sf", scale="small")

# Build site key
sites <- data %>% 
  group_by(county, site, source) %>% 
  summarize(ntests=n(),
            nyrs=n_distinct(year),
            lat_dd=median(lat_dd, na.rm=T),
            long_dd=median(long_dd, na.rm=T)) %>% 
  ungroup() %>% 
  mutate(source=factor(source, levels=c( "wild", "sentinel", "cultured"))) %>% 
  # Order for plotting (wild on bottom)
  arrange(source, desc(nyrs))

# Map theme
map_theme <- theme(axis.text=element_text(size=7),
                   axis.text.y = element_text(angle = 90, hjust = 0.5),
                   axis.title=element_blank(),
                   legend.text=element_text(size=7),
                   legend.title=element_text(size=8),
                   strip.text=element_text(size=8),
                   plot.title=element_blank(),
                   plot.tag = element_text(size=9),
                   # Gridlines
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key = element_rect(fill = NA, color=NA),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot map
ggplot() +
  # Plot land
  geom_sf(data=world, fill="grey80", color="white", lwd=0.2, inherit.aes = F) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.2, inherit.aes = F) +
  # Plot all sites
  geom_point(data=sites, 
             mapping=aes(x=long_dd, y=lat_dd, fill=source, size=nyrs),
             pch=21, alpha=0.5) +
  # Label sentinel/cultured
  geom_text(data=sites %>% filter(source!="wild"),
            mapping=aes(x=long_dd, y=lat_dd, color=source, label=site),
            size=2, hjust=1,
            show.legend = F) +
  # Axes
  scale_y_continuous(breaks=seq(32,50,2)) +
  # Legend
  scale_color_discrete(name="Source", drop=F) +
  scale_fill_discrete(name="Source", drop=F) +
  scale_size_continuous(name="# of years") +
  # Crop
  coord_sf(xlim=c(-116.5,-126.5), ylim=c(32.5, 42)) +
  # Theme
  theme_bw() + map_theme +
  theme(legend.position = c(0.8, 0.7))




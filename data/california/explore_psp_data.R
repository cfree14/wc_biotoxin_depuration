

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(stringr)
library(tidyverse)
library(plotly)

# Directories
indir <- "data/california/raw/fdb_christina"
outdir <- "data/california/processed"
intdir <- "data/california/intermediate"
plotdir <- "data/california/figures"

# Read data
data_orig <- readRDS(file=file.path(outdir, "CDPH_EMB_1962_2025_bivalve_psp_data.Rds"))

# Reduce to usable data
data <- data_orig %>% 
  filter(!is.na(toxicity_ug_100g) & !is.na(comm_name))
freeR::complete(data)


# Table
################################################################################

# Stats
stats <- data %>% 
  group_by(comm_name, species) %>% 
  summarize(type=unique(source_use) %>% paste(., collapse=", "),
            ntests=n(),
            ntests_above=sum(toxicity_ug_100g>=80),
            nyrs=n_distinct(year),
            nyrs_above=n_distinct(year[toxicity_ug_100g>=80]),
            toxicity_ug_100g_max=max(toxicity_ug_100g)) %>% 
  ungroup()

# Stats
stats1 <- data %>% 
  group_by(comm_name, species, source) %>% 
  summarize(type=unique(source_use) %>% paste(., collapse=", "),
            ntests=n(),
            ntests_above=sum(toxicity_ug_100g>=80),
            nyrs=n_distinct(year),
            nyrs_above=n_distinct(year[toxicity_ug_100g>=80]),
            toxicity_ug_100g_max=max(toxicity_ug_100g)) %>% 
  ungroup()


# Investigate farmed sites
################################################################################

g <- ggplot(data %>% filter(source_use=="cultured"), aes(x=date, y=lat_dd, color=source, text=site)) +
  facet_wrap(~comm_name, nrow=1) +
  geom_point() +
  theme_bw()
ggplotly(g, tooltip = "text")

g <- ggplot(data %>% filter(source_use=="cultured"), aes(x=date, y=lat_dd, color=comm_name, text=site)) +
  geom_point(pch=1) +
  # lims(y=c(37.5,38.5)) +
  theme_bw()
ggplotly(g, tooltip = "text")

# Investigate sentinel sites
################################################################################

g <- ggplot(data %>% filter(source_use=="sentinel"), aes(x=date, y=lat_dd, color=source, text=site)) +
  facet_wrap(~comm_name, nrow=1) +
  geom_point() +
  theme_bw()
ggplotly(g, tooltip = "text")

g <- ggplot(data %>% filter(source_use=="sentinel"), aes(x=date, y=lat_dd, color=comm_name, text=site)) +
  geom_point(pch=1) +
  # lims(y=c(37.5,38.5)) +
  theme_bw()
ggplotly(g, tooltip = "text")


# Investigate wild sites
################################################################################

g <- ggplot(data %>% filter(source_use%in%c("wild", "not specified")), 
                            aes(x=date, y=lat_dd, color=source_use, text=site)) +
  facet_wrap(~comm_name) +
  geom_point() +
  theme_bw()
ggplotly(g, tooltip = "text")


# Plot sentinel timeline
################################################################################

# Timeline theme
time_theme <- theme(axis.text=element_text(size=8),
                    # axis.text.y = element_text(angle = 90, hjust = 0.5),
                    axis.title=element_text(size=9),
                    legend.text=element_text(size=8),
                    legend.title=element_text(size=9),
                    strip.text=element_text(size=8),
                    plot.title=element_text(size=9),
                    # Gridlines
                    panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(), 
                    axis.line = element_line(colour = "black"),
                    # Legend
                    legend.position = "top",
                    legend.key = element_rect(fill = NA, color=NA),
                    legend.background = element_rect(fill=alpha('blue', 0)))

# Build data
data_s <- data %>% 
  # Reduce
  filter(source_use=="sentinel") %>% 
  # Shorten name
  mutate(comm_name=recode(comm_name,
                          "Mediterranean/Pacific blue/blue mussels"="Mixed mussels")) %>% 
  # Add sample size
  group_by(comm_name) %>% 
  mutate(n=n()) %>% 
  ungroup() %>% 
  mutate(comm_name=paste0(comm_name, " (", n, ")")) %>% 
  # Factor
  arrange(desc(n)) %>% 
  mutate(comm_name=factor(comm_name, levels=unique(comm_name)))

# Lat lines
lines_s <- data_s %>% 
  # Round lat
  mutate(lat_dd_round=round(lat_dd, digits=1)) %>% 
  # Id most common site
  count(lat_dd_round, site) %>% 
  arrange(lat_dd_round, desc(n)) %>% 
  group_by(lat_dd_round) %>% 
  slice(1) %>% 
  ungroup() %>% 
  # Remove a few
  filter(!site %in% c("Point St. George", "Trinidad Pier")) %>%
  # Format site names
  mutate(site=recode(site,
                     "Crescent City Harbor" = "Crescent City",
                     "Trinidad, Camel Rock"="Trinidad",
                     "Humboldt Bay, Indian Is. Ch." = "Humboldt Bay",
                     "Mugu Lagoon, Laguna Bridge"="Mugu Lagoon",
                     "Santa Cruz Is., Prisoners Hrbr"="Santa Cruz Island"))
  
# Plot data
g <- ggplot(data_s, aes(x=date, y=lat_dd, color=comm_name, text=site)) +
  # 2025
  geom_vline(xintercept=lubridate::ymd("2024-01-01")) +
  # Lines
  geom_hline(yintercept=lines_s$lat_dd_round, color="grey90") +
  # Data
  geom_point(pch=1) +
  # Labels
  labs(x="Date", y="Latitude (°N)") +
  scale_x_date(breaks=seq(ymd("1980-01-01"), 
                          ymd("2025-01-01"), by="5 years"),
               date_label="%Y") +
  scale_y_continuous(breaks=seq(32, 42, 1),
                     sec.axis = dup_axis(
                       breaks = lines_s$lat_dd_round,
                       labels = lines_s$site,
                       name   = NULL
                     )) +
  # Legend
  scale_color_discrete(name="") +
  # Theme
  theme_bw() + time_theme +
  theme(legend.key.size = unit(0.3, "cm"))
g

# Export figure
ggsave(g, filename=file.path(plotdir, "FigSX_ca_psp_sentinel_timline.png"), 
       width=6.5, height=5, units="in", dpi=600)


# Plot cultured timeline
################################################################################

# Build data
data_c <- data %>% 
  # Reduce
  filter(source_use=="cultured") %>% 
  # Shorten name
  mutate(comm_name=recode(comm_name,
                          "Mediterranean/Pacific blue/blue mussels"="Mixed mussels")) %>% 
  # Add sample size
  group_by(comm_name) %>% 
  mutate(n=n()) %>% 
  ungroup() %>% 
  mutate(comm_name=paste0(comm_name, " (", n, ")")) %>% 
  # Factor
  arrange(desc(n)) %>% 
  mutate(comm_name=factor(comm_name, levels=unique(comm_name)))

# Lat lines
lines_c <- data_c %>% 
  # Round lat
  mutate(lat_dd_round=round(lat_dd, digits=1)) %>% 
  # Id most common site
  count(lat_dd_round, site) %>% 
  arrange(lat_dd_round, desc(n)) %>% 
  group_by(lat_dd_round) %>% 
  slice(1) %>% 
  ungroup() %>% 
  # Remove a few
  filter(!site %in% c("Estero Bay", "Santa Barbara Ch., Plt Houchin", "Humboldt Bay, Mad River 7-2")) %>% 
  # Format site names
  mutate(site=recode(site,
                     "San Diego Bay, U.S. Navy Pier" = "San Diego Bay-Navy Pier", 
                     "Agua Hedionda Lagoon-M" = "Agua Hedionda Lagoon",         
                     "Santa Catalina Ch., Plt. Edith"="Santa Catalina Channel",
                     "Santa Barbara Ch., M-653-02-M"  = "Santa Barbara Channel",
                     "Morro Bay, Lease M-614-01 P1-O" = "Morro Bay",
                     "Elkhorn Slough, Seal Bend" = "Elkhorn Slough",
                     # "Pescadero State Beach"          
                     # "China Beach"                    
                     # "Drakes Bay"                    
                     "Tomales Bay, Lease #M430-02" = "Tomales Bay",   
                     "Humboldt Bay, East Bay 1-1" = "Humboldt Bay",  
                     # "Point St. George"               
                     "Drakes Estero, Bed #12" = "Drakes Estero"))
  
# Plot data
g <- ggplot(data_c, aes(x=date, y=lat_dd, color=comm_name, text=site)) +
  # 2025
  geom_vline(xintercept=lubridate::ymd("2024-01-01")) +
  # Lines
  geom_hline(yintercept=lines_c$lat_dd_round, color="grey90") +
  # Data
  geom_point(pch=1) +
  # Labels
  labs(x="Date", y="Latitude (°N)") +
  scale_x_date(breaks=seq(ymd("1950-01-01"), 
                          ymd("2020-01-01"), by="10 years"),
               date_label="%Y") +
  # Y-axis
  scale_y_continuous(breaks=seq(32, 42, 1),
                     sec.axis = dup_axis(
                       breaks = lines_c$lat_dd_round,
                       labels = lines_c$site,
                       name   = NULL
                     )) +
  # Legend
  scale_color_discrete(name="") +
  # Theme
  theme_bw() + time_theme +
  theme(legend.key.size = unit(0.3, "cm"))
g

# Export figure
ggsave(g, filename=file.path(plotdir, "FigSX_ca_psp_cultured_timline.png"), 
       width=6.5, height=5, units="in", dpi=600)


# Plot wild timelines
################################################################################

# Build data
data_w <- data %>% 
  # Reduce
  filter(source_use=="wild") %>% 
  # Shorten name
  mutate(comm_name=recode(comm_name,
                          "Mediterranean/Pacific blue/blue mussels"="Unidentified mussel")) %>% 
  # Add sample size
  group_by(comm_name) %>% 
  mutate(n=n()) %>% 
  ungroup() %>% 
  mutate(comm_name_label=paste0(comm_name, " (", n, ")")) %>% 
  # Factor
  arrange(desc(n)) %>% 
  mutate(comm_name_label=factor(comm_name_label, levels=unique(comm_name_label)))

# Plot data
g <- ggplot(data_w, aes(x=date, y=lat_dd, color=comm_name_label, text=site)) +
  facet_wrap(~comm_name_label) +
  geom_point(pch=1) +
  # Labels
  labs(x="Date", y="Latitude (°N)") +
  scale_x_date(breaks=seq(ymd("1960-01-01"), 
                          ymd("2030-01-01"), by="10 years"),
               date_label="%Y") +
  scale_y_continuous(breaks=seq(32, 42, 1)) +
  # Legend
  scale_color_discrete(name="", guide="none") +
  # Theme
  theme_bw() + time_theme +
  theme(legend.key.size = unit(0.2, "cm"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.text = element_text(size=7))
g

# Export figure
ggsave(g, filename=file.path(plotdir, "FigSX_ca_psp_wild_timline.png"), 
       width=6.5, height=6, units="in", dpi=600)






# Sites
################################################################################

sites <- readxl::read_excel("/Users/cfree/Dropbox/Chris/UCSB/projects/wc_biotoxin_depuration/data/merged/processed/monitoring_sites.xlsx")


# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
world <- rnaturalearth::ne_countries(country = c("Mexico", "Canada"), returnclass = "sf", scale="small")

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
g1 <- ggplot() +
  # Plot land
  geom_sf(data=world, fill="grey90", color="white", lwd=0.2, inherit.aes = F) +
  geom_sf(data=usa, fill="grey90", color="white", lwd=0.2, inherit.aes = F) +
  # Plot sites
  geom_point(data=sites, aes(x=long_dd, lat_dd, color=type, shape=status), size=3) +
  # ggrepel::geom_text_repel(data=sites, mapping=aes(x=long_dd, lat_dd, label=name, color=type), size=2, direction="y", hjust=0) +
  # Labels
  labs(y="", x="", tag="") +
  # Legends
  scale_shape_manual(name='Status', values=c(16, 21)) +
  scale_color_discrete(name="Site type") +
  # Axes
  scale_y_continuous(breaks=seq(32, 42, 2)) +
  # Crop
  coord_sf(xlim=c(-125, -116), ylim=c(32, 42)) +
  # Theme
  theme_bw() + map_theme
g1


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
da_orig <- readRDS(file=file.path(outdir, "WC_domoic_acid_data.Rds"))
pst_orig <- readRDS(file=file.path(outdir, "WC_psp_data.Rds"))


# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
world <- rnaturalearth::ne_countries(country = c("Mexico", "Canada"), returnclass = "sf", scale="large")



# Build data
################################################################################

# Format data
da <- da_orig %>% 
  # Filter
  filter(state=='California' & date>="2015-01-01") %>% 
  # Simplify
  select(date, comm_name, lat_dd, long_dd, toxicity_ppm) %>% 
  na.omit()

# Summarize by species
da_spp <- da %>% 
  group_by(comm_name) %>% 
  summarize(n=n(),
            toxicity_ppm=max(toxicity_ppm)) %>% 
  ungroup() %>% 
  arrange(desc(n)) %>% 
  # Classify
  mutate(comm_name1=ifelse(toxicity_ppm<20 | n<40, "Other", comm_name))

# Re-summarize on broad name
da_spp1 <- da_spp %>% 
  group_by(comm_name1) %>% 
  summarize(n=sum(n)) %>% 
  ungroup() %>% 
  arrange(desc(n)) %>% 
  mutate(comm_name1=factor(comm_name1, comm_name1))

# Add recoded common name to data
da1 <- da %>% 
  left_join(da_spp %>% select(comm_name, comm_name1), by="comm_name") %>% 
  mutate(comm_name1=factor(comm_name1, levels=levels(da_spp1$comm_name1)))

# Build grid
res <- 0.25
da_gps <- da_grid <- da %>%
  mutate(long_dd = floor(long_dd / res) * res,
         lat_dd  = floor(lat_dd  / res) * res) %>%
  count(long_dd, lat_dd, name = "n")

# DA lat stats
da_lat <- da %>% 
  mutate(lat_bin=cut(lat_dd, breaks=c(32,42,0.5))) %>% 
  group_by(lat_bin) %>% 
  summarize(n=n()) %>% 
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

g1a <- ggplot(da_spp1, aes(y=comm_name1, 
                           x=n, 
                           fill=comm_name1)) +
  geom_bar(stat="identity") +
  # Labels
  labs(x="# of tests") +
  # Legend
  scale_fill_manual(name="Species",
                    values=c("sienna4", # D crab
                             "seagreen4", # CA mussel
                             "navyblue", # Pacific oyster
                             "magenta4", # Razor clam
                             "firebrick2", # Rock crab
                             "seagreen3", # Med mussel
                             "grey70", # Other
                             "lightblue", # Sardine
                             "steelblue", # Anchovy
                             "orange1" # Lobster
                    )) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position="none",
        axis.text=element_text(size=6),
        axis.title=element_text(size=7),
        axis.title.y=element_blank(),
        plot.background = element_rect(fill = NA, color = NA),
        panel.border = element_rect(color = "black", fill = "white"))
g1a

# Plot map
g1 <- ggplot() +
  # Plot data
  geom_tile(data=da_grid, mapping=aes(x=long_dd, y=lat_dd, fill=n)) +
  # Plot land
  geom_sf(data=world, fill="grey90", color="white", lwd=0.3, inherit.aes = F) +
  geom_sf(data=usa, fill="grey90", color="white", lwd=0.3, inherit.aes = F) +
  # Labels
  labs(x="", y="", tag="A") +
  # Axis
  scale_y_continuous(breaks=seq(32, 42, 1)) +
  # Legend
  scale_fill_gradientn(name="# of tests", 
                       colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev()) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", frame.linewidth = 0.2)) +
  # Crop
  coord_sf(xlim=c(-124.5, -116.8), 
           ylim=c(32.1, 42.1), 
           expand=F) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position=c(0.2, 0.15),
        legend.key.size = unit(0.3, "cm"),
        axis.text.x=element_text(color="white"),
        axis.ticks.x=element_blank(),
        axis.text.y = element_text(angle = 90, hjust = 0.5),
        axis.title.y=element_blank())
g1

g1_inset <- g1 +
  patchwork::inset_element(
    g1a,
    left   = 0.05,
    bottom = 0.6,
    right  = 0.98,
    top    = 0.98
  )
g1_inset
g1_inset_grob <- patchwork::patchworkGrob(g1_inset)


# Plot DA
g2 <- ggplot() +
  # Below 
  geom_point(da1, mapping=aes(x=date, 
                       y=lat_dd,
                       color=comm_name1,
                       size=toxicity_ppm), pch=16) +
  # Labels
  labs(x="Date", y=" ", tag="B") +
  # Y-axis
  scale_y_continuous(breaks=seq(32, 42, 1), 
                     labels=paste0(seq(32, 42, 1), "°"),
                     lim=c(32.1, 42.1),
                     expand=F) +
  # X-axis
  scale_x_date(breaks=seq(ymd("1990-01-01"), 
                          ymd("2025-01-01"), by="1 year"),
               date_label="%Y") +
  # Legend
  scale_color_manual(name="Species",
                    values=c("sienna4", # D crab
                             "seagreen4", # CA mussel
                             "navyblue", # Pacific oyster
                             "magenta4", # Razor clam
                             "firebrick2", # Rock crab
                             "seagreen3", # Med mussel
                             "grey70", # Other
                             "lightblue", # Sardine
                             "steelblue", # Anchovy
                             "orange1" # Lobster
                    )) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = "none",
        # axis.title.y=element_blank(),
        axis.text.y = element_text(angle = 90, hjust = 0.5))
g2

# Lat hist
g3 <- ggplot(da1, mapping=aes(y=lat_dd, fill=comm_name1)) +
  geom_histogram(aes(x = after_stat(count / 100)),
                 binwidth=0.2) +
  # Labels
  labs(x="100s of tests", y="", tag="C") +
  # Axes
  scale_y_continuous(lim=c(32.1, 42.1), 
                     breaks=seq(32, 42, 1),
                     labels=paste0(seq(32, 42, 1), "°"),
                     expand=F) +
  scale_fill_manual(name="Species",
                     values=c("sienna4", # D crab
                              "seagreen4", # CA mussel
                              "navyblue", # Pacific oyster
                              "magenta4", # Razor clam
                              "firebrick2", # Rock crab
                              "seagreen3", # Med mussel
                              "grey70", # Other
                              "lightblue", # Sardine
                              "steelblue", # Anchovy
                              "orange1" # Lobster
                     )) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = "none",
        axis.title.y=element_blank(),
        axis.text.y = element_text(angle = 90, hjust = 0.5))
g3

# Time histogram
g4 <- ggplot(da1, mapping=aes(x=date, fill=comm_name1)) +
  geom_histogram( binwidth = 31,   # approximate month width in days
                  boundary = 0,
                  aes(y = after_stat(count / 100))) +
  # Labels
  labs(y="100s of tests", x="Date", tag="D") +
  # Axes
  scale_x_date(breaks=seq(ymd("1990-01-01"), 
                          ymd("2025-01-01"), by="1 year"),
               date_label="%Y") +
  scale_fill_manual(name="Species",
                     values=c("sienna4", # D crab
                              "seagreen4", # CA mussel
                              "navyblue", # Pacific oyster
                              "magenta4", # Razor clam
                              "firebrick2", # Rock crab
                              "seagreen3", # Med mussel
                              "grey70", # Other
                              "lightblue", # Sardine
                              "steelblue", # Anchovy
                              "orange1" # Lobster
                     )) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = "none",
        axis.title.x=element_blank(),
        axis.text.y = element_text(angle = 90, hjust = 0.5))
g4

# Merge
layout_matrix <- matrix(data=c(NA, 4, NA,
                               1, 2, 3), byrow=T, ncol=3)
g <- gridExtra::grid.arrange(g1_inset_grob, g2, g3, g4, 
                             heights=c(0.2, 0.8),
                             widths=c(0.33, 0.54, 0.13),
                             layout_matrix=layout_matrix)

# Export
ggsave(g, filename=file.path(plotdir, "FigX_recent_data_da.png"), 
       width=8.5, height=5.5, units="in", dpi=600, bg="white")






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

# Read data
data_orig <- readRDS(file=file.path(outdir, "CDPH_1991_2024_bivalve_domoic_data.Rds"))


# Build data
################################################################################

# Species stats
stats_spp <- data_orig %>% 
  group_by(comm_name, species) %>% 
  summarize(n=n(),
            nyr=n_distinct(year),
            nsites=n_distinct(site),
            asp_ug_g_max=max(asp_ug_g)) %>% 
  ungroup() %>% 
  arrange(desc(n))

# Order data by species
data <- data_orig %>% 
  mutate(comm_name=factor(comm_name, levels=stats_spp$comm_name))


# Plot data
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
                     plot.title=element_blank(),
                     plot.tag = element_text(size=9),
                     # Gridlines
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(),
                     axis.line = element_line(colour = "black"))

# Plot data
ggplot(data, aes(x=date, y=lat_dd, color=comm_name, size=asp_ug_g)) +
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
  theme_bw() + my_theme


# Plot data
ggplot(data, aes(x=date, y=lat_dd, color=source, size=asp_ug_g)) +
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
  theme_bw() + my_theme







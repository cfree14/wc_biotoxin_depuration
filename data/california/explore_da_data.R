

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

# Read data
data_orig <- readRDS(file=file.path(outdir, "CDPH_1991_2025_all_domoic_data.Rds"))
freeR::complete(data_orig)


# Reduce to usable data
data <- data_orig 
freeR::complete(data)

data_plot <- data %>% 
  filter(domoic_ppm>=20 & date>=ymd("2020-01-01"))

ggplot(data_plot, aes(x=date, y=lat_dd, size=domoic_ppm, color=comm_name)) +
  geom_point() +
  # Labels
  labs(x="Date", y="Latitude (°N)") +
  scale_y_continuous(breaks=seq(32,50,2)) +
  scale_x_date(breaks=seq(ymd("2020-01-01"), 
                          ymd("2025-01-01"), by="1 years"),
               date_label="%Y") +
  
  # Theme
  theme_bw()


# Table
################################################################################

# Stats
stats <- data %>% 
  group_by(comm_name, species) %>% 
  summarize(type=unique(source_use) %>% paste(., collapse=", "),
            ntests=n(),
            ntests_above=sum(domoic_ppm>=20),
            nyrs=n_distinct(year),
            nyrs_above=n_distinct(year[domoic_ppm>=20])) %>% 
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



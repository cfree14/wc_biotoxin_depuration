

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



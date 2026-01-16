
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
data_da_orig <- readRDS(file=file.path(outdir, "WC_domoic_acid_data.Rds"))
data_psp_orig <- readRDS(file=file.path(outdir, "WC_psp_data.Rds"))
data_dsp_orig <- readRDS(file=file.path(outdir, "WC_dsp_data.Rds"))


# Build data
################################################################################

data_da <- data_da_orig %>% 
  filter(date>=lubridate::ymd("2020-01-1") & toxicity_ppm>=20)
data_da_low <- data_da_orig %>% 
  filter(date>=lubridate::ymd("2020-01-1") & toxicity_ppm<20)

data_psp <- data_psp_orig %>% 
  filter(date>=lubridate::ymd("2020-01-1") & toxicity_ug_100g>=80) %>% 
  mutate(comm_name=recode(comm_name, "Mediterranean/Pacific blue/blue mussels"="Unspecified mussel"))
data_psp_lo <- data_psp_orig %>% 
  filter(date>=lubridate::ymd("2020-01-1") & toxicity_ug_100g<80)

data_dsp <- data_dsp_orig %>% 
  filter(date>=lubridate::ymd("2020-01-1") & toxicity_ug_100g>=16)
data_dsp_lo <- data_dsp_orig %>% 
  filter(date>=lubridate::ymd("2020-01-1") & toxicity_ug_100g<16)

# Base theme
base_theme <- theme(axis.text=element_text(size=8),
                    axis.title=element_text(size=9),
                    legend.text=element_text(size=7),
                    legend.title=element_text(size=8),
                    plot.title=element_text(size=8),
                    plot.tag=element_text(size=9),
                    # Gridlines
                    panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(), 
                    axis.line = element_line(colour = "black"),
                    # Legend
                    legend.key.size = unit(0.3, "cm"),
                    legend.spacing = unit(-0.1, "cm"),
                    legend.key = element_rect(fill = NA, color=NA),
                    legend.background = element_rect(fill=alpha('blue', 0)))


# Domoic
################################################################################

# Plot data
g <- ggplot(data_da, aes(x=date, y=lat_dd, size=toxicity_ppm, fill=comm_name)) +
  # Data
  geom_point(data=data_da_low, aes(x=date, y=lat_dd), color="grey90", size=0.5, inherit.aes = F) +
  geom_point(alpha=0.7, color="grey30", pch=21, stroke=0.15) +
  # Year line
  geom_vline(xintercept = lubridate::ymd("2025-01-01"), color="black", linetype="dotted") +
  # State lines
  geom_hline(yintercept=c(42, 46.27)) +
  annotate(geom="text", x=lubridate::ymd("2020-01-01"), y=46.27,
           label="Washington", hjust=0, vjust=-0.7, size=2.2) +
  annotate(geom="text", x=lubridate::ymd("2020-01-01"), y=46.27,
           label="Oregon", hjust=0, vjust=1.5, size=2.2) +
  annotate(geom="text", x=lubridate::ymd("2020-01-01"), y=42,
           label="California", hjust=0, vjust=1.5, size=2.2) +
  # Labels
  labs(x="Date", y="Latitude (°N)", title="Domoic acid tests above the 20 ppm action threshold") +
  # Axis
  scale_y_continuous(breaks=seq(32,50, 2)) +
  scale_x_date(breaks=seq(ymd("2020-01-01"), 
                          ymd("2026-01-01"), by="1 year"),
               date_label="%Y") +
  # Legend
  scale_fill_discrete(name="Species") +
  scale_size_continuous(name="Toxicity (ppm)") +
  guides(fill = guide_legend(order = 1), size = guide_legend(order = 2)) +
  # Theme
  theme_bw() + base_theme +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5))
g

# Export
ggsave(g, filename=file.path(plotdir, "cciea_domoic.png"), 
       width=6.5, height=3.5, units="in", dpi=600)

# PSP
################################################################################

# Plot data
g <- ggplot(data_psp, aes(x=date, y=lat_dd, size=toxicity_ug_100g, fill=comm_name)) +
  # Data
  geom_point(data=data_psp_lo, aes(x=date, y=lat_dd), color="grey90", size=0.5, inherit.aes = F) +
  geom_point(alpha=0.7, color="grey30", pch=21, stroke=0.15) +
  # Year line
  geom_vline(xintercept = lubridate::ymd("2025-01-01"), color="black", linetype="dotted") +
  # State lines
  geom_hline(yintercept=c(42, 46.27)) +
  annotate(geom="text", x=lubridate::ymd("2020-01-01"), y=46.27,
           label="Washington", hjust=0, vjust=-0.7, size=2.2) +
  annotate(geom="text", x=lubridate::ymd("2020-01-01"), y=46.27,
           label="Oregon", hjust=0, vjust=1.5, size=2.2) +
  annotate(geom="text", x=lubridate::ymd("2020-01-01"), y=42,
           label="California", hjust=0, vjust=1.5, size=2.2) +
  # Labels
  labs(x="Date", y="Latitude (°N)", title="Paralytic shellfish toxin tests above the 80 ug/100g action threshold") +
  # Axis
  scale_y_continuous(breaks=seq(32,50, 2)) +
  scale_x_date(breaks=seq(ymd("2020-01-01"), 
                          ymd("2026-01-01"), by="1 year"),
               date_label="%Y") +
  # Legend
  scale_fill_discrete(name="Species") +
  scale_size_continuous(name="Toxicity (ug/100g)") +
  guides(fill = guide_legend(order = 1), size = guide_legend(order = 2)) +
  # Theme
  theme_bw() + base_theme +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5))
g

# Export
ggsave(g, filename=file.path(plotdir, "cciea_psp.png"), 
       width=6.5, height=3.5, units="in", dpi=600)


# DSP
################################################################################

# Plot data
g <- ggplot(data_dsp, aes(x=date, y=lat_dd, size=toxicity_ug_100g, fill=comm_name)) +
  # Data
  geom_point(data=data_dsp_lo, aes(x=date, y=lat_dd), color="grey90", size=0.5, inherit.aes = F) +
  geom_point(alpha=0.7, color="grey30", pch=21, stroke=0.15) +
  # Label state
  annotate(geom="text", x=lubridate::ymd("2020-01-01"), y=49,
           label="Washington is the only state that monitors DST in shellfish", hjust=0, vjust=-0.5, size=2.2) +
  # Year line
  geom_vline(xintercept = lubridate::ymd("2025-01-01"), color="black", linetype="dotted") +
  # Labels
  labs(x="Date", y="Latitude (°N)", title="Diarrhetic shellfish toxin tests above the 16 ug/100g action threshold") +
  # Axis
  scale_y_continuous(breaks=seq(32, 50, 0.5), lim=c(46.4, NA)) +
  scale_x_date(breaks=seq(ymd("2020-01-01"), 
                          ymd("2026-01-01"), by="1 year"),
               date_label="%Y") +
  # Legend
  scale_fill_discrete(name="Species") +
  scale_size_continuous(name="Toxicity (ug/100g)") +
  guides(fill = guide_legend(order = 1), size = guide_legend(order = 2)) +
  # Theme
  theme_bw() + base_theme +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5))
g

# Export
ggsave(g, filename=file.path(plotdir, "cciea_dsp.png"), 
       width=6.5, height=3.5, units="in", dpi=600)




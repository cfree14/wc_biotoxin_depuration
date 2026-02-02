
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
plotdir <- "figures/cciea"

# Read data
data_da_orig <- readRDS(file=file.path(outdir, "WC_domoic_acid_data.Rds"))
data_psp_orig <- readRDS(file=file.path(outdir, "WC_psp_data.Rds"))
data_dsp_orig <- readRDS(file=file.path(outdir, "WC_dsp_data.Rds"))


# Theme
################################################################################

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

# High values
data_da <- data_da_orig %>% 
  filter(date>=lubridate::ymd("2020-01-1")) %>% 
  filter((toxicity_ppm>=20 & tissue_use!="viscera") | (toxicity_ppm>=30 & tissue_use=="viscera"))

# Low values
data_da_low <- data_da_orig %>% 
  filter(date>=lubridate::ymd("2020-01-1") & !sample_id %in% data_da$sample_id)

# Stats
data_da %>% 
  count(comm_name) %>% 
  arrange(desc(n))

# Modify
da_show <- c("Razor clam", "Dungeness crab", "California mussel", 
             "California spiny lobster", "Pacific sardine", "Manila clam", "Rock crab")
data_da <- data_da %>% 
  mutate(comm_name_use=ifelse(comm_name %in% da_show, comm_name, "Other species"),
         comm_name_use=factor(comm_name_use,
                              levels=c(da_show, "Other species"))) %>% 
  arrange(comm_name_use, date)

# Plot data
g <- ggplot(data_da, aes(x=date, y=lat_dd, size=toxicity_ppm, fill=comm_name_use)) +
  # Data
  geom_point(data=data_da_low, aes(x=date, y=lat_dd), color="grey90", size=0.5, inherit.aes = F) +
  geom_point(alpha=0.7, color="grey30", pch=21, stroke=0.15) +
  # Year line
  geom_vline(xintercept = lubridate::ymd("2025-01-01"), color="black", linetype="dotted") +
  # State lines
  geom_hline(yintercept=c(42, 46.27)) +
  annotate(geom="text", x=lubridate::ymd("2020-01-01"), y=49, #46.27,
           label="Washington", hjust=0, vjust=-0.7, size=2.2) +
  annotate(geom="text", x=lubridate::ymd("2020-01-01"), y=46.27,
           label="Oregon", hjust=0, vjust=1.5, size=2.2) +
  annotate(geom="text", x=lubridate::ymd("2020-01-01"), y=42,
           label="California", hjust=0, vjust=1.5, size=2.2) +
  # Labels
  labs(x="Date", y="Latitude (°N)", title="Domoic acid tests above the relevant action threshold") +
  # Axis
  scale_y_continuous(breaks=seq(32,50, 2)) +
  scale_x_date(breaks=seq(ymd("2020-01-01"), 
                          ymd("2026-01-01"), by="1 year"),
               date_label="%Y") +
  # Legend
  scale_fill_manual(name="Species", values=RColorBrewer::brewer.pal(8, "Set2")) +
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

# High values
data_psp <- data_psp_orig %>% 
  filter(date>=lubridate::ymd("2020-01-1") & toxicity_ug_100g>=80) %>% 
  mutate(comm_name=recode(comm_name, "Mediterranean/Pacific blue/blue mussels"="Unspecified mussel"))

# Low values
data_psp_lo <- data_psp_orig %>% 
  filter(date>=lubridate::ymd("2020-01-1") & toxicity_ug_100g<80)

# Stats
data_psp %>% 
  count(comm_name) %>% 
  arrange(desc(n))

# Modify
psp_show <- c("Pacific blue mussel", "California mussel", "Butter clam", 
              "Geoduck", "Pacific oyster", "Manila clam", "Razor clam", "Purple varnish clam")
data_psp <- data_psp %>% 
  mutate(comm_name_use=ifelse(comm_name %in% psp_show, comm_name, "Other bivalve species"),
         comm_name_use=factor(comm_name_use,
                              levels=c(psp_show, "Other bivalve species"))) %>% 
  arrange(comm_name_use, date)


# Plot data
g <- ggplot(data_psp, aes(x=date, y=lat_dd, size=toxicity_ug_100g, fill=comm_name_use)) +
  # Data
  geom_point(data=data_psp_lo, aes(x=date, y=lat_dd), color="grey90", size=0.5, inherit.aes = F) +
  geom_point(alpha=0.7, color="grey30", pch=21, stroke=0.15) +
  # Year line
  geom_vline(xintercept = lubridate::ymd("2025-01-01"), color="black", linetype="dotted") +
  # State lines
  geom_hline(yintercept=c(42, 46.27)) +
  annotate(geom="text", x=lubridate::ymd("2020-01-01"), y=49, #46.27,
           label="Washington", hjust=0, vjust=-0.7, size=2.2) +
  annotate(geom="text", x=lubridate::ymd("2020-01-01"), y=46.27,
           label="Oregon", hjust=0, vjust=1.5, size=2.2) +
  annotate(geom="text", x=lubridate::ymd("2020-01-01"), y=42,
           label="California", hjust=0, vjust=1.5, size=2.2) +
  # Labels
  labs(x="Date", y="Latitude (°N)", title="Paralytic Shellfish Toxin tests above the 80 ug/100 g action threshold") +
  # Axis
  scale_y_continuous(breaks=seq(32,50, 2)) +
  scale_x_date(breaks=seq(ymd("2020-01-01"), 
                          ymd("2026-01-01"), by="1 year"),
               date_label="%Y") +
  # Legend
  scale_fill_manual(name="Species", values=RColorBrewer::brewer.pal(9, "Set1")) +
  scale_size_continuous(name="Toxicity (ug/100 g)") +
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

data_dsp <- data_dsp_orig %>% 
  filter(date>=lubridate::ymd("2020-01-1") & toxicity_ug_100g>=16)
data_dsp_lo <- data_dsp_orig %>% 
  filter(date>=lubridate::ymd("2020-01-1") & toxicity_ug_100g<16)

data_dsp %>% 
  count(comm_name) %>% 
  arrange(desc(n))

data_dsp <- data_dsp %>% 
  mutate(comm_name_use=case_when(comm_name!="Pacific blue mussel" ~ "Other bivalve species", T ~ comm_name),
         comm_name_use=factor(comm_name_use,
                              levels=c("Pacific blue mussel", "Other bivalve species"))) %>% 
  arrange(comm_name_use, date)


# Plot data
g <- ggplot(data_dsp, aes(x=date, y=lat_dd, size=toxicity_ug_100g, fill=comm_name_use)) +
  # Data
  geom_point(data=data_dsp_lo, aes(x=date, y=lat_dd), color="grey90", size=0.5, inherit.aes = F) +
  geom_point(alpha=0.7, color="grey30", pch=21, stroke=0.15) +
  # Label state
  # annotate(geom="text", x=lubridate::ymd("2020-01-01"), y=49,
  #          label="Washington is the only state that monitors DST in shellfish", hjust=0, vjust=-0.5, size=2.2) +
  # Year line
  geom_vline(xintercept = lubridate::ymd("2025-01-01"), color="black", linetype="dotted") +
  # Labels
  labs(x="Date", y="Latitude (°N)", title="Diarrhetic Shellfish Toxin tests above the 16 ug/100 g action threshold") +
  # Axis
  scale_y_continuous(breaks=seq(32, 50, 0.5), lim=c(46.4, NA)) +
  scale_x_date(breaks=seq(ymd("2020-01-01"), 
                          ymd("2026-01-01"), by="1 year"),
               date_label="%Y") +
  # Legend
  scale_fill_manual(name="Species", values=RColorBrewer::brewer.pal(2, "Set2")) +
  scale_size_continuous(name="Toxicity (ug/100 g)") +
  guides(fill = guide_legend(order = 1), size = guide_legend(order = 2)) +
  # Theme
  theme_bw() + base_theme +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5))
g

# Export
ggsave(g, filename=file.path(plotdir, "cciea_dsp.png"), 
       width=6.5, height=3.5, units="in", dpi=600)




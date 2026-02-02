
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
data_orig <- readRDS(file=file.path(outdir, "WC_psp_data.Rds"))


# Themes
################################################################################

# Build data
stats <- data_orig %>% 
  group_by(comm_name, species) %>% 
  summarize(n=n()) %>% 
  ungroup() %>% 
  # Remove generic
  filter(!is.na(comm_name)) %>% 
  filter(!grepl("spp", species)) %>% 
  filter(!grepl("/", comm_name))


# Plot figure
################################################################################


# Base theme
base_theme <- theme(axis.text=element_text(size=7),
                    axis.title=element_text(size=8),
                    legend.text=element_text(size=7),
                    legend.title=element_text(size=8),
                    strip.text=element_text(size=8),
                    plot.title=element_text(size=8),
                    plot.tag=element_text(size=9),
                    # Gridlines
                    panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(), 
                    axis.line = element_line(colour = "black"),
                    # Legend
                    legend.key = element_rect(fill = NA, color=NA),
                    legend.background = element_rect(fill=alpha('blue', 0)))
# Plot monitoring
g1 <- ggplot(data_orig, aes(x=date, y=lat_dd, size=toxicity_ug_100g, color=comm_name)) +
  geom_point() +
  # State lines
  geom_hline(yintercept=c(42, 46.27)) +
  # Labels
  labs(x="Date", y="Latitude (°N)") +
  # Axis
  scale_y_continuous(breaks=seq(32,50, 2)) +
  scale_x_date(breaks=seq(ymd("1960-01-01"), 
                          ymd("2020-01-01"), by="10 years"),
               date_label="%Y") +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = "none",
        axis.text.y = element_text(angle = 90, hjust = 0.5))
g1

# Plot species
g2 <- ggplot(stats, aes(x=n, y=reorder(comm_name, desc(n)))) +
  geom_bar(stat="identity", fill="grey70") +
  geom_vline(xintercept=50, linetype="dashed") +
  # Labels
  labs(x="Number of tests", y="") +
  scale_x_continuous(trans="log10") +
  # Theme
  theme_bw() + base_theme
g2

# Merge
g <- gridExtra::grid.arrange(g1, g2, widths=c(0.6, 0.5))

# Export
ggsave(g, filename=file.path(plotdir, "FigX_psp_timeline_and_spp.png"), 
       width=8.5, height=5.5, units="in", dpi=600)



# # Merge (three plot layout)
# layout_matrix <- matrix(data=c(1,2,
#                                1,3), ncol=2, byrow=T)
# g <- gridExtra::grid.arrange(g1, g2, g2, 
#                              layout_matrix=layout_matrix, 
#                              widths=c(0.37, 0.63))


  
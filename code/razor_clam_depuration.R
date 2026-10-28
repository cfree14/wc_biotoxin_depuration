
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


# Build data
################################################################################

# Depuration rate (k)
k <- 0.005

# Compute half life
log(2)/k

# DA to eval
toxicities <- 21:500
action_level <- 20

# Calculate days to 20 ppm action thresh
days2clean <- log(toxicities/action_level)/k
  
# Build data
data <- tibble(toxicity=toxicities,
               days2clean=days2clean,
               months2clean=days2clean/(365/12))

# Highlight points
data_pts <- data %>% 
  filter(toxicity %in% c(50, seq(100, 400, 100))) %>% 
  mutate(label=paste(round(months2clean, 0), "months"))


# Plot data
################################################################################

# Base theme
base_theme <- theme(axis.text=element_text(size=8),
                    axis.title=element_text(size=9),
                    legend.text=element_text(size=8),
                    legend.title=element_text(size=9),
                    plot.tag = element_text(size=10),
                    # Gridlines
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(),
                    axis.line = element_line(colour = "black"),
                    # Legend
                    legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g <- ggplot(data, aes(x=toxicity, y=months2clean)) +
  geom_line() +
  # Plot points
  geom_point(data=data_pts, size=1.5) +
  geom_text(data=data_pts, mapping=aes(label=label),
            size=2.2, hjust=-0.2, vjust=1.2) +
  # Labels
  labs(x="Maximum domoic acid (ppm)", y="Months until the first clean test") +
  # Axes
  scale_x_continuous(breaks=c(0, 20, seq(50, 500, 50))) +
  scale_y_continuous(breaks=seq(0, 20, 2)) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = c(0.2,0.8),
        legend.key.size = unit(0.4, "cm"))
g

# Export 
ggsave(g, filename=file.path(plotdir, "razor_clam_depuration.png"), 
       width=3.5, height=2.5, units="in", dpi=600)



  
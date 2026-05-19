
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(stringr)
library(tidyverse)
library(ggbreak)


# Directories
outdir <- "data/merged/processed"
tabledir <- "tables"
plotdir <- "figures"

# Read data
da_orig <- readRDS(file=file.path(outdir, "WC_domoic_acid_data.Rds"))
pst_orig <- readRDS(file=file.path(outdir, "WC_psp_data.Rds"))
dsp_orig <- readRDS(file=file.path(outdir, "WC_dsp_data.Rds"))


# Build data
################################################################################

# DA stats
da_stats <- da_orig %>% 
  count(state, comm_name, species) %>% 
  # Remove general species
  filter(!grepl("spp.|/", species)) %>% 
  # Order state
  mutate(state=factor(state, levels=c("California", "Oregon", "Washington") %>% rev())) %>% 
  # Calculate total n
  group_by(comm_name) %>% 
  mutate(n_tot=sum(n)) %>% 
  ungroup() %>% 
  # Classify
  filter(n_tot>=50) %>% 
  mutate(class=ifelse(n_tot>1000, ">1000 tests", "<1000 tests"))

# PST
pst_stats <- pst_orig %>% 
  count(state, comm_name, species) %>% 
  # Remove general species
  filter(!grepl("spp.|/", species)) %>% 
  # Order state
  mutate(state=factor(state, levels=c("California", "Oregon", "Washington") %>% rev())) %>% 
  # Calculate total n
  group_by(comm_name) %>% 
  mutate(n_tot=sum(n)) %>% 
  ungroup() %>% 
  # Classify
  filter(n_tot>=50) %>% 
  mutate(class=ifelse(n_tot>1000, ">1000 tests", "<1000 tests"))

# DST
dst_stats <- dsp_orig %>% 
  count(state, comm_name, species) %>% 
  # Remove general species
  filter(!grepl("spp.|/", species)) %>% 
  # Order state
  mutate(state=factor(state, levels=c("California", "Oregon", "Washington") %>% rev())) %>% 
  # Calculate total n
  group_by(comm_name) %>% 
  mutate(n_tot=sum(n)) %>% 
  ungroup() %>% 
  # Classify
  filter(n_tot>=50) %>% 
  mutate(class=ifelse(n_tot>1000, ">1000 tests", "<1000 tests"))


species <- c(dst_stats$comm_name, pst_stats$comm_name, da_stats$comm_name) %>% 
  unique()
n_distinct(species)

# Plot data
################################################################################

# Base theme
base_theme <- theme(axis.text=element_text(size=6),
                    axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
                    axis.title=element_text(size=7),
                    legend.text=element_text(size=6),
                    legend.title=element_text(size=7),
                    strip.text=element_text(size=7),
                    plot.title=element_text(size=7),
                    plot.tag=element_text(size=8),
                    # Gridlines
                    panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(), 
                    axis.line = element_line(colour = "black"),
                    # Legend
                    legend.key = element_rect(fill = NA, color=NA),
                    legend.background = element_rect(fill=alpha('blue', 0)))

# Plot
g1 <- ggplot(da_stats %>% filter(class=="<1000 tests"), aes(x=n, 
                                                            y=reorder(comm_name, desc(n_tot)), 
                                                            fill=state)) +
  facet_grid(class~., scales="free", space="free") +
  geom_bar(stat="identity", alpha=0.8) +
  # Labels
  labs(x="Number of tests", y="", tag="A", title="Domoic acid") +
  # Legend
  scale_fill_manual(name="", values=c("#1B5E20", "#0B3D91", "#B22234")) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = c(0.6, 0.9),
        legend.key.size=unit(0.3, "cm"))
g1

g2 <- ggplot(da_stats %>% filter(class==">1000 tests"), aes(x=n, 
                     y=reorder(comm_name, desc(n_tot)), 
                     fill=state)) +
  facet_grid(class~., scales="free", space="free") +
  geom_bar(stat="identity", alpha=0.8) +
  # Labels
  labs(x="Number of tests", y="") +
  # Legend
  scale_fill_manual(name="State", values=c("#1B5E20", "#0B3D91", "#B22234")) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = "none")
g2


# Plot
g3 <- ggplot(pst_stats %>% filter(class=="<1000 tests"), aes(x=n, 
                                                            y=reorder(comm_name, desc(n_tot)), 
                                                            fill=state)) +
  facet_grid(class~., scales="free", space="free") +
  geom_bar(stat="identity", alpha=0.8) +
  # Labels
  labs(x="Number of tests", y="", tag="B", title="Paralytic shellfish toxin") +
  # Legend
  scale_fill_manual(name="State", values=c("#1B5E20", "#0B3D91", "#B22234")) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = "none")
g3

g4 <- ggplot(pst_stats %>% filter(class==">1000 tests"), aes(x=n, 
                                                            y=reorder(comm_name, desc(n_tot)), 
                                                            fill=state)) +
  facet_grid(class~., scales="free", space="free") +
  geom_bar(stat="identity", alpha=0.8) +
  # Labels
  labs(x="Number of tests", y="") +
  # Legend
  scale_fill_manual(name="State", values=c("#1B5E20", "#0B3D91", "#B22234")) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = "none")
g4


# Plot
g5 <- ggplot(dst_stats %>% filter(class=="<1000 tests"), aes(x=n, 
                                                             y=reorder(comm_name, desc(n_tot)), 
                                                             fill=state)) +
  facet_grid(class~., scales="free", space="free") +
  geom_bar(stat="identity", alpha=0.8) +
  # Labels
  labs(x="Number of tests", y="", tag="C", title="Diarrhetic shellfish toxin") +
  # Legend
  scale_fill_manual(name="State", values=c("#1B5E20", "#0B3D91", "#B22234")) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = "none")
g5

g6 <- ggplot(dst_stats %>% filter(class==">1000 tests"), aes(x=n, 
                                                             y=reorder(comm_name, desc(n_tot)), 
                                                             fill=state)) +
  facet_grid(class~., scales="free", space="free") +
  geom_bar(stat="identity", alpha=0.8) +
  # Labels
  labs(x="Number of tests", y="") +
  # Legend
  scale_fill_manual(name="State", values=c("#1B5E20", "#0B3D91", "#B22234")) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = "none")
g6

# Merge panels
gA <- gridExtra::grid.arrange(g1, g2, ncol=1,
                        heights=c(0.63,0.37))
gB <- gridExtra::grid.arrange(g3, g4, ncol=1,
                              heights=c(0.55,0.45))
gC <- gridExtra::grid.arrange(g5, g6, ncol=1,
                              heights=c(0.6,0.4))

# Merge
g <- gridExtra::grid.arrange(gA, gB, gC, nrow=1)

# Export
ggsave(g, filename=file.path(plotdir, "FigX_test_sample_size.png"), 
       width=6.5, height=4, units="in", dpi=600, bg="white")






# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
datadir <- "data/closures/data"

# Read data
data_orig <- readxl::read_excel(file.path(datadir, "2026_California_HAB_closure_database.xlsx"))


# Format data
################################################################################

# Format data
data <- data_orig %>% 
  # Clean names
  janitor::clean_names("snake") %>% 
  rename(action_type=action_type_closure_advisory,
         action=action_close_open,
         where=where_south_north,
         lat_dd_s=s_latitude_n,
         lat_dd_n=n_latitude_n,
         comm_name=species)
         

# Inspect
str(data)

# Build species/toxins/fisheries
build_list <- data %>% 
  select(comm_name, fishery_type, reason) %>% 
  unique()



# Function to build and plot dat
################################################################################

species <- "Mussels"
build_data <- function(species){
  
  # Date range
  date1 <- min(data$date)
  date2 <- max(data$date)
  
  # Subset data
  sdata <- data %>% 
    # Filter to species of interest
    filter(grepl(tolower(species), tolower(comm_name)))
  
  # Plot data
  ggplot() +
    geom_segment(data=sdata, mapping=aes(x=date, y=lat_dd_s, yend=lat_dd_n, xend=date, 
                                         color=reason, linetype=action)) +
    # Labels
    labs(y="Latitude (°N)", x="Date", title=species) +
    # Date axis
    scale_x_date(lim=c(ymd(date1), ymd(date2))) +
    # Theme
    theme_bw()
  
}

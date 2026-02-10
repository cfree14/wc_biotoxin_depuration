
# Packages
library(shiny)
library(dplyr)
library(ggplot2)

# Read data
data_orig <- readRDS("data/biotoxin_data.Rds") # when app
# data_orig <- readRDS("shiny_app/data/biotoxin_data.Rds")

# Species
species_vec <- sort(unique(data_orig$comm_name))


# UI
################################################################################

# UI
ui <- fluidPage(
  
  # Title
  titlePanel("U.S. West Coast seafood biotoxin explorer"),
  
  # Layout
  sidebarLayout(
    
    # Sidebar
    sidebarPanel(
      
      # Toxin selector
      selectInput(
        inputId = "toxin",
        label   = "Toxin",
        choices = c("Domoic acid", "Paralytic shellfish toxin", "Diarrhetic shellfish toxin"),
        selected = "Domoic acid"
      ),
      
      # Species selector
      # Will be updated dynamically based on toxin
      selectInput(
        inputId = "species",
        label   = "Species",
        choices = character(0)
      ),
      
      # Year selector
      sliderInput(
        inputId = "year",
        label   = "Year range",
        min   = 1960,
        max    = 2026,
        value = c(1960, 2026),
        step=1,
        sep = "" # no thousands seperator
      ),
      
      # Lat selector
      sliderInput(
        inputId = "lat_range",
        label   = "Latitudinal range (°N)",
        min     = 32,
        max     = 50,
        value   = c(32, 50),
        step    = 0.1
      ),
      
      # Zoom in/out buttons
      fluidRow(
        column(
          6,
          actionButton("zoom_data", "Zoom to data", width = "100%")
        ),
        column(
          6,
          actionButton("zoom_full", "Zoom out", width = "100%")
        )
      ),
      br(),
      
      # Helper text
      helpText("Points are filtered by toxin, species, date range, and latitude range.")
      
    ),
    
    # Plot panel
    mainPanel(
      
      # Plot graphic
      plotOutput("obs_plot", height = "500px", width="700px"),
      
    )
    
  )
  
)


# Server
################################################################################

# Server
server <- function(input, output, session) {
  
  # Reactive table of counts per species given toxin + year + latitude filters
  sp_counts <- reactive({
    req(input$toxin, input$year, input$lat_range)
    
    data_orig %>%
      filter(
        toxin == input$toxin,
        year >= input$year[1],
        year <= input$year[2],
        lat_dd >= input$lat_range[1],
        lat_dd <= input$lat_range[2]
      ) %>%
      count(comm_name, name = "n") %>%
      arrange(desc(n), comm_name)
  })
  
  # Update species choices whenever toxin/year/lat change
  observeEvent(list(input$toxin, input$year, input$lat_range), {
    
    sp <- sp_counts()
    
    # If nothing matches, keep dropdown but show empty
    if (nrow(sp) == 0) {
      updateSelectInput(session, "species", choices = character(0), selected = character(0))
      return()
    }
    
    # Preserve current selection if it still exists; otherwise choose top species
    current <- isolate(input$species)
    selected <- if (!is.null(current) && current %in% sp$comm_name) current else sp$comm_name[1]
    
    choices_named <- setNames(
      sp$comm_name,
      paste0(sp$comm_name, " (n=", format(sp$n, big.mark = ","), ")")
    )
    
    updateSelectInput(
      session,
      inputId = "species",
      choices = choices_named,
      selected = selected
    )
    
  }, ignoreInit = FALSE)
  
  # 1) Update species list whenever toxin changes
  # observeEvent(input$toxin, {
  #   sp <- data_orig %>%
  #     filter(toxin == input$toxin) %>%
  #     count(comm_name, name = "n") %>%
  #     arrange(desc(n), comm_name)
  #   
  #   choices_named <- setNames(
  #     sp$comm_name,
  #     paste0(sp$comm_name, " (n=", format(sp$n, big.mark = ","), ")")
  #   )
  #   
  #   updateSelectInput(
  #     session,
  #     inputId = "species",
  #     choices = choices_named,
  #     selected = sp$comm_name[1]
  #   )
  # }, ignoreInit = FALSE)
  
  # 2) Reactive filtered data (drives the plot)
  data_filt <- reactive({
    req(input$toxin, input$species, input$year, input$lat_range)
    
    data_orig %>%
      filter(
        toxin == input$toxin,
        comm_name == input$species,
        year >= input$year[1],
        year <= input$year[2],
        lat_dd >= input$lat_range[1],
        lat_dd <= input$lat_range[2]
      )
  })
  
  # Compute data extent for toxin-species
  extent_ts <- reactive({
    req(input$toxin, input$species)
    
    df_ts <- data_orig %>%
      filter(toxin == input$toxin, comm_name == input$species) %>%
      filter(!is.na(date), !is.na(lat_dd))
    
    if (nrow(df_ts) == 0) return(NULL)
    
    list(
      year_min = min(df_ts$year, na.rm = TRUE),
      year_max = max(df_ts$year, na.rm = TRUE),
      lat_min  = min(df_ts$lat_dd, na.rm = TRUE),
      lat_max  = max(df_ts$lat_dd, na.rm = TRUE)
    )
  })
  
  # Zoom to toxin-species data extent
  observeEvent(input$zoom_data, {
    ex <- extent_ts()
    req(ex)
    
    # Optional padding so points aren't right on the boundary
    pad_year <- 0      # change to 1 if you want a 1-year pad
    pad_lat  <- 0.2
    
    yr_min <- max(1960, floor(ex$year_min - pad_year))
    yr_max <- min(2026, ceiling(ex$year_max + pad_year))
    
    lat_min <- max(32, ex$lat_min - pad_lat)
    lat_max <- min(50, ex$lat_max + pad_lat)
    
    updateSliderInput(session, "year", value = c(yr_min, yr_max))
    updateSliderInput(session, "lat_range", value = c(lat_min, lat_max))
  })
  
  # Zoom out to full extent
  observeEvent(input$zoom_full, {
    updateSliderInput(session, "year", value = c(1960, 2026))
    updateSliderInput(session, "lat_range", value = c(32, 50))
  })
  
  # Plot the data
  output$obs_plot <- renderPlot({
    
    # Filter data
    df <- data_filt()
    
    # Run validation
    validate(
      need(nrow(df) > 0, "No data match the selected filters.")
    )
    
    year_break_step <- function(year_span) {
      if (year_span < 10)  return(1)
      if (year_span < 20)  return(2)
      return(5)
    }
    
    lat_break_step <- function(lat_span) {
      if (lat_span < 2)   return(0.25)
      if (lat_span < 6)   return(0.5)
      if (lat_span < 12)  return(1)
      if (lat_span < 20)  return(2)
      return(5)
    }
    
    yr_min <- input$year[1]
    yr_max <- input$year[2]
    yr_span <- yr_max - yr_min
    yr_step <- year_break_step(yr_span)
    
    lat_min <- input$lat_range[1]
    lat_max <- input$lat_range[2]
    lat_span <- lat_max - lat_min
    lat_step <- lat_break_step(lat_span)
    
    # Toxicity title
    tox_title <- ifelse(input$toxin=="Domoic acid", "Toxicity (ppm)", "Toxicity (ug/100g)")
    
    # Plot data
    ggplot(df, aes(x = date, y = lat_dd, size = toxicity, fill = toxicity)) +
      # State lines
      annotate(geom="text", 
               x=as.Date(paste0(input$year[1], "-01-01")),
               y=c(42, 45, 49),
               label=c("California", "Oregon", "Washington"),
               hjust=0,
               vjust=1.5) +
      geom_hline(yintercept=c(42, 45, 49)) +
      # Points
      geom_point(pch = 21, stroke = 0.1, alpha = 0.85) +
      # Labels
      labs(x = "Date", y = "Latitude (°N)") +
      # Latitude axis
      scale_y_continuous(breaks = seq(32, 
                                      50, 
                                      lat_step),
                         lim=c(input$lat_range[1], input$lat_range[2])) +
      # Date axis
      scale_x_date(
        lim=c( as.Date(paste0(input$year[1], "-01-01")),
               as.Date(paste0(input$year[2], "-01-01"))),
        breaks = seq(
          as.Date(paste0(input$year[1], "-01-01")),
          as.Date(paste0(input$year[2], "-01-01")),
          by = paste0(yr_step, " years")
        ),
        date_labels = "%Y"
      ) +
      # Legends
      scale_size_continuous(name =  tox_title) +
      scale_fill_gradientn(
        name =  tox_title,
        colors = RColorBrewer::brewer.pal(9, "Spectral") %>% rev()
      ) +
      guides(fill = guide_colorbar(
        ticks.colour = "black",
        frame.colour = "black",
        frame.linewidth = 0.2
      )) +
      # Theme
      theme_bw() +
      theme(axis.text=element_text(size=12),
            axis.title=element_text(size=13),
            legend.text=element_text(size=12),
            legend.title=element_text(size=13),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank())
    
  })
  
}

# Build app
shinyApp(ui, server)

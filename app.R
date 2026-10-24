# Load required packages
library(shiny)
library(bslib)
library(readr)
library(DT)

# Load data ----

cephs <- read_csv(file = 'data/cephs.csv')

# Vector of numeric variables that the user can select from
selNumVar <- c('CEPH_COUNT', 'S_LAT', 'S_LON', 'VOL_FILT', 'DEPTH_IBEG', 
               'DEPTH_IEND', 'DEPTH_SSTA', 'TEMP_SSURF')

# Set user-friendly names for these variables
names(selNumVar) <- c('Number of Cephalopods', 'Latitude', 'Longitude', 
                      'Volume Filtered', 'Beginning Depth', 'End Depth',
                      'Water Depth', 'Sea Surface Temperature')

# Define UI ----
ui <- page_sidebar(
  
  # App Title
  title = "Cephalopd Sampling in the Gulf of Mexico and Caribbean Sea",
  
  # Sidebar
  sidebar = sidebar(
    
    # Widget to subset from a categorical variable
    # Select which years to subset
    checkboxGroupInput(
      inputId = "selected_years",
      label = "Select sampling years:",
      choices = as.character(c(unique(cephs$YEAR))),
      selected = '2007'
    ),
    
    # Widget to subset from a categorical variable
    # Select which seasons to subset
    checkboxGroupInput(
      inputId = "selected_seasons",
      label = "Select sampling seasons:",
      choices = c('Winter', 'Spring', 'Late Summer/Early Fall'),
      selected = 'Winter'
    ),
    
    # Widget to select a numeric variable
    selectInput(
      inputId = "num_var1",
      label = "Select a numeric variable",
      choices = names(selNumVar),
      selected = names(selNumVar)[1]
    ),
    
    # Dynamic UI slider to subset on that numeric variable once it's selected
    
    # Widget to select a second numeric variable
    
    # Dynamic slider to filter by lower and upper bound
    
    # Action button to subset the data based on the selections made in the widgets
    
    
  ),
  
  # Main Panel
  
  navset_card_pill(
    placement = "above",
    
    # About tab
    nav_panel(title = "About", 
              p("Describe the purpose of the app"),
              p("Description of the data and its source"),
              a("SEAMAP", href = "https://seamap.org/data-portal/"),
              p("Tell the user the purpose of the sidebar and each tab (main panel section) in the app"),
              img(height = 300, width = 400, src = "squid.jpg"),
              tags$caption("Squid paralarvae")
              ),
    
    # Data Download tab
    nav_panel(title = "Data Download", 
              # Display data table
              DTOutput(outputId = "cephTable"),
              # Have a download button to save the data
              downloadButton(outputId = 'downloadData', label = "Download")
              ),
    
    # Data Exploration tab (numeric and graphical summaries)
    nav_panel(title = "Data Exploration", 
              p("Third tab content")
              ),
    
    # Adding spacing between nav items
    nav_spacer()
  )
  



)

# Define server logic ----
server <- function(input, output) {
  
  
  # Create reactive data frame
  samples_selected <- reactive({
    cephs |>
      filter(input$selected_years)
  })

  # # Create data table
  # output$cephTable <- DT::renderDataTable({
  #   req(input$selected_var)
  #   datatable(
  #     data = samples_selected(),
  #     options = list(pageLength = 10),
  #     rownames = FALSE
  #   )
  # })
  # 
  # # Download data when the download button is clicked
  # output$downloadData <- downloadHandler(
  #   filename = function() {
  #     paste0('selected-cephalopod-data.csv')
  #     },
  #   content = function(file) {
  #     write.csv(samples_selected(), file)
  #     }
  #   )
  
}

# Run the app ----
shinyApp(ui = ui, server = server)
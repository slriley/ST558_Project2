# Load required packages
library(shiny)
library(bslib)
library(readr)
library(DT)
library(tidyr)
library(dplyr)
library(tibble)

# Load data ----

cephs <- as_tibble(read_csv(file = 'data/cephs.csv'))

# Vector of categorical variables that the user can select from
selCatVar <- c('CRUISE_NO', 'GEAR_LOC', 'SEASON', 'fYEAR', 'fMONTH')

# Set user-friendly names for these variables
names(selCatVar) <- c('Cruise Number', 'Gear Location', 'Season', 'Year', 
                      'Month')

# Vector of numeric variables that the user can select from
selNumVar <- c('CEPH_COUNT', 'S_LAT', 'S_LON', 'VOL_FILT', 'DEPTH_IBEG', 
               'DEPTH_IEND', 'DEPTH_SSTA', 'TEMP_SSURF')

# Set user-friendly names for these variables
names(selNumVar) <- c('Number of Cephalopods', 'Latitude', 'Longitude', 
                      'Volume Filtered', 'Beginning Depth', 'End Depth',
                      'Water Depth', 'Sea Surface Temperature')

# Define UI ----
ui <- fluidPage(
  
  # App Title
  titlePanel("Cephalopd Sampling in the Gulf of Mexico and Caribbean Sea"),
  
  # Sidebar
  sidebarPanel(
    
    # Widget to subset from a categorical variable
    # Select which years to subset
    checkboxGroupInput(
      inputId = "selected_years",
      label = "Select sampling years:",
      choices = as.character(c(unique(cephs$YEAR))),
      selected = as.character(c(unique(cephs$YEAR)))
    ),
    
    # Widget to subset from a categorical variable
    # Select which seasons to subset
    checkboxGroupInput(
      inputId = "selected_seasons",
      label = "Select sampling seasons:",
      choices = c('Winter', 'Spring', 'Late Summer/Early Fall'),
      selected = c('Winter', 'Spring', 'Late Summer/Early Fall')
    ),
    
    # Widget to select a numeric variable
    selectInput(
      inputId = "num_var1",
      label = "Select a numeric variable:",
      choices = names(selNumVar),
      selected = names(selNumVar)[1]
    ),
    
    # Dynamic UI slider to subset on that numeric variable once it's selected
    uiOutput("numVar1_slider"),
    
    # Widget to select a second numeric variable
    selectInput(
      inputId = "num_var2",
      label = "Select a second numeric variable:",
      choices = names(selNumVar),
      selected = names(selNumVar)[2]
    ),
    
    # Dynamic slider to filter by lower and upper bound
    uiOutput("numVar2_slider"),
    
    # Action button to subset the data based on the selections made in the widgets
    actionButton("subsetData", label = "Subset data")
    
    
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
              card(DTOutput(outputId = "cephTable")),
              # Have a download button to save the data
              card(downloadButton(outputId = 'downloadData', label = "Download"))
              ),
    
    # Data Exploration tab (numeric and graphical summaries)
    nav_panel(title = "Data Exploration", 
              sidebarPanel(
                # Section header
                h2("Summary statistics"),
                # Select which type of data summary to display
                radioButtons(inputId = 'varType',
                             label = 'Choose which data summaries to display:',
                             choices = c('Categorical', 'Numeric')
                ),
                # Dynamic widget to select which categorical variables to put in
                # the contingency tables
                conditionalPanel(
                  condition = "input.varType == 'Categorical'",
                  selectInput(inputId = 'conTableVar',
                              label = 'Choose up to two categorical variables to summarize',
                              choices = names(selCatVar),
                              selected = NULL,
                              multiple = TRUE
                  )
                ),
                
                # Dynamic widget to select which numeric variables to display
                # summary statistics
                conditionalPanel(
                  condition = "input.varType == 'Numeric'",
                  selectInput(inputId = 'sumStats',
                              label = 'Choose which numeric variables to summarize',
                              choices = names(selNumVar),
                              selected = NULL,
                              multiple = TRUE
                  )
                ),
                
                # Dynamic widget to select which categorical variables to display
                # the summary statistics of the numeric variable across its levels
                conditionalPanel(
                  condition = "input.varType == 'Numeric'",
                  selectInput(inputId = 'catVarAcross',
                              label = 'Choose which categorical variable to 
                              calculate summary statistics across',
                              choices = names(selCatVar),
                              selected = NULL,
                              multiple = FALSE
                  )
                ),
                
                # Action button to produce summary
                actionButton(inputId = 'produceSum',
                             label = 'Produce data summary'),

                
                # Section header
                h2("Graphs"),
                # Select which categorical variables to plot
                selectInput(inputId = 'plotCatVar',
                            label = 'Choose which categorical variables to plot, if any',
                            choices = names(selCatVar),
                            selected = NULL,
                            multiple = TRUE
                            ),
                # Select which numeric variables to plot
                selectInput(inputId = 'plotNumVar',
                            label = 'Choose which numeric variables to plot, if any',
                            choices = names(selNumVar),
                            selected = NULL,
                            multiple = TRUE
                )                
              ),
              
              
              # Display contingency table or summary statistics table
              card(DTOutput(outputId = "conTable")), 
              card(DTOutput(outputId = "sumTable"))
              
    ),

    
    # Adding spacing between nav items
    nav_spacer()
  )
  



)

# Define server logic ----
server <- function(input, output, session) {
  
  # Slider for first numeric variable
  output$numVar1_slider <- renderUI({
    sliderInput(inputId = "numVar1_values",
                label = "Select upper and lower bounds:",
                min = round(min(cephs[unname(selNumVar[input$num_var1])], 
                                na.rm = TRUE), 2),
                max = round(max(cephs[unname(selNumVar[input$num_var1])],
                                na.rm = TRUE), 2),
                value = c(round(min(cephs[unname(selNumVar[input$num_var1])],
                                    na.rm = TRUE), 2),
                          round(max(cephs[unname(selNumVar[input$num_var1])],
                                    na.rm = TRUE), 2))
                )
  })
  
  # Slider for second numeric variable
  output$numVar2_slider <- renderUI({
    sliderInput(inputId = "numVar2_values",
                label = "Select upper and lower bounds:",
                min = round(min(cephs[unname(selNumVar[input$num_var2])], 
                                na.rm = TRUE), 2),
                max = round(max(cephs[unname(selNumVar[input$num_var2])],
                                na.rm = TRUE), 2),
                value = c(round(min(cephs[unname(selNumVar[input$num_var2])],
                                    na.rm = TRUE), 2),
                          round(max(cephs[unname(selNumVar[input$num_var2])],
                                    na.rm = TRUE), 2))
                )
  })
  
  # Create reactive data frame when the action button is clicked
  samples_selected <- eventReactive(input$subsetData, {
    # Subset the data by:
    cephs |>
      # Year and season
      filter(YEAR %in% input$selected_years,
             SEASON %in% input$selected_seasons) |>
      # The first user-selected numeric variable
      filter(between(get(unname(selNumVar[input$num_var1])), input$numVar1_values[1], input$numVar1_values[2])) |>
      # The second user-selected numeric variable
      filter(between(get(unname(selNumVar[input$num_var2])), input$numVar2_values[1], input$numVar2_values[2]))
  })

  # Create data table
  output$cephTable <- DT::renderDataTable({
    req(input$subsetData)
    datatable(
      data = samples_selected(),
      options = list(pageLength = 10),
      rownames = FALSE
    )
  })
  
  # Download data when the download button is clicked
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0('selected-cephalopod-data.csv')
      },
    content = function(file) {
      write.csv(samples_selected(), file)
      }
    )
  
  # Create contingency table when the action button is clicked and categorical 
  # variables are selected
  conTblR <- reactive({
    if (input$varType == 'Categorical') {
      input$produceSum
      if (length(input$conTableVar) == 1){
            samples_selected() |>
              group_by(get(unname(selCatVar[input$conTableVar]))) |>
              summarize(count = n()) |>
              #rename(get(unname(selCatVar[input$conTableVar])) = `get(unname(selCatVar[input$conTableVar))`) |>
              remove_rownames()
      } else if (length(input$conTableVar) == 2){
        samples_selected() |>
          group_by(get(unname(selCatVar[input$conTableVar[1]])), 
                   get(unname(selCatVar[input$conTableVar[2]]))) |>
          summarize(count = n()) |>
          pivot_wider(names_from = `get(unname(selCatVar[input$conTableVar[1]]))`, 
                      values_from = count) |>
          remove_rownames()
        }
    }
  })
  
  # Send contingency table to UI
  output$conTable <- renderDataTable({ conTblR() })
  

  
  # Create summary statistics table when the action button is clicked and 
  # numeric variables are selected
  sumTblR <- reactive({
    if (input$varType == 'Numeric') {
      input$produceSum
      samples_selected() |>
        group_by(get(unname(selCatVar[input$catVarAcross]))) |>
        summarize(across(all_of(unname(selNumVar[input$sumStats])), 
                         list('mean' = ~ signif(mean(.x, na.rm = TRUE), digits = 3), 
                              'median' = ~ signif(median(.x, na.rm = TRUE), digits = 3),
                              'sd' = ~ signif(sd(.x, na.rm = TRUE), digits = 3),
                              'IQR' = ~ signif(IQR(.x, na.rm = TRUE), digits = 3)), 
                         .names = "{.fn}_{.col}"),
                  count = n())
    }
  })
  
  # Send summary statistics table to UI
  output$sumTable <- renderDataTable({ sumTblR() })
  
  
}

# Run the app ----
shinyApp(ui = ui, server = server)
# Load required packages
library(shiny)
library(bslib)
library(readr)
library(DT)
library(tidyr)
library(dplyr)
library(tibble)
library(ggplot2)

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
              p("This Shiny app allows the user to explore data from the 
                Southeast Monitoring and Assessment Program's plankton 
                collection surveys."),
              p("The data explored by this app are from the Southeast Area 
                Monitoring and Assessment Program, a consortium which conducts
                research in the Gulf of Mexico. The data consist of information 
                about plankton collection surveys and give details about the 
                net deployments. Finally, the number of paralarval cephalopods 
                collected in each sample is also included in the data."),
              a("Learn more about the SEAMAP program here.", 
                href = "https://seamap.org/data-portal/"),
              p("The sidebar widgets allow the user to select which years and 
              seasons to include in the data, as well as select other variables 
              to filter the data, and then subset the data for further use."),
              p("The Data Download tab allows the user to view the filtered data
                and download it if desired."),
              p("The Data Exploration tab allows the user to explore the 
                filtered data by making selections in the tab's widgets."),
              img(height = 300, width = 400, src = "squid.jpg"),
              tags$caption("Squid paralarvae (Photo Credit: Shannon Riley, 2022)")
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
                # Select which variables to plot
                selectInput(inputId = 'plotVar',
                            label = 'Choose up to three variables to plot:',
                            choices = c(names(selCatVar), names(selNumVar)),
                            selected = NULL,
                            multiple = TRUE
                            ),
                
                # Don't need at the moment
                # Check why it's not working
                # verbatimTextOutput("check"),
                
                # Dynamic UI radio buttons to choose data distribution plot type
                uiOutput("histDens"),
                
                # Dynamic UI radio buttons to choose bivariate barplot type
                uiOutput("whichBar"),
                
                # Dynamic UI radio buttons to choose boxplot type
                uiOutput("whichBox"),
                
                # Dynamic UI radio buttons to choose faceted boxplot type
                uiOutput("whichTriBox"),
                
                # Action button to produce plot
                actionButton(inputId = 'producePlot',
                             label = 'Produce graphs'),
              ),
              

# Data Exploration tab displays -------------------------------------------
              
              # Display message about not having this display figured out
              card(p("Please scroll down until you find the table/plot you've
                     made. I ran out of time to figure this display out.")),
              
              # Display contingency table or summary statistics table
              card(DTOutput(outputId = "conTable")),
              card(DTOutput(outputId = "sumTable")),
              
              # Display selected plot
              card(plotOutput(outputId = 'barplot')),
              card(plotOutput(outputId = 'dataDist')),
              card(plotOutput(outputId = 'biBarplot')),
              card(plotOutput(outputId = 'scatterplot')),
              card(plotOutput(outputId = 'boxplot')),
              card(plotOutput(outputId = 'catScatter')),
              card(plotOutput(outputId = 'triScatter')),
              card(plotOutput(outputId = 'facetBox')),
              
    ),

    
    # Adding spacing between nav items
    nav_spacer()
  )
)


# Define server logic ----
server <- function(input, output, session) {

# Widgets -----------------------------------------------------------------

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

  
# Data Download Tab -------------------------------------------------------

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


# Data Summaries ----------------------------------------------------------

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
  
  # don't need at the moment
  # Check input to figure out why it's not working
  # output$check <- renderText({ input$plotVar })
  

# Univariate Plots --------------------------------------------------------
  
  # Produce barplot when action button is clicked and one categorical variable 
  # is selected
  output$barplot <- renderPlot({
    # Check that button has been clicked
    req(input$producePlot)
    # Check that at least one variable has been selected to plot
    req(input$plotVar)
    if (length(input$plotVar) == 1) {
      if (input$plotVar %in% names(selCatVar)) {
        # Bar chart
        ggplot(data = samples_selected(), aes(get(unname(selCatVar[input$plotVar])))) +
          geom_bar() +
          labs(x = paste(input$plotVar),
               y = 'Number of Samples Collected') +
          theme_classic()
      }
    } 
  })
  
  
  # Produce radio buttons to select histogram or density plot if one numeric 
  # variable is selected
  output$histDens <- renderUI({
    req(input$plotVar)
    if (length(input$plotVar) == 1) {
      if (input$plotVar %in% names(selNumVar)) {
        radioButtons(inputId = "univarNum",
                     label = "Select which data distribution plot to produce:",
                     choices = c('Histogram', 'Smoothed Density Estimate'),
                     selected = 'Smoothed Density Estimate'
        )
      }
    } 
  })
  
  # Produce chosen data distribution plot when action button is clicked
  output$dataDist <- renderPlot({
    # Check that button has been clicked
    req(input$producePlot)
    # Check that at least one variable has been selected to plot
    req(input$plotVar)
    # Check that a plot type was selected
    req(input$univarNum)
    # Check that only one variable has been selected
    if (length(input$plotVar) == 1) {
      # Produce plot
      if (input$univarNum == 'Histogram') {
        # Histogram
        ggplot(data = samples_selected(), aes(get(unname(selNumVar[input$plotVar])))) +
          geom_histogram() +
          labs(x = paste(input$plotVar),
               y = 'Frequency') +
          theme_classic()
      } else if (input$univarNum == 'Smoothed Density Estimate') {
        # Density Estimates
        ggplot(data = samples_selected(), aes(get(unname(selNumVar[input$plotVar])))) +
          geom_density() +
          labs(x = paste(input$plotVar),
               y = 'Density') +
          theme_classic()
      } else {}
    }
  })
  

# Bivariate Plots ---------------------------------------------------------

  # Produce radio buttons to select stacked or side-by-side barplot if two 
  # categorical variables are selected
  output$whichBar <- renderUI({
    req(input$plotVar)
    if (length(input$plotVar) == 2) {
      if (all(input$plotVar %in% names(selCatVar))) {
        radioButtons(inputId = "bivarCat",
                     label = "Select which barplot to produce:",
                     choices = c('Stacked', 'Side-by-Side'),
                     selected = 'Stacked'
        )
      }
    } 
    
  })
  
  # Produce chosen barplot when action button is clicked
  output$biBarplot <- renderPlot({
    # Check that button has been clicked
    req(input$producePlot)
    # Check that at least one variable has been selected to plot
    req(input$plotVar)
    # Check that a plot type was selected
    req(input$bivarCat)
    # Produce plot
    if (input$bivarCat == 'Stacked') {
      # Stacked bar chart
      ggplot(data = samples_selected(), 
             aes(x = get(unname(selCatVar[input$plotVar[1]])), 
                 fill = as.factor(get(unname(selCatVar[input$plotVar[2]]))))) +
        geom_bar(position = 'stack') +
        labs(x = paste(input$plotVar[1]),
             y ='Number of Samples Collected',
             fill = paste(input$plotVar[2])) +
        theme_classic()
    } else if (input$bivarCat == 'Side-by-Side') {
      # Side-by-side bar chart
      ggplot(data = samples_selected(), 
             aes(x = get(unname(selCatVar[input$plotVar[1]])), 
                 fill = as.factor(get(unname(selCatVar[input$plotVar[2]]))))) +
        geom_bar(position = 'dodge') +
        labs(x = paste(input$plotVar[1]),
             y ='Number of Samples Collected',
             fill = paste(input$plotVar[2])) +
        theme_classic()
    } else {}

  })
  
  
  # Produce scatterplot when action button is clicked and two numeric variables 
  # are selected
  output$scatterplot <- renderPlot({
    # Check that button has been clicked
    req(input$producePlot)
    # Check that at least one variable has been selected to plot
    req(input$plotVar)
    if (length(input$plotVar) == 2) {
      if (all(input$plotVar %in% names(selNumVar))) {
        # Scatterplot
        ggplot(data = samples_selected(), aes(x = get(unname(selNumVar[input$plotVar[1]])), 
                                              y = get(unname(selNumVar[input$plotVar[2]])))) +
          geom_point() +
          labs(x = paste(input$plotVar[1]),
               y = paste(input$plotVar[2])) + 
          theme_classic()
      }
    } 
  })
  
  
  
  # Produce radio buttons to select boxplot or violin plot if one numeric and
  # one categorical variable are selected
  output$whichBox <- renderUI({
    req(input$plotVar)
    if (length(input$plotVar) == 2) {
      if (all(input$plotVar %in% names(selCatVar))) {

      } else if (all(input$plotVar %in% names(selNumVar))) {
        
      } else {
        radioButtons(inputId = "biCatNum",
                     label = "Select which boxplot to produce:",
                     choices = c('Boxplot', 'Violin Plot'),
                     selected = 'Violin Plot'
        )
      }
    } 
  })
  
  # Produce chosen boxplot when action button is clicked
  output$boxplot <- renderPlot({
    # Check that button has been clicked
    req(input$producePlot)
    # Check that at least one variable has been selected to plot
    req(input$plotVar)
    # Check that a plot type was selected
    req(input$biCatNum)
    # Check that only two variables were selected
    if (length(input$plotVar) == 2) {
      # Produce plot
      if (input$biCatNum == 'Boxplot') {
        # Box plot
        ggplot(data = samples_selected(), 
               aes(x = as.factor(get(unname(selCatVar[input$plotVar[1]]))),
                   y = get(unname(selCatVar[input$plotVar[2]])), 
                   fill = as.factor(get(unname(selCatVar[input$plotVar[1]]))))) +
          geom_boxplot() +
          labs(x = paste(input$plotVar[1]),
               y = paste(input$plotVar[2])) +
          theme_classic() +
          theme(legend.position = 'none')
      } else if (input$biCatNum == 'Violin Plot') {
        # Violin plot
        ggplot(data = samples_selected(), 
               aes(x = as.factor(get(unname(selCatVar[input$plotVar[1]]))),
                   y = get(unname(selNumVar[input$plotVar[2]])), 
                   fill = as.factor(get(unname(selCatVar[input$plotVar[1]]))))) +
          geom_violin() +
          labs(x = paste(input$plotVar[1]),
               y = paste(input$plotVar[2])) +
          theme_classic() +
          theme(legend.position = 'none')
      } else {}
      
    }
    
  })
  


# Trivariate Plots --------------------------------------------------------

  # Produce scatterplot with color when action button is clicked and two numeric
  # variables and one categorical variable are selected
  output$catScatter <- renderPlot({
    # Check that button has been clicked
    req(input$producePlot)
    # Check that at least one variable has been selected to plot
    req(input$plotVar)
    if (length(input$plotVar) == 3) {
      if (sum(input$plotVar %in% names(selCatVar)) == 1 & sum(input$plotVar %in% names(selNumVar)) == 2) {
          # Scatterplot with categorical variable
          ggplot(data = samples_selected(), 
                 aes(x = get(unname(selNumVar[input$plotVar[2]])),
                     y = get(unname(selNumVar[input$plotVar[3]])),
                     color = get(unname(selCatVar[input$plotVar[1]])))) +
            geom_point() +
            labs(x = paste(input$plotVar[2]),
                 y = paste(input$plotVar[3]),
                 color = paste(input$plotVar[1])) +
            theme_classic()
      }
    } 
  })
  
  
  # Produce scatterplot when action button is clicked and three numeric 
  # variables are selected
  output$triScatter <- renderPlot({
    # Check that button has been clicked
    req(input$producePlot)
    # Check that at least one variable has been selected to plot
    req(input$plotVar)
    if (length(input$plotVar) == 3) {
      if (all(input$plotVar %in% names(selNumVar))) {
        # Scatterplot with point size
        ggplot(data = samples_selected(), 
               aes(x = get(unname(selNumVar[input$plotVar[1]])),
                   y = get(unname(selNumVar[input$plotVar[2]])),
                   size = get(unname(selNumVar[input$plotVar[3]])))) +
          geom_point() +
          labs(x = paste(input$plotVar[1]),
               y = paste(input$plotVar[2]),
               size = paste(input$plotVar[3])) + 
          theme_classic()
      }
    } 
  })
  
  
  
  # Produce radio buttons to select boxplot or violin plot if one numeric and
  # two categorical variables are selected
  output$whichTriBox <- renderUI({
    req(input$plotVar)
    if (length(input$plotVar) == 3) {
      if (sum(input$plotVar %in% names(selCatVar)) == 2 & sum(input$plotVar %in% names(selNumVar)) == 1) {
        radioButtons(inputId = "triCatNum",
                     label = "Select which faceted boxplot to produce:",
                     choices = c('Boxplot', 'Violin Plot'),
                     selected = 'Violin Plot'
        )
      }
    } 
  })
  
  # Produce faceted boxplot or violin plot when action button is clicked and one
  # numeric variable and two categorical variables are selected
  output$facetBox <- renderPlot({
    # Check that button has been clicked
    req(input$producePlot)
    # Check that at least one variable has been selected to plot
    req(input$plotVar)
    # Check that a plot type was selected
    req(input$triCatNum)
    # Check that three variables are selected
    if (length(input$plotVar) == 3) {
      if (sum(input$plotVar %in% names(selCatVar)) == 2 & sum(input$plotVar %in% names(selNumVar)) == 1) {
        # Produce plot
        if (input$triCatNum == 'Boxplot') {
          # Box plot
          ggplot(data = samples_selected(), 
                 aes(x = as.factor(get(unname(selCatVar[input$plotVar[1]]))),
                     y = get(unname(selCatVar[input$plotVar[3]])), 
                     fill = as.factor(get(unname(selCatVar[input$plotVar[1]]))))) +
            geom_boxplot() +
            facet_wrap(vars(as.factor(get(unname(selCatVar[input$plotVar[2]]))))) +
            labs(x = NULL,
                 y = paste(input$plotVar[3]),
                 fill = paste(input$plotVar[1])) +
            theme_classic() +
            theme(
              axis.text.x = element_blank()
            )
        } else if (input$triCatNum == 'Violin Plot') {
          # Violin plot
          ggplot(data = samples_selected(), 
                 aes(x = as.factor(get(unname(selCatVar[input$plotVar[1]]))),
                     y = get(unname(selNumVar[input$plotVar[3]])), 
                     fill = as.factor(get(unname(selCatVar[input$plotVar[1]]))))) +
            geom_violin() +
            facet_wrap(vars(as.factor(get(unname(selCatVar[input$plotVar[2]]))))) +
            labs(x = NULL,
                 y = paste(input$plotVar[3]),
                 fill = paste(input$plotVar[1])) +
            theme_classic() +
            theme(
              axis.text.x = element_blank()
            )
        } else {}
      }
    } 
  })
  
  
  
  # Never mind for now - complicated - but it's really cool in the .qmd file!
  # Produce map when action button is clicked and two numeric variables 
  # are selected, one of which is latitude or longitude
  # output$map <- renderPlot({
  #   # Check that button has been clicked
  #   req(input$producePlot)
  #   # Check that at least one variable has been selected to plot
  #   req(input$plotVar)
  #   if (length(input$plotVar) == 2) {
  #     if (all(input$plotVar %in% names(selNumVar))) {
  #       if ('Latitude' %in% input$plotVar | 'Longitude' %in% input$plotVar) {
  #         
  #       }
  #     }
  #   } 
  # })
  
  
  
  
  
  
}

# Run the app ----
shinyApp(ui = ui, server = server)
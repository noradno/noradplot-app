library(shiny)
library(ggplot2)
library(jsonlite)
library(noradplot)
library(shinyjs)
library(DT)

ggnorad("default")

source("utilities.R")

ui <- fluidPage(
  useShinyjs(),  # Enable shinyjs
  
  tags$head(
    tags$style(
      HTML("
      #data {
        max-height: 300px; /* Adjust the maximum height as needed */
        overflow-y: auto;
      }
      ")
    )
  ),
  titlePanel("Noradplot generator"),
  sidebarLayout(
    sidebarPanel(width = 2,
      selectInput("x_var", "Select X Variable", ""),
      selectInput("y_var", "Select Y Variable", ""),
      selectInput("plot_type", "Select Plot Type",
                  choices = c("Bar plot", "Line plot", "Scatter plot")),
      selectInput("group_var", "Grouping variable for fill/color aesthetic", choices = NULL),
      selectInput("color_fill", "Aesthetic", choices = c("Color", "Fill")),
      textInput("title", "Plot title"),
      textInput("subtitle", "Plot subtitle"),
      textInput("x_label", "Y-axis label", ""),
      textInput("y_label", "Y-axis label", ""),
      textInput("legend_title", "Legend title", "")
    ),
    mainPanel(
      width = 10,
      fluidRow(
        column(4,
               textAreaInput("data", label = "Data input", placeholder = "Paste data from Excel here", 
                             width = 400, height = 300)),
        column(8, plotOutput("plot"))
      ),
      fluidRow(
        column(8,
               DTOutput("data_table"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  observe({
    # Auto-resize textarea input based on content
    runjs("$('#data').on('input', function() {this.style.height = 'auto';this.style.height = (this.scrollHeight) + 'px';});")
  })
  
  data <- reactive({
    req(input$data)
    if (validateData(input$data)) {
      # Create data frame
      df <- read.table(text = input$data, header = TRUE, dec = ",")
      
      # Update selectInput choices for x and y variables
      updateSelectInput(session, "x_var", choices = colnames(df))
      updateSelectInput(session, "y_var", choices = colnames(df), selected = colnames(df)[2])
      updateSelectInput(session, "group_var", choices = colnames(df))
      
      df
    } else {
      # Invalid format, return NULL
      NULL
    }
  })
  
  output$plot <- renderPlot(height = 400, width = 500, res = 120, {
    
    if (is.null(data())) {
      # Handle invalid format
      return(NULL)
    }
    
    # Create ggplot
    p <- ggplot(data(), aes(x = .data[[input$x_var]], y = .data[[input$y_var]], color = .data[[input$group_var]]))
    
    observeEvent(data(), {
      # Detect data types of x variable
      x_type <- typeof(data()[[input$x_var]])

      # Suggest a default geom based on data types
      default_geom <- if (x_type == "character" && is.numeric(data()[[input$y_var]])) {
        "Bar plot"
      } else if (is.numeric(data()[[input$x_var]]) && is.numeric(data()[[input$y_var]])) {
        "Scatter plot"
      } else {
        "Scatter plot"
      }
      
      # Update selectInput choices for plot type and set default value
      updateSelectInput(session, "plot_type", selected = default_geom)
    })
    
    # Set plot type based on user selection or suggested default
    if (input$plot_type == "Bar plot") {
      p <- p + geom_bar(stat = "identity")
    } else if (input$plot_type == "Line plot") {
      p <- p + geom_line()
    } else if (input$plot_type == "Scatter plot") {
      p <- p + geom_point()
    }
    
    if (input$color_fill == "Color") {
      p <- p + aes(color = .data[[input$group_var]])
      color_title <- input$legend_title
      fill_title <- NULL
    } else if (input$color_fill == "Fill") {
      p <- p + aes(fill = .data[[input$group_var]])
      fill_title <- input$legend_title
      color_title <- NULL
    }
    
    # Customize axes labels
    p <- p + 
      labs(x = input$x_label, 
           y = input$y_label,
           title = input$title,
           subtitle = input$subtitle,
           color = color_title,
           fill = fill_title)
    
    # Render the plot
    p
  })
  
  # Create the output table
  output$data_table <- renderDT({
    req(data())
    datatable(data(), options = list(
      autoWidth = TRUE
    ))
  })
  
}

shinyApp(ui = ui, server = server)

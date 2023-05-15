library(shiny)
library(ggplot2)
library(jsonlite)
library(noradplot)
library(shinyjs)
library(DT)

ggnorad("default")

ui <- fluidPage(
  useShinyjs(),  # Enable shinyjs
  
  sidebarLayout(
    sidebarPanel(width = 2,
      selectInput("x_var", "Select X Variable", ""),
      selectInput("y_var", "Select Y Variable", ""),
      selectInput("plot_type", "Select Plot Type",
                  choices = c("Bar Plot", "Line Plot", "Scatter Plot")),
      selectInput("group_var", "Grouping variable for fill/color aesthetic", choices = NULL),
      selectInput("color_fill", "Aesthetic", choices = c("Color", "Fill", "Color and Fill")),
      textInput("x_label", "X-Axis Label", ""),
      textInput("y_label", "Y-Axis Label", ""),
      textInput("legend_title", "Legend Title", "")
    ),
    mainPanel(width = 4,
      textAreaInput("data", label = "Data input", placeholder = "Paste data from Excel here", width = 400, height = 300),
      plotOutput("plot"),
      DTOutput("data_table")
    )
  )
)

server <- function(input, output, session) {
  
  observe({
    # Auto-resize textarea input based on content
    runjs("$('#data').on('input', function() {this.style.height = 'auto';this.style.height = (this.scrollHeight) + 'px';});")
  })
  
  validateData <- function(data) {
    if (grepl("\\s+", data)) {
      # Data contains whitespace or tab characters
      return(TRUE)
    }
    # Add more format checks if needed
    # e.g., check for comma-separated data
    
    FALSE
  }
  
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
  
  output$plot <- renderPlot(height = 300, width = 400, {
    
    if (is.null(data())) {
      # Handle invalid format
      return(NULL)
    }
    
    # Create ggplot
    p <- ggplot(data(), aes(x = .data[[input$x_var]], y = .data[[input$y_var]], color = .data[[input$group_var]]))
    
    observeEvent(data(), {
      # Detect data types of x and y variables
      x_type <- typeof(data()[[input$x_var]])
      y_type <- typeof(data()[[input$y_var]])
      
      # Suggest a default geom based on data types
      default_geom <- if (x_type == "character" && is.numeric(data()[[input$y_var]])) {
        "Bar Plot"
      } else if (is.numeric(data()[[input$x_var]]) && is.numeric(data()[[input$y_var]])) {
        "Scatter Plot"
      } else {
        "Scatter Plot"
      }
      
      # Update selectInput choices for plot type and set default value
      updateSelectInput(session, "plot_type", selected = default_geom)
    })
    
    # Set plot type based on user selection or suggested default
    if (input$plot_type == "Bar Plot") {
      p <- p + geom_bar(stat = "identity")
    } else if (input$plot_type == "Line Plot") {
      p <- p + geom_line()
    } else if (input$plot_type == "Scatter Plot") {
      p <- p + geom_point()
    } else {
      # Use suggested default geom
      p <- p + default_geom
    }
    
    if (input$color_fill == "Color") {
      p <- p + aes(color = .data[[input$group_var]])
    } else if (input$color_fill == "Fill") {
      p <- p + aes(fill = .data[[input$group_var]])
    }
    
    # Customize axes labels
    p <- p + xlab(input$x_label) + ylab(input$y_label)
    
    # Customize legend title
    p <- p + guides(fill = guide_legend(title = input$legend_title))
    
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

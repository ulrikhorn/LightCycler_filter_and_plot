library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("newtitle"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV File"),
      uiOutput("column_ui"),      # Column selector for filtering
      uiOutput("value_ui"),       # Value selector for filtering
      hr(),
      uiOutput("x_ui"),           # X-axis column selector
      uiOutput("y_ui"),           # Y-axis column selector
      uiOutput("color_ui")        # Optional grouping selector
    ),
    mainPanel(
      plotOutput("plot"),
      downloadButton("download_plot", "Download Plot"),
      downloadButton("download_data", "Download Filtered Data"),
      tableOutput("filtered_data")
    )
  )
)

server <- function(input, output, session) {
  # Read the uploaded file
  df <- reactive({
    req(input$file)
    filename <- input$file
    read.csv2(input$file$datapath, stringsAsFactors = FALSE, skip = 1, sep = "\t")
  })
  
  # Filtering column selector
  output$column_ui <- renderUI({
    req(df())
    selectInput("selected_column", "Filter by Column", choices = names(df()), selected = "SampleName")
  })
  
  # Value selector (unique values from selected column)
  output$value_ui <- renderUI({
    req(input$selected_column)
    values <- unique(df()[[input$selected_column]])
    
    sample_vals <- values[grepl("^Sample\\s*\\d*", values)]
    other_vals <- setdiff(values, sample_vals)
    sorted_vals <- c(sort(other_vals), sort(sample_vals))  # Both parts sorted alphabetically
    
    selectInput(
      "selected_values",
      "Select Values",
      choices = sorted_vals,
      multiple = TRUE
      #selected = unique(df()[[input$selected_column]])[1]
    )
  })
  
  # Filtered data
  filtered_df <- reactive({
    req(input$selected_column, input$selected_values)
    df()[df()[[input$selected_column]] %in% input$selected_values, ]
  })
  
  output$filtered_data <- renderTable({
    filtered_df()
  })
  
  # X and Y axis selectors
  output$x_ui <- renderUI({
    req(filtered_df())
    selectInput("x_col", "X-axis", choices = names(filtered_df()), selected = "Cycle.")
  })
  
  output$y_ui <- renderUI({
    req(filtered_df())
    selectInput("y_col", "Y-axis", choices = names(filtered_df()), selected = "X465.510")
  })
  
  output$color_ui <- renderUI({
    req(filtered_df())
    selectInput("color_col", "Color (optional)", choices = c("None", names(filtered_df())), selected = "SampleName")
  })
  
  # Render the ggplot
  output$plot <- renderPlot({
    req(input$x_col, input$y_col)
    
    data <- filtered_df()
    
    # Convert x and y to numeric if possible (e.g., if read.csv2 imported them as strings)
    data[[input$x_col]] <- suppressWarnings(as.numeric(data[[input$x_col]]))
    data[[input$y_col]] <- suppressWarnings(as.numeric(data[[input$y_col]]))
    
    p <- ggplot(data, aes_string(x = input$x_col, y = input$y_col))
    
    if (!is.null(input$color_col) && input$color_col != "None") {
      p <- p + geom_line(aes_string(color = input$color_col))
    } else {
      p <- p + geom_line()
    }
    
    p + theme_minimal()
  })
  
  output$download_plot <- downloadHandler(
    filename = function() {
      paste0("plot-", Sys.Date(), ".jpg")
    },
    content = function(file) {
      req(input$x_col, input$y_col)
      data <- filtered_df()
      
      # Convert to numeric if needed
      data[[input$x_col]] <- suppressWarnings(as.numeric(data[[input$x_col]]))
      data[[input$y_col]] <- suppressWarnings(as.numeric(data[[input$y_col]]))
      
      # Build plot
      p <- ggplot(data, aes_string(x = input$x_col, y = input$y_col))
      if (!is.null(input$color_col) && input$color_col != "None") {
        p <- p + geom_line(aes_string(color = input$color_col))
      } else {
        p <- p + geom_line()
      }
      p <- p + theme_minimal()
      
      # Save to PNG
      ggsave(file, plot = p, width = 8, height = 5, dpi = 300)
    }
  )
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("filtered_data-", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv2(filtered_df(), file, row.names = FALSE)
    }
  )
  
}

shinyApp(ui, server)

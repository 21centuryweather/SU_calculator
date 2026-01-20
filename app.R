#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(shinyjs)
library(data.table)
source("functions.R")

ui <- fluidPage(
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  
  titlePanel("SU Calculator for Gadi"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("node", "Select node type:", choices = names(node_queues)),
      selectInput("queue", "Queue:", choices = node_queues[[1]]),
      shiny::uiOutput("cpus"),
      div(class = "limit-text", uiOutput("cpu_limit_text")),
      
      numericInput("memory", "Memory (GB):", value = 1, min = 1),
      div(class = "limit-text", uiOutput("memory_limit_text")),
      
      numericInput("walltime", "Walltime (hours):", value = 1, min = 1),
      div(class = "limit-text", uiOutput("walltime_limit_text"))
    ),
    mainPanel(
      h3("Estimated Service Units (SU)"),
      verbatimTextOutput("su_result"),
      p("The equation for every job run on Gadi will be charged using the formula"),
      p("Job Cost (SU) = Queue Charge Rate  ✕  Max (NCPUs, Memory)  ✕  Walltime (Hours)"),
      p("Check the NCI documentation to learn about ",
        a("Queue options and limits", href = "https://opus.nci.org.au/spaces/Help/pages/236881198/Queue+Limits..."))
    )
  ),
  tags$footer(
    style = "position:fixed;
                 # bottom:50px;
                 width:80%;
                 height:40px; 
                 # color: black;
                 padding: 0px;
                 # background-color: lightgrey;
                 z-index: 100;",
    img(src = "https://www.21centuryweather.org.au/wp-content/themes/21CW-2023/img/logo_arc.png", height = "40px", style = "vertical-align:middle;")
  )
)

server <- function(input, output, session) {
  observeEvent(input$node, {
    updateSelectInput(session, "queue", choices = node_queues[[input$node]])
  })
  
  
  output$cpus <- renderUI({
    if (input$node == "GPU") {
      numericInput("cpus", "GPUs:", value = 1, min = 1) 
    } else {
      numericInput("cpus", "CPUs:", value = 1, min = 1)
    }
  })

  
  cpus <- reactive({
    req(input$node, input$queue, input$cpus)
    
    if(input$node == "GPU") {
      gpu_to_cpu(input$queue, input$cpus)
    } else {
      input$cpus
    }
    
  })

  
  # Reactive values for limits
  node_memory_limit <- reactive({
    req(input$queue)
    get_memory_limit(input$queue)
  })
  
  current_memory_limit <- reactive({
    req(input$queue, input$cpus)
    memory_per_node <- get_memory_limit(input$queue)
    cpus_per_node <- get_cpus_per_node(input$queue)
    
    ceiling(cpus()/cpus_per_node) * memory_per_node
      
  })
  
  
  current_walltime_limit <- reactive({
    req(input$queue, input$cpus)
    get_walltime_limit(input$queue, cpus())
  })
  
  # Update input limits
  observe({
    req(input$queue)
    memory_limit <- current_memory_limit()
    if(!is.na(memory_limit) && length(memory_limit) > 0) {
      updateNumericInput(session, "memory", max = memory_limit)
    }
  })
  
  observe({
    req(input$queue, input$cpus)
    walltime_limit <- current_walltime_limit()
    if(!is.na(walltime_limit) && length(walltime_limit) > 0) {
      updateNumericInput(session, "walltime", max = walltime_limit)
    }
  })
  
  # Display CPU limit information
  output$cpu_limit_text <- renderUI({
    req(input$node, input$queue, input$cpus)
    
    max_cpus <- queue_param[queue == input$queue, max(max_cpus)]
    if(!is.na(max_cpus)) {
      cpu_message(cpus(), input$queue, max_cpus)
    }
  })
  
  
  # Display memory limit information
  output$memory_limit_text <- renderUI({
    req(input$queue)
    memory_limit <- current_memory_limit()
    memory_limit_per_node <- node_memory_limit()
    if(!is.na(memory_limit) && length(memory_limit) > 0) {
      if(input$memory > memory_limit) {
        tags$span(paste("Max memory for this queue:", memory_limit_per_node, "GB per node. Memory limit for", cpus(), "CPUs:", memory_limit, "GB"), 
                  class = "limit-exceeded")
      } else {
        tags$span(paste("Max memory for this queue:", memory_limit_per_node, "GB per node"), 
                  class = "limit-ok")
      }
    }
  })
  
  # Display walltime limit information
  output$walltime_limit_text <- renderUI({
    req(input$queue, input$cpus)
    walltime_limit <- current_walltime_limit()
    if(!is.na(walltime_limit) && length(walltime_limit) > 0) {
      if(input$walltime > walltime_limit) {
        tags$span(paste("Max walltime for", cpus(), "CPUs:", walltime_limit, "hours"), 
                  class = "limit-exceeded")
      } else {
        tags$span(paste("Max walltime for", cpus(), "CPUs:", walltime_limit, "hours"), 
                  class = "limit-ok")
      }
    } 
  })
  
  output$su_result <- renderText({
    
    # req(input$queue)
    max_cpus <- queue_param[queue == input$queue, max(max_cpus)]
    memory_limit <- current_memory_limit()
    walltime_limit <- current_walltime_limit()
    valid_ncpu <- is_valid_cpu(cpus(), input$queue)
    
    if(!valid_ncpu) {
      "Invald number of CPU for queue"
    } else if (is.na(max_cpus) | cpus() > max_cpus) {
      "CPU/GPUs requested out of range"
    } else if (input$memory > memory_limit ){
      "Memory requested out of range"
    } else if (input$walltime > walltime_limit) {
      "Walltime requested out of range"
    } else {
      su <- calculate_SU(input$queue, cpus(), input$memory, input$walltime)
      paste("SU Cost:", su)
    }
  })
}


# Run the application 
shinyApp(ui = ui, server = server)

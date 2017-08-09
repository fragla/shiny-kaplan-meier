library(survminer)
library(survival)

shinyServer(function(input, output) {

  plot.kaplan.meier <- function(input) {
    dat <- read.csv(input$dataset$datapath, header=TRUE)

    names(dat)[which(names(dat) %in% c(input$time, input$status, input$group))] <- c("time", "status", "group")
    fit <- survfit(Surv(time, status) ~ group, data = dat)
    ggsurvplot(fit, data = dat, risk.table = input$risk, conf.int = input$confint, pval=input$pvalue)
  }

  # Drop-down selection box for which data set
  output$choose_dataset <- renderUI({
    #selectInput("dataset", "Data set", as.list(data_sets))
    fileInput("dataset", "Choose data file",
          accept = c(
              "text/csv",
              "text/comma-separated-values,text/plain",
              ".csv")
        )
  })

  # Check boxes
  output$choose_time_column <- renderUI({
    # If missing input, return to avoid error later in function
    if(is.null(input$dataset))
      return()

    # Get the data set with the appropriate name
    #dat <- get(input$dataset) 
    dat <- read.csv(input$dataset$datapath, header=TRUE)
    colnames <- names(dat)

    # Create the checkboxes and select them all by default
#    checkboxGroupInput("columns", "Choose columns", 
#                        choices  = colnames),#,
                        #selected = colnames)
    radioButtons("time", "Time column:",
               colnames,
               selected=TRUE,
               inline=T)#
  })

  output$choose_status_column <- renderUI({
    # If missing input, return to avoid error later in function
    if(is.null(input$dataset))
      return()

    # Get the data set with the appropriate name
    #dat <- get(input$dataset) 
    dat <- read.csv(input$dataset$datapath, header=TRUE)
    colnames <- names(dat)

    # Create the checkboxes and select them all by default
#    checkboxGroupInput("columns", "Choose columns", 
#                        choices  = colnames),#,
            
    radioButtons("status", "Status column:",
               colnames,
               selected=TRUE,
               inline=T)
  })

   output$choose_group_column <- renderUI({
    # If missing input, return to avoid error later in function
    if(is.null(input$dataset))
      return()

    # Get the data set with the appropriate name
    #dat <- get(input$dataset) 
    dat <- read.csv(input$dataset$datapath, header=TRUE)
    colnames <- names(dat)

    # Create the checkboxes and select them all by default
#    checkboxGroupInput("columns", "Choose columns", 
#                        choices  = colnames),#,
            
    radioButtons("group", "Group column:",
               colnames,
               selected=TRUE,
               inline=T)
  })

  output$show_confidence_intervals <- renderUI({
    checkboxInput("confint", "Show confidence intervals", value = TRUE)
  })

  output$show_risk_table <- renderUI({
    checkboxInput("risk", "Show risk table", value = TRUE)
  })

  output$show_p_value <- renderUI({
    checkboxInput("pvalue", "Show p value", value = TRUE)
  })


  # Output the data
  output$km_plot <- renderPlot({
    # If missing input, return to avoid error later in function
    if(is.null(input$dataset))
      return()

    # Get the data set
    
    # Make sure columns are correct for data set (when data set changes, the
    # columns will initially be for the previous data set)
    #if (is.null(input$columns) || !(input$columns %in% names(dat)))
    #  return()

    if (is.null(input$time) || is.null(input$status) || is.null(input$group)) {
      return()
    }
    #print(paste("++++++", input$confint))
    plot.kaplan.meier(input)
    
    
  })
  output$downloadPlot <- downloadHandler(
    filename = function() { "km.pdf" },
    content = function(file) {
      plot <- plot.kaplan.meier(input)
      pdf(file, onefile=FALSE)
      print(plot)
      dev.off()
      #ggsave(file = file, print(plot))
    }
  )
})

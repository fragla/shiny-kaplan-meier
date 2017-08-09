shinyUI(pageWithSidebar(

  headerPanel("Kaplan-Meier"),

  sidebarPanel(
    uiOutput("choose_dataset"),

    uiOutput("choose_time_column"),

    uiOutput("choose_status_column"),

    uiOutput("choose_group_column"),

    uiOutput("show_confidence_intervals"),

    uiOutput("show_risk_table"),

    uiOutput("show_p_value")
  ),


  mainPanel(
    plotOutput("km_plot"),
    downloadButton(outputId = "downloadPlot", label = "Download the plot")

  )
))
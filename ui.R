shinyUI(fluidPage(
  titlePanel("eMun Grievance Drill-Down Dashboard"),
  fluidRow(uiOutput("go_back"),
           plotly::plotlyOutput("plot"),
           verbatimTextOutput("click"),
           verbatimTextOutput("level_id"),
           verbatimTextOutput("level"),
           verbatimTextOutput("selected"))
))
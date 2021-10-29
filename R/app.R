ui <- shiny::fluidPage(
  shiny::titlePanel('Agile Cadence Calendar'),

  shiny::sidebarLayout(
    shiny::sidebarPanel(),
    shiny::mainPanel(
      shiny::plotOutput(outputID = 'calendar'))
  )
)

server <- function(input, output) {
  output$calendar <- renderPlot({
    plot_calendar(cal)
  })
}

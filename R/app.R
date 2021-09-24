ui <- shiny::fluidpage(
  titlePanel('Agile Cadence Calendar')

  sidebarLayout(
    sidebarPanel()
    mainPanel()
  )
)

server <- function(input, output) {
}

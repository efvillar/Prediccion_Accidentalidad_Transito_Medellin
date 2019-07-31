library(shiny)
library(quantmod)

ui<-fluidPage(
  titlePanel("Proyeccion Acidentes de transito Medell�n"),
  sidebarLayout(
    sidebarPanel("Seleccione el rango a estimar",
                 selectInput('accion',
                             label = 'Tipo de Accidente',
                             choices = c("Total"="T", "Choque"="C",
                                         "Otro"="O")),
                 dateInput(inputId="fecha_desde", label="Desde la fecha",
                           language= "es", width = "40%", value = "2017-01-01"),
                 dateInput(inputId="fecha_hasta", label="Hasta la fecha",
                           language= "es", width = "40%", value = "2017-08-18")

    ),
    mainPanel("Gráfico de Acciones del Mercado de Valores Americano",
              h1('Gráficos de Precios'),
              p('A continuación se muestra la gráfica del precio de la acción seleccionada.'),
              plotOutput('grafico'))
  )
)

# Define server logic required to draw a histogram
server<-function(input, output) {
  stockdata <- reactive({
    getSymbols(input$accion, src="yahoo", from = input$fechadesde,
               to = input$fechahasta, auto.assign = FALSE)
  })
  output$grafico <- renderPlot({
    chartSeries(stockdata(), name = input$accion, type = input$tipo, theme = input$tema)
  })
}

shinyApp(ui=ui, server=server)


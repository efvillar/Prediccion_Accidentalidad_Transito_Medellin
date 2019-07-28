library(shiny)
library(quantmod)

ui<-fluidPage(
    titlePanel("Mercados con R"),
    sidebarLayout(
        sidebarPanel("Seleccione la acción que desea consultar",
                     selectInput('accion',
                                 label = 'Acción',
                                 choices = c("Apple"="AAPL", "Cisco"="CSCO",
                                             "IBM"="IBM", "Facebook"="FB",
                                             "Twitter"="TWTR", "Microsoft"="MSFT",
                                             "Google"="GOOG")),
                     dateInput(inputId="fechadesde", label="Desde la fecha",
                               language= "es", width = "40%", value = "2017-01-01"),
                     dateInput(inputId="fechahasta", label="Hasta la fecha",
                               language= "es", width = "40%", value = "2017-08-18"),
                     selectInput('tema', label = 'Tema', choices = c("Negro"="black", "Blanco"="white")),
                     selectInput('tipo', label = 'Tipo de gráfico',
                                 choices = c("Velas"="candlesticks","Barras"="bars", "Línea"="line"))
        ),
        mainPanel("Gráfico de Acciones del Mercado de Valores Americano",
                  h1('Gráficos de Precios'),
                  p('A continuación se muestra la gráfica del precio de la acción seleccionada.'),
                  plotOutput('grafico'))
    )
)

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


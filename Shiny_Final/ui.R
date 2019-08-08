#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(
    #primero se define una estructura de la UI
    pageWithSidebar(
        headerPanel("Predicción de Accidentes de Tránsito en Medellín"),
        
        
        sidebarPanel("Seleccione el periodo de la predicción",
                                          selectInput('Periodo',
                                                      label = 'Periodo',
                                                      choices = c("Dia"="Dia", "Mes"="Mes",
                                                                  "Semana"="Semana")),
                                          dateInput(inputId="fechadesde", label="Desde la fecha",
                                                    language= "es", width = "40%", value = "2019-01-01"),
                                          dateInput(inputId="fechahasta", label="Hasta la fecha",
                                                    language= "es", width = "40%", value = "2021-12-31"),
                                          selectInput('tema', label = 'Tema', choices = c("Negro"="black", "Blanco"="white")),
                                          selectInput('tipo', label = 'Tipo de gráfico',
                                                      choices = c("Velas"="candlesticks","Barras"="bars", "Línea"="line"))
        ),
        mainPanel(
            tabsetPanel(
                tabPanel("Plot Variables",h3(textOutput("output_text")),
                         plotOutput("output_plot")
                ),
                tabPanel("Imagen Estática",
                         img(src="Notacion_Cubo.PNG", height=200, width=200)
                ),
                tabPanel("Summary", verbatimTextOutput("summary")
                         
                ),
                tabPanel("Table", tableOutput("table")
                         
                ),
                tabPanel("Data Table", dataTableOutput("datatable")
                         
                )
            )
        )
    )  #la pagina tendra una barra lateral
    
)
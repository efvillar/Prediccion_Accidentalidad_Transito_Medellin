library(shiny)
library(plotly)
ui <- fluidPage(

        
        mainPanel(
            
            tabsetPanel(type = "tabs",
                        
                        tabPanel("Gráfica Datos Históricos Semanales Totales", 
                                                                                plotlyOutput("Serie_semanal_Total"),
                                                                                plotlyOutput("Serie_semanal_AG"),
                                                                                plotlyOutput("Serie_semanal_AL")),
   
                        tabPanel("Resumen del Modelo Semanal", verbatimTextOutput("summary_sem")), # Model output
                        tabPanel("Data semanal histórica", DT::dataTableOutput('tbl')) # Data as datatable
                        
            )
        )
    )



# SERVER
server <- function(input, output) {
    

    #load XY data semanal historica
    load(file = "Data_sem1.Rda")
    XY_Sem_shiny <- Data_sem1
    
    
    
    #grafica historica semanal Total plotly
    output$Serie_semanal_Total <- renderPlotly({plot_ly(data=XY_Sem_shiny,
                                                       x = ~SEMANA,
                                                       y = ~Total_Accidentes,
                                                       color = ~ANO,
                                                       split = ~ANO,
                                                       type = "scatter" ,mode = "lines",
                                                       line=list(width=1,color='rgb(80, 80, 80)'))%>%
                                                       
            layout(title='Accidentalidad Total Semanal Historica Medellin',
                   xaxis=list(title="SEMANA"),
                   yaxis=list(title="Accidentes Totales"))  })
    
    
    
    
    #grafica historica semanal Accidentes Graves plotly
    output$Serie_semanal_AG <- renderPlotly({plot_ly(data=XY_Sem_shiny,
                                                        x = ~SEMANA,
                                                        y = ~ACCIDENTES_GRAVES,
                                                        color = ~ANO,
                                                        split = ~ANO,
                                                        type = "scatter" ,mode = "lines",
                                                        line=list(width=1,color='rgb(90, 20, 120)'))%>%
            
            layout(title='Accidentalidad Accidentes Graves Semanal Historica Medellin',
                   xaxis=list(title="SEMANA"),
                   yaxis=list(title="Accidentes Graves"))  })
    
    
    #grafica historica semanal Accidentes Leves plotly
    output$Serie_semanal_AL <- renderPlotly({plot_ly(data=XY_Sem_shiny,
                                                     x = ~SEMANA,
                                                     y = ~ACCIDENTES_LEVES,
                                                     color = ~ANO,
                                                     split = ~ANO,
                                                     type = "scatter" ,mode = "lines",
                                                     line=list(width=1,color='rgb(140, 20, 40)'))%>%
            
            layout(title='Accidentalidad Accidentes Leves Semanal Historica Medellin',
                   xaxis=list(title="SEMANA"),
                   yaxis=list(title="Accidentes Leves"))  }) 
    
    
    
    # Model summary semanal precalculado
    output$summary_sem <- renderPrint({
        model_sem <- readRDS("model_sem.rds")
        summary(model_sem)
    })
    
    # Data output semanal
    #output$tbl = DT::renderDataTable({
    #    DT::datatable(XY_Sem, options = list(lengthChange = TRUE))
    #})
    output$tbl <- DT::renderDataTable({
        XY_Sem_shiny
    }, options = list(aLengthMenu = c(5,25,50),
                      iDisplayLength = 5)
    )
    
    
    
    
    
}

shinyApp(ui = ui, server = server)


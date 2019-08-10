library(shiny)
library(plotly)
ui <- fluidPage(

        
        mainPanel(
            
            tabsetPanel(type = "tabs",
                        
                        tabPanel("Gráfica Datos Históricos", 
                                                                                plotlyOutput("Serie_Diaria_Total"),
                                                                                plotlyOutput("Serie_Diaria_AG"),
                                                                                plotlyOutput("Serie_diaria_AL"),
                                                                                plotlyOutput("Serie_semanal_Total"),
                                                                                plotlyOutput("Serie_semanal_AG"),
                                                                                plotlyOutput("Serie_semanal_AL")),
                        tabPanel("Resumen del Modelo Diario",
                                 #h5(textOutput("Modelo Diario para Accidentes Totales")),
                                 verbatimTextOutput("text_model_D_T"),
                                 verbatimTextOutput("summary_D_T"), # Model output
                                 
                                 verbatimTextOutput("text_model_D_L"),
                                 verbatimTextOutput("summary_D_L"),
                                 
                                 verbatimTextOutput("text_model_D_G"),
                                 verbatimTextOutput("summary_D_G")                               
                                 
                                 
                                 ), # Model output
                        
                                 
                                                      
                        tabPanel("Resumen del Modelo Semanal", verbatimTextOutput("summary_sem")), # Model output
                        tabPanel("Data Histórica Diaria ", DT::dataTableOutput('tbl_D')), # Data as datatable
                        tabPanel("Data Histórica Semanal", DT::dataTableOutput('tbl')) # Data as datatable
                        
            )
        )
    )



# SERVER
server <- function(input, output) {
    

    #load XY data semanal historica
    load(file = "Data_sem1.Rda")
    XY_Sem_shiny <- Data_sem1
    #load data histórica diaria
    load(file="./data_modelos_diario/Total_Dataset_Freq_diaria.Rda")
    Data_Historica_Diaria<-Total_Dataset_Freq[,c("FECHA","ACCIDENTES_GRAVES","ACCIDENTES_LEVES","TOTAL_ACCIDENTES","ANO","SEMANA","MES","DIA")]
    
 
    #grafica historica diaria Total Accidentes
    output$Serie_Diaria_Total <- renderPlotly({plot_ly(data=Data_Historica_Diaria,
                                                        x = ~FECHA,
                                                        y = ~TOTAL_ACCIDENTES,
                                                        #color = ~ANO,
                                                        #split = ~ANO,
                                                        type = "scatter" ,mode = "lines",
                                                        line=list(width=1,color='rgb(80, 80, 80)'))%>%
            
            layout(title='Accidentalidad: Accidentes TOTALES Diarios - Datos Históricos Medellín',
                   xaxis=list(title="FECHA"),
                   yaxis=list(title="Accidentes Totales"))  })
    
    #grafica historica Diaria Accidentes Graves plotly
    output$Serie_Diaria_AG <- renderPlotly({plot_ly(data=Data_Historica_Diaria,
                                                     x = ~FECHA,
                                                     y = ~ACCIDENTES_GRAVES,
                                                     #color = ~ANO,
                                                     #split = ~ANO,
                                                     type = "scatter" ,mode = "lines",
                                                     line=list(width=1,color='rgb(90, 20, 120)'))%>%
            
            layout(title='Accidentalidad: Accidentes GRAVES Diarios - Datos Históricos Medellín',
                   xaxis=list(title="SEMANA"),
                   yaxis=list(title="Accidentes Graves"))  })
    
    #grafica historica diaria Accidentes Leves plotly
    output$Serie_diaria_AL <- renderPlotly({plot_ly(data=Data_Historica_Diaria,
                                                     x = ~FECHA,
                                                     y = ~ACCIDENTES_LEVES,
                                                     #color = ~ANO,
                                                     #split = ~ANO,
                                                     type = "scatter" ,mode = "lines",
                                                     line=list(width=1,color='rgb(140, 20, 40)'))%>%
            
            layout(title='Accidentalidad: Accidentes LEVES Diarios - Datos Históricos Medellín',
                   xaxis=list(title="FECHA"),
                   yaxis=list(title="Accidentes Leves"))  })     
    
    
       
    
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
    
    
    
    
     #RESUMEN MODELOS DIARIOS   
        #carga de modelo Accidentes Totales
    
                output$text_model_D_T <- renderText({ 
                    paste("MODELO DE REGRESIÓN PARA PRONÓSTICO DIARIO ACCIDENTES TOTALES")
                })
                output$summary_D_T<- renderPrint({
                        model_D_T <- readRDS("./data_modelos_diario/Prediccion_Total_Diario.Rds")
                        summary(model_D_T)
                })  
    
    #carga de modelo Accidentes Leves
    
                output$text_model_D_L <- renderText({ 
                    paste("MODELO DE REGRESIÓN PARA PRONÓSTICO DIARIO ACCIDENTES LEVES")
                })
                output$summary_D_L<- renderPrint({
                    model_D_L <- readRDS("./data_modelos_diario/Prediccion_leves_Diario.Rds")
                    summary(model_D_L)
                })  
                
    #carga de modelo Accidentes GRAVES
                
                output$text_model_D_G <- renderText({ 
                    paste("MODELO DE ÁRBOL DE REGRESIÓN PARA PRONÓSTICO DIARIO ACCIDENTES GRAVES")
                })
                output$summary_D_G<- renderPrint({
                    model_D_G <- readRDS("./data_modelos_diario/Prediccion_Grave_Diario.Rds")
                    model_D_G
                })                
                
    
    # Model summary semanal precalculado
    
                output$summary_sem <- renderPrint({
                    model_sem <- readRDS("model_sem.rds")
                    model_sem
                    #summary(model_sem)
                })
    
    
    

    
    
    
    
    # Data historica TABLA
    #SEMANAL
    output$tbl <- DT::renderDataTable({
        XY_Sem_shiny
    }, options = list(aLengthMenu = c(5,25,50),
                      iDisplayLength = 5)
    )
    #DIARIA
    output$tbl_D <- DT::renderDataTable({
        Data_Historica_Diaria
    }, options = list(aLengthMenu = c(5,25,50),
                      iDisplayLength = 5)
    )    
    
    
    
    
}

shinyApp(ui = ui, server = server)


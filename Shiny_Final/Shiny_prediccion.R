library(shiny)
library(plotly)
ui <- fluidPage(

        
        mainPanel(
            
            tabsetPanel(type = "tabs",
                        
                        tabPanel("Pronósticos Diarios",
                                
                        plotlyOutput("plot_pronostico_D_T"),
                        plotlyOutput("plot_pronostico_D_L"),
                        plotlyOutput("plot_pronostico_D_G"),
                        DT::dataTableOutput('Pronosticos_D')),# Data as datatable
                        
                        tabPanel("Gráfica Datos Históricos", 
                                                                                plotlyOutput("Serie_Diaria_Total"),
                                                                                plotlyOutput("Serie_Diaria_AG"),
                                                                                plotlyOutput("Serie_diaria_AL"),
                                 
                                                                                plotlyOutput("Serie_semanal_Total"),
                                                                                plotlyOutput("Serie_semanal_AG"),
                                                                                plotlyOutput("Serie_semanal_AL"),
                        plotlyOutput("Serie_mensual_Total"),
                        plotlyOutput("Serie_mensual_AG"),
                        plotlyOutput("Serie_mensual_AL")),                        
                        
                        
                        
                        
                        tabPanel("Resumen Modelos Diarios",
                                 #h5(textOutput("Modelo Diario para Accidentes Totales")),
                                 verbatimTextOutput("text_model_D_T"),
                                 verbatimTextOutput("summary_D_T"), # Model output
                                 
                                 verbatimTextOutput("text_model_D_L"),
                                 verbatimTextOutput("summary_D_L"),
                                 
                                 verbatimTextOutput("text_model_D_G"),
                                 verbatimTextOutput("summary_D_G")                               
                                 
                                 
                                 ), # Model output
                        
                                 
                                                      
                        tabPanel("Resumen Modelos Semanales",
                                 #h5(textOutput("Modelo Semanal para Accidentes Totales")),
                                 verbatimTextOutput("text_model_S_T"),
                                 verbatimTextOutput("summary_S_T"), # Model output
                                 
                                 verbatimTextOutput("text_model_S_L"),
                                 verbatimTextOutput("summary_S_L"),
                                 
                                 verbatimTextOutput("text_model_S_G"),
                                 verbatimTextOutput("summary_S_G")),
                        
                        
                        tabPanel("Resumen Modelos Mensuales",
                                 #h5(textOutput("Modelo Mensual para Accidentes Totales")),
                                 verbatimTextOutput("text_model_M_T"),
                                 verbatimTextOutput("summary_M_T"), # Model output
                                 
                                 verbatimTextOutput("text_model_M_L"),
                                 verbatimTextOutput("summary_M_L"),
                                 
                                 verbatimTextOutput("text_model_M_G"),
                                 verbatimTextOutput("summary_M_G")),                        
                        
                        
                        
                        
                        tabPanel("Data Histórica Diaria ", DT::dataTableOutput('tbl_D')), # Data as datatable
                        tabPanel("Data Histórica Semanal", DT::dataTableOutput('tbl')), # Data as datatable
                        tabPanel("Data Histórica Mensual", DT::dataTableOutput('tbl_M')) # Data as datatable                       
                        
            )
        )
    )



# SERVER
server <- function(input, output) {
    


    #load data histórica diaria
        load(file="./data_modelos_diario/Total_Dataset_Freq_diaria.Rda")
        Data_Historica_Diaria<-Total_Dataset_Freq[,c("FECHA","ACCIDENTES_GRAVES","ACCIDENTES_LEVES","TOTAL_ACCIDENTES","ANO","SEMANA","MES","DIA")]
    #load data historica Semanal
        load(file="./data_modelos_semana/Total_Dataset_Freq_S_semanal.Rda")
        XY_Sem_shiny <- Total_Dataset_Freq_S
    #load data historica Semanal
        load(file="./data_modelos_mes/Total_Dataset_Freq_M_mensual.Rda")
        XY_Mes_shiny <- Total_Dataset_Freq_M[,c("ANO","MES","ACCIDENTES_GRAVES","ACCIDENTES_LEVES","TOTAL_ACCIDENTES")]        
        
        
        
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
                                                         y = ~TOTAL_ACCIDENTES,
                                                         color = ~ANO,
                                                         split = ~ANO,
                                                         type = "scatter" ,mode = "lines",
                                                         line=list(width=1,color='rgb(90, 20, 120)'))%>%
                
                layout(title='Accidentalidad Accidentes TOTALES Semanal Histórica Medellín',
                       xaxis=list(title="SEMANA"),
                       yaxis=list(title="Accidentes Graves"))  })
    
    
    
    
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
    
    

        #grafica historica mensual Total plotly
        output$Serie_mensual_Total <- renderPlotly({plot_ly(data=XY_Mes_shiny,
                                                            x = ~MES,
                                                            y = ~TOTAL_ACCIDENTES,
                                                            color = ~ANO,
                                                            split = ~ANO,
                                                            type = "scatter" ,mode = "lines",
                                                            line=list(width=1,color='rgb(80, 80, 80)'))%>%
                
                layout(title='Accidentalidad Total Mensual Histórica Medellin',
                       xaxis=list(title="MES"),
                       yaxis=list(title="Accidentes Totales"))  })
        
        
        
        
        #grafica historica Mensual Accidentes Graves plotly
        output$Serie_mensual_AG <- renderPlotly({plot_ly(data=XY_Mes_shiny,
                                                         x = ~MES,
                                                         y = ~ACCIDENTES_GRAVES,
                                                         color = ~ANO,
                                                         split = ~ANO,
                                                         type = "scatter" ,mode = "lines",
                                                         line=list(width=1,color='rgb(90, 20, 120)'))%>%
                
                layout(title='Accidentalidad Accidentes Graves Mensual Historica Medellin',
                       xaxis=list(title="SMES"),
                       yaxis=list(title="Accidentes Graves"))  })
        
        
        
        #grafica historica Mensual Accidentes Leves plotly
        output$Serie_mensual_AL <- renderPlotly({plot_ly(data=XY_Mes_shiny,
                                                         x = ~MES,
                                                         y = ~ACCIDENTES_LEVES,
                                                         color = ~ANO,
                                                         split = ~ANO,
                                                         type = "scatter" ,mode = "lines",
                                                         line=list(width=1,color='rgb(140, 20, 40)'))%>%
                
                layout(title='Accidentalidad Accidentes Leves Mensual Historica Medellin',
                       xaxis=list(title="MES"),
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
                
                
                
#RESUMEN MODELOS SEMANALES    
 
        #carga de modelo Accidentes Totales
                
                output$text_model_S_T <- renderText({ 
                    paste("MODELO LINEAL PARA PRONÓSTICO SEMANAL ACCIDENTES TOTALES")
                })
                output$summary_S_T<- renderPrint({
                    model_S_T <- readRDS("./data_modelos_semana/Prediccion_Total_Semanal.Rds")
                    summary(model_S_T)
                })  
                
        #carga de modelo Accidentes Leves
                
                output$text_model_S_L <- renderText({ 
                    paste("MODELO LINEAL  PARA PRONÓSTICO SEMANAL ACCIDENTES LEVES")
                })
                output$summary_S_L<- renderPrint({
                    model_S_L <- readRDS("./data_modelos_semana/Prediccion_leves_Semanal.Rds")
                    summary(model_S_L)
                })  
                
        #carga de modelo Accidentes GRAVES
                
                output$text_model_S_G <- renderText({ 
                    paste("MODELO DE ÁRBOLES ALEATORIOS PARA PRONÓSTICO SEMANAL ACCIDENTES GRAVES")
                })
                output$summary_S_G<- renderPrint({
                    model_S_G <- readRDS("./data_modelos_semana/Prediccion_Grave_Semanal.Rds")
                    model_S_G
                })      
    
#RESUMEN MODELOS MENSUALES
                
                #carga de modelo Accidentes Totales
                
                output$text_model_M_T <- renderText({ 
                    paste("MODELO LINEAL GENERALIZADO PARA PRONÓSTICO MENSUAL ACCIDENTES TOTALES")
                })
                output$summary_M_T<- renderPrint({
                    model_M_T <- readRDS("./data_modelos_mes/Prediccion_Total_Mensual.Rds")
                    summary(model_M_T)
                })  
                
                #carga de modelo Accidentes Leves
                
                output$text_model_M_L <- renderText({ 
                    paste("MODELO LINEAL PARA PRONÓSTICO MENSUAL ACCIDENTES LEVES")
                })
                output$summary_M_L<- renderPrint({
                    model_M_L <- readRDS("./data_modelos_mes/Prediccion_leves_Mensual.Rds")
                    summary(model_M_L)
                })  
                
                #carga de modelo Accidentes GRAVES
                
                output$text_model_M_G <- renderText({ 
                    paste("MODELO LINEALGENERALIZADO POISSON PARA PRONÓSTICO MENSUAL ACCIDENTES GRAVES")
                })
                output$summary_M_G<- renderPrint({
                    model_M_G <- readRDS("./data_modelos_mes/Prediccion_Grave_Mensual.Rds")
                    summary(model_M_G)
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


#MENSUAL
            output$tbl_M <- DT::renderDataTable({
                XY_Mes_shiny
            }, options = list(aLengthMenu = c(5,25,50),
                              iDisplayLength = 5)
            )
        
            
            
            
# PREDICCION

    #MODELOS DIARIOS


        #Total Accidentes, Accidentes graves y  leves
        load(file="./data_modelos_diario/datos_pronostico_diario.Rda")
        datos_PD1 <- subset(datos_pronostico_diario, Fecha>="2019-01-1")
        
        model_D_T <- readRDS("./data_modelos_diario/Prediccion_Total_Diario.Rds")
        datos_PD1$prediccion_Total_D<-predict(model_D_T,datos_PD1[,c("Ano_Base","DIA","SEMANA","Feriado_Lunes","Feriado_Otro","Madre","Semana_Santa","Viernes_Desp_Quincena_v2","Feria_Flores")])
        
        model_D_L <- readRDS("./data_modelos_diario/Prediccion_leves_Diario.Rds")
        datos_PD1$prediccion_Leves_D<-predict(model_D_L,datos_PD1[,c("Ano_Base","DIA","SEMANA","Feriado_Lunes","Feriado_Otro","Madre","Semana_Santa","Viernes_Desp_Quincena_v2","Feria_Flores")])
        
        model_D_G <- readRDS("./data_modelos_diario/Prediccion_Grave_Diario.Rds")
        datos_PD1$prediccion_Graves_D<-predict(model_D_G,datos_PD1[,c("Ano_Base","DIA","SEMANA","Feriado_Lunes","Feriado_Otro","Madre","Semana_Santa","Viernes_Desp_Quincena_v2","Feria_Flores")])
        
        #Accidentes graves

        #TABLA PRONOSTICOS DIARIOS
        
        output$Pronosticos_D <- DT::renderDataTable({
            datos_PD1
        }, options = list(aLengthMenu = c(5,25,50),
                          iDisplayLength = 5)
        )
        

        #Grafica diaria
        
        output$plot_pronostico_D_T <- renderPlotly({plot_ly(data=datos_PD1,
                                                           x = ~Fecha,
                                                           y = ~prediccion_Total_D,
                                                           #color = ~ANO,
                                                           #split = ~ANO,
                                                           type = "scatter" ,mode = "lines",
                                                           line=list(width=1,color='rgb(80, 80, 80)'))%>%
                
                layout(title='Pronóstico Diario Accidentes Totales Medellín',
                       xaxis=list(title="FECHA"),
                       yaxis=list(title="Accidentes Totales"))  })        
        
        output$plot_pronostico_D_L <- renderPlotly({plot_ly(data=datos_PD1,
                                                            x = ~Fecha,
                                                            y = ~prediccion_Leves_D,
                                                            #color = ~ANO,
                                                            #split = ~ANO,
                                                            type = "scatter" ,mode = "lines",
                                                            line=list(width=1,color='rgb(80, 160, 80)'))%>%
                
                layout(title='Pronóstico Diario Accidentes Leves Medellín',
                       xaxis=list(title="FECHA"),
                       yaxis=list(title="Accidentes Leves"))  })        
        
        output$plot_pronostico_D_G <- renderPlotly({plot_ly(data=datos_PD1,
                                                            x = ~Fecha,
                                                            y = ~prediccion_Graves_D,
                                                            #color = ~ANO,
                                                            #split = ~ANO,
                                                            type = "scatter" ,mode = "lines",
                                                            line=list(width=1,color='rgb(130, 10, 45)'))%>%
                
                layout(title='Pronóstico Diario Accidentes Graves Medellín',
                       xaxis=list(title="FECHA"),
                       yaxis=list(title="Accidentes Graves"))  })           
        
        #MODELOS SEMANALES
        

        
        
}
shinyApp(ui = ui, server = server)


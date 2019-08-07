library(shiny)
library(plotly)
library(rpart)
library(randomForest)


ui <- fluidPage(dateInput(inputId = "Fecha1", "Fecha Máx Visualización Datos", value = "2017-12-31", min = "2000-01-01", max = "2019-03-31", format = "yyyy-mm-dd", language = "es"), plotlyOutput("SerieCaudales"), dateInput(inputId = "Fecha2", "Fecha Máxima para Considerar Entrenamiento", value = "2018-01-01", min = "2000-01-01", max = "2019-03-31", format = "yyyy-mm-dd", language = "es"), plotlyOutput("ModelosEntrenamiento"), dateInput(inputId = "FechaInicial", "Fecha Inicial Validacion", value = "2018-01-01", min = "2018-01-01", max = "2019-03-31", format = "yyyy-mm-dd", language = "es"), dateInput(inputId = "FechaFinal", "Fecha Final Validacion", value = "2018-12-31", min = "2018-01-01", max = "2019-03-31", format = "yyyy-mm-dd", language = "es"), plotlyOutput("ModelosValidacion") )

DatosTrabajoFinal_AP <- read.delim("DatosTrabajoFinal_AP.csv", header=TRUE, sep=",", encoding = "latin1")

DatosTrabajoFinal_AP$Fecha=as.Date(DatosTrabajoFinal_AP$Fecha,"%d/%m/%Y")

month.name<-c('enero','febrero','marzo','abril','mayo','junio','julio','agosto','septiembre','octubre','noviembre','diciembre')
DatosTrabajoFinal_AP$mes<-strftime(DatosTrabajoFinal_AP$Fecha, format = "%B")
DatosTrabajoFinal_AP$mes<-factor(DatosTrabajoFinal_AP$mes,levels=month.name )

DatosTrabajoFinal_AP$dia_semana<-weekdays(DatosTrabajoFinal_AP$Fecha)

DatosTrabajoFinal_AP$dia_semana<-ordered(DatosTrabajoFinal_AP$dia_semana,levels=c( "lunes", "martes", "miércoles","jueves", "viernes", "sábado","domingo"))




server <- function(input, output) {

  output$SerieCaudales <- renderPlotly({    plot_ly (data=subset(DatosTrabajoFinal_AP,subset = (Fecha<=input$Fecha1)),
                                                     x = ~Fecha,
                                                     y = ~QmedioDia_L_s,
                                                     type = "scatter" ,mode = "lines",
                                                     #line=list(width=1,color='rgb(205, 12, 24)'))%>%
                                                     line=list(width=1,color='rgb(66, 51, 255)'))%>%
      layout(title='Caudales Medios Día Cadena Salvatorianos',
             xaxis=list(title="Año"),
             yaxis=list(title="[l/s]"))  })
  
  output$ModelosEntrenamiento <- renderPlotly({ plot_ly (data=data.frame(Fecha=DatosTrabajoFinal_AP$Fecha[DatosTrabajoFinal_AP$Fecha<=input$Fecha2],
                                                                         QmedioDia_L_s=DatosTrabajoFinal_AP$QmedioDia_L_s[DatosTrabajoFinal_AP$Fecha<=input$Fecha2],
                                                                         glm=predict(glm(QmedioDia_L_s~Ordinal+dia_semana+mes+Festivo+Festivo_Sem_S,data=DatosTrabajoFinal_AP,subset = (Fecha<=input$Fecha2),family = "poisson"),type="response"),
                                                                         arbol=predict(rpart(QmedioDia_L_s~Ordinal+dia_semana+mes+Festivo,data=DatosTrabajoFinal_AP,subset = (Fecha<=input$Fecha2))),
                                                                         bosque=predict(randomForest(QmedioDia_L_s~Ordinal+dia_semana+mes+Festivo,data=DatosTrabajoFinal_AP,subset = (Fecha<=input$Fecha2),importance=FALSE,ntree=500,mtry=2))),
                                                         x = ~Fecha,
                                                         y = ~QmedioDia_L_s,
                                                         type = "scatter" ,mode = "lines",
                                                         name='Real',
                                                         line=list(width=1,color='rgb(66, 51, 255)'))%>%
      add_trace(y= ~glm,
                name='Modelo Poisson',
                line=list(width=1,color='rgb(255, 51, 0)'))%>%
      add_trace(y= ~arbol,
                name='Árbol',
                line=list(width=1,color='rgb(250, 247, 23)'))%>%
      add_trace(y= ~bosque,
                name='Bosque',
                line=list(width=1,color='rgb(79, 250, 29)'))%>%
      layout(title='Caudales Medios Día (Entrenamiento)',
             xaxis=list(title="Fecha"),
             yaxis=list(title="[L/s]"),
             legend = list(x = 0.75, y = 0.4))  })
  
  output$ModelosValidacion <- renderPlotly({  plot_ly (data=data.frame(Fecha=subset(DatosTrabajoFinal_AP,subset=(Fecha>=input$FechaInicial & Fecha<=input$FechaFinal))$Fecha,
                                                                       QmedioDia_L_s=subset(DatosTrabajoFinal_AP,subset=(Fecha>=input$FechaInicial & Fecha<=input$FechaFinal))$QmedioDia_L_s,
                                                                       glm=predict(glm(QmedioDia_L_s~Ordinal+dia_semana+mes+Festivo+Festivo_Sem_S,data=DatosTrabajoFinal_AP,subset = (Fecha<="2017-12-31"),family = "poisson"),type="response",newdata = subset(DatosTrabajoFinal_AP,subset=(Fecha>=input$FechaInicial & Fecha<=input$FechaFinal))),
                                                                       arbol=predict(rpart(QmedioDia_L_s~Ordinal+dia_semana+mes+Festivo,data=DatosTrabajoFinal_AP,subset = (Fecha<="2017-12-31")),newdata = subset(DatosTrabajoFinal_AP,subset=(Fecha>=input$FechaInicial & Fecha<=input$FechaFinal))),
                                                                       bosque=predict(randomForest(QmedioDia_L_s~Ordinal+dia_semana+mes+Festivo,data=DatosTrabajoFinal_AP,subset = (Fecha<="2017-12-31"),importance=FALSE,ntree=500,mtry=2)
                                                                                      ,newdata = subset(DatosTrabajoFinal_AP,subset=(Fecha>=input$FechaInicial & Fecha<=input$FechaFinal)))),
                                                       x = ~Fecha,
                                                       y = ~QmedioDia_L_s,
                                                       type = "scatter" ,mode = "lines",
                                                       name='Real',
                                                       line=list(width=1,color='rgb(66, 51, 255)'))%>%
      add_trace(y= ~glm,
                name='Modelo Poisson',
                line=list(width=1,color='rgb(255, 51, 0)'))%>%
      add_trace(y= ~arbol,
                name='Árbol',
                line=list(width=1,color='rgb(250, 247, 23)'))%>%
      add_trace(y= ~bosque,
                name='Bosque',
                line=list(width=1,color='rgb(79, 250, 29)'))%>%
      layout(title='Caudales Medios Día (Validación)',
             xaxis=list(title="Fecha"),
             yaxis=list(title="[L/s]"),
             legend = list(x = 0.60, y = 0.4))   })

}

shinyApp(ui = ui, server = server)


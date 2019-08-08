library(shiny)
library(plotly)
ui <- fluidPage(
    titlePanel("PREDICCIÓN DE ACCIDENTES"),
    sidebarLayout(
        sidebarPanel(
            selectInput("outcome", label = h3("Periodo de Predicción"),
                        choices = list("Fertility" = "Fertility",
                                       "Agriculture" = "Agriculture",
                                       "Examination" = "Examination",
                                       "Education" = "Education",
                                       "Catholic" = "Catholic",
                                       "Infant.Mortality" = "Infant.Mortality"), selected = 1),
            
            selectInput("indepvar", label = h3("Explanatory variable"),
                        choices = list("Fertility" = "Fertility",
                                       "Agriculture" = "Agriculture",
                                       "Examination" = "Examination",
                                       "Education" = "Education",
                                       "Catholic" = "Catholic",
                                       "Infant.Mortality" = "Infant.Mortality"), selected = 1)
            
        ),
        
        mainPanel(
            
            tabsetPanel(type = "tabs",
                        
                        tabPanel("Gráfica Datos Históricos Semanales", plotlyOutput("Serie_semanal")),
                        tabPanel("Scatterplot", plotOutput("scatterplot")), # Plot
                        tabPanel("Distribution", # Plots of distributions
                                 fluidRow(
                                     column(6, plotOutput("distribution1")),
                                     column(6, plotOutput("distribution2")))
                        ),
                        tabPanel("Resumen del Modelo Semanal", verbatimTextOutput("summary_sem")), # Model output
                        tabPanel("Data semanal histórica", DT::dataTableOutput('tbl')) # Data as datatable
                        
            )
        )
    ))



# SERVER
server <- function(input, output) {
    

    #load XY data semanal historica
    load(file = "Data_sem1.Rda")
    XY_Sem_shiny <- Data_sem1
    
    #grafica historica semanal plotly
    output$Serie_semanal <- renderPlotly({plot_ly (data=XY_Sem_shiny,
                                                       x = ~SEMANA,
                                                       y = ~Total_Accidentes,
                                                       split = ~ANO,
                                                       type = "scatter" ,mode = "lines",
                                                       #line=list(width=1,color='rgb(205, 12, 24)'))%>%
                                                       line=list(width=1,color='rgb(66, 51, 255)'))%>%
            layout(title='Accidentalidad Semanal Historica Medellin',
                   xaxis=list(title="SEMANA"),
                   yaxis=list(title="Accidentes"))  })
    
    
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
    
    # Scatterplot output
    output$scatterplot <- renderPlot({
        plot(swiss[,input$indepvar], swiss[,input$outcome], main="Scatterplot",
             xlab=input$indepvar, ylab=input$outcome, pch=19)
        abline(lm(swiss[,input$outcome] ~ swiss[,input$indepvar]), col="red")
        lines(lowess(swiss[,input$indepvar],swiss[,input$outcome]), col="blue")
    }, height=400)
    
    
    # Histogram output var 1
    output$distribution1 <- renderPlot({
        hist(swiss[,input$outcome], main="", xlab=input$outcome)
    }, height=300, width=300)
    
    # Histogram output var 2
    output$distribution2 <- renderPlot({
        hist(swiss[,input$indepvar], main="", xlab=input$indepvar)
    }, height=300, width=300)
}

shinyApp(ui = ui, server = server)


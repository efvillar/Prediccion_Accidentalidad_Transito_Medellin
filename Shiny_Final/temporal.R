#cargar datos

load(file = "Dias_Especiales_Mensuales.Rda")
load(file = "Dias_Especiales_Semanal.Rda")
load(file = "Frecuencia_De_Accidentes_Diario.Rda")
load(file = "Frecuencia_De_Accidentes_Semanal.Rda")
XY_Sem <- Total_Dataset_Freq_M
load(file = "Frecuencia_De_Accidentes_Mensual.Rda")


# Modelo Semanal de Pueba

library(reshape)
#XY_Sem$ANO <- NULL
str(XY_Sem)
Data_sem <- cast(XY_Sem[,c(1,2,3,4)],ANO+SEMANA~GRAVEDAD, sum)
Data_sem$Total_Accidentes <- Data_sem$ACCIDENTES_GRAVES+Data_sem$ACCIDENTES_LEVES
Data_sem$ANO1 <- Data_sem$ANO
Data_sem$SEMANA1 <- Data_sem$SEMANA
library(dplyr)
Data_sem<-unite_(Data_sem, "Ano_Sem", c("ANO1","SEMANA1"))

XY_Sem$ANO1 <- XY_Sem$ANO
XY_Sem$SEMANA1 <- XY_Sem$SEMANA
XY_Sem<-unite_(XY_Sem, "Ano_Sem", c("ANO1","SEMANA1"))
XY_Sem1 <- XY_Sem
XY_Sem1$FREQ<-NULL
XY_Sem1$GRAVEDAD<-NULL
XY_Sem1$ANO<-NULL
XY_Sem1$SEMANA<-NULL


XY_Sem1<-distinct(XY_Sem1,Ano_Sem, .keep_all= TRUE)


library(sqldf)
Data_sem1 <- sqldf("SELECT *
              FROM Data_sem
              LEFT JOIN XY_Sem1 USING(Ano_Sem)")
head(Data_sem1)
Data_sem1$Ano_Sem <- NULL


trcntrl = trainControl(method="cv", number=10)
model_sem = caret::train(Total_Accidentes~ANO+SEMANA+Feria_Flores+Semana_Santa+Feriados_Lunes+Feriados_Otros, data=Data_sem1,
                            method = "lm", trControl = trcntrl,
                            preProcess=c("center", "scale"),
                            tuneLength = 10)

summary(model_sem)

saveRDS(model_sem,"model_sem.rds")

save(Data_sem1,file="Data_sem1.Rda")

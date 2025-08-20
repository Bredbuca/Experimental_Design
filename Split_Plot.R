#diseños en factores dificiles de variar
#(parcelas divididas y exensones)

#Arreglos completamente aleatorizados
#ejercicio Lawson (2015), página 308

#diseño del experimento y arreglo de tratamientos
#con algDesign
install.packages("AlgDesign")
library(AlgDesign)
sp <- expand.grid(trayT = factor( c("RoomT", "Hot")),
                   bakeT = factor( c("low", "mid", "high") ))
sp
wp <- data.frame(short = factor( c("100%", "80%") ))
wp <- rbind(wp, wp)
wp
splitP <- optBlock( ~ short * (trayT + bakeT + trayT:bakeT), 
                    withinData = sp, blocksizes = rep(6, 4),
                      wholeBlockData = wp)
splitP
#con Agricolae
library(agricolae)
str(design.split)
trt1<-c("A","B","C","D")
trt2<-c("a","b","c")
outdesign <-design.split(trt1,trt2,r=3,serie=2,seed=543)
head(outdesign$book)
outdesign$parameters

#análisis del experimento de galletas DAEWR
library(daewr)
data(splitPdes)
help(splitPdes)
splitPdes
install.packages("GAD")
library(GAD)
Short <- as.fixed(splitPdes$short)
Batch <- as.random(splitPdes$batch)
BakeT <- as.fixed(splitPdes$bakeT)
TrayT <- as.fixed(splitPdes$trayT)
model <- aov(y ~ Short + Short%in%Batch + BakeT +
                       + TrayT + Short*BakeT + Short*TrayT + BakeT*TrayT +
                       + Short*BakeT*TrayT, data = splitPdes)
gad(model)

#análisis del experimetno con LME4
install.packages("lme4", type = "source")
library(lme4)
install.packages("Matrix")
install.packages("Matrix", repos = "http://R-Forge.R-project.org")
rmodel <- lmer(y ~ 1 + short + bakeT + trayT + short:bakeT +
                          short:trayT + bakeT:trayT + short:bakeT:trayT +
                         (1|short:batch), data = splitPdes)
anova(rmodel)

#anovas split plot complatamente aleatorizada
#Con paquete aov
library(daewr)
data(splitPdes)
names(splitPdes)
help("splitPdes")
model_1<-aov(y ~short+Error(short:batch)+short*BakeT*TrayT,data=splitPdes)
summary(model_1)
str(splitPdes)
splitPdes

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


# Verificacion Bondad de ajuste
#variacion parcela principal
cv<-100*sqrt(0.084)/mean(splitPdes$y)
cv
#variacion subparcela
cv2<-100*sqrt(0.0098)/mean(splitPdes$y)
cv2

#validacion de supuestos
# en arreglo de parcela principal
mod_1_pp<-aov(y ~ short+Error(short:batch), data=splitPdes)
summary(mod_1_pp)# El modelo se reparametriza.
qqnorm(residuals(mod_1_pp))
bartlett.test(splitPdes$y~splitPdes$short)
plot(fitted(mod_1_pp))

#unicamente para ver residuales, porque no se esta teniendo encuenta
# el error.
mod_0<-aov(splitPdes$y~splitPdes$short)
plot(residuals(mod_0))
library(MASS)
boxcox(mod_0)
hist(residuals(mod_0))
shapiro.test(residuals(mod_0))
#quedando sin el modelo sin transformar
interaction.plot(splitPdes$short, splitPdes$trayT, splitPdes$y, mean)



library(agricolae)
#prueba de tukey para parcela principall
print(HSD.test(splitPdes$y,splitPdes$short,2, 0.084))

#prueba de tukey para temperatura del horno
print(HSD.test(splitPdes$y,splitPdes$bakeT,10, 0.098))

print(HSD.test(splitPdes$y,splitPdes$trayT,10, 0.098))



#anovas split plot complatamente aleatorizada Casella 2010 DIETAS HIPERTENSION
#Con paquete aov
library(daewr)
data(splitPdes)
names(splitPdes)
model_1<-aov(y ~short+Error(short:batch)+short*BakeT*TrayT,data=splitPdes)
summary(model_1)
str(splitPdes)
splitPdes

# Verificacion Bondad de ajuste
#variacion parcela principal
cv<-100*sqrt(0.084)/mean(splitPdes$y)
cv
#variacion subparcela
cv2<-100*sqrt(0.0098)/mean(splitPdes$y)
cv2

#validacion de supuestos
# en arreglo de parcela principal
mod_1_pp<-aov(y ~ short+Error(short:batch), data=splitPdes)
summary(mod_1_pp)# El modelo se reparametriza.
qqnorm(residuals(mod_1_pp))
bartlett.test(splitPdes$y~splitPdes$short)
plot(fitted(mod_1_pp))

#unicamente para ver residuales, porque no se esta teniendo encuenta
# el error.
mod_0<-aov(splitPdes$y~splitPdes$short)
plot(residuals(mod_0))
library(MASS)
boxcox(mod_0)
hist(residuals(mod_0))
shapiro.test(residuals(mod_0))
#quedando sin el modelo sin transformar
interaction.plot(splitPdes$short, splitPdes$trayT, splitPdes$y, mean)



library(agricolae)
#prueba de tukey para parcela principall
print(HSD.test(splitPdes$y,splitPdes$short,2, 0.084))

#prueba de tukey para temperatura del horno
print(HSD.test(splitPdes$y,splitPdes$bakeT,10, 0.098))

print(HSD.test(splitPdes$y,splitPdes$trayT,10, 0.098))



# parcelas divididas, analisis mediante modelos mixtos
anova(model_1)
summary(model_1)

#test f manual
1-pt(0.1026/0.0004,1,2)

model2<-lmer(y ~ 1  + bakeT + trayT + short:bakeT +
               short:trayT + bakeT:trayT + short:bakeT:trayT +
             (1|short:batch), data = splitPdes, REML = FALSE)
model3<-lmer(y ~ 1 + short + bakeT + trayT + short:bakeT +
               short:trayT + bakeT:trayT + short:bakeT:trayT +
               (1|short:batch), data = splitPdes, REML = FALSE)


#parcelas divididas arregladas en bloques
#casella
library(readxl)
var.alfalfa <- read.table("C:/Users/Tuptc/Downloads/VarietySP.txt", h=T)
View(var.alfalfa) 

#ANÁLISIS EXPLORATORIO

#Numérico
head(var.alfalfa)
str(var.alfalfa)
summary(var.alfalfa)
attach(var.alfalfa)

boxplot(Y~Variety,data = var.alfalfa)
boxplot(Y~Trt,data = var.alfalfa)


#ANALISIS ESTADISTICO
p<-as.factor(var.alfalfa$P)
k<-as.factor(var.alfalfa$K)

modalf<-aov(Y~Rep + p:Rep + k:Rep + Variety +)


# Diseños compleamente aleatorizados
# Especializacion en estadistica

# Determinación del número de repeticiones
install.packages("daewr")
library(daewr)
rmin <-2 #smallest number of replicates considered
rmax <-10 # largest number of replicates considered
alpha <- rep(0.05, rmax - rmin +1)
alpha
sigma <-1.058
sigma
nlev <- 4
nreps <- rmin:rmax
Delta <- 5
power <- Fpower1(alpha,nlev,nreps,Delta,sigma)
power

#arreglo de campo
set.seed(7638)
f <- factor( rep( c("T0", "T1", "T2", "T3", "T4" ), each = 10))
fac <- sample( f, 50)
fac
eu <- 1:50
plan <- data.frame( parcela=eu, tratamientos=fac )
plan
write.csv( plan, file = "Plan.csv", row.names = FALSE)

#Ejercicio 4 capitulo 2 Lawson
library(readxl)
PolvoHornear <- read_excel("PolvoHornear.xlsx")
View(PolvoHornear)
attach(PolvoHornear)

#análisis descriptivo
summary(PolvoHornear)
hist(Altura)
boxplot(Altura~Tratamiento)
std<-sqrt(var(Altura))
std
#ANOVA a una vía CRD
mod_1<-aov(Altura~Tratamiento)
anova(mod_1)
X<-model.matrix(mod_1)
qr(X)$rank
plot(mod_1)
shapiro.test(resid(mod_1))


#MODELAMIENTO DE CRD con falsas repeticiones
peces<-read.table("FishTank.txt", h=T)
attach(peces)
summary(peces)

#modelamiento sin corrección de variables
mod1<-aov(WtGain~Diet)
anova(mod1)

#correccion por tipo de variable
#mal por estructura del error
Diet<-as.factor(Diet)
mod2<-aov(WtGain~Diet)
anova(mod2)
summary(mod2)

#modelamiento correcto
mod3<-aov(WtGain~Diet + Error(Tank/Diet))
#anova(mod3)
summary(mod3)

#modelamiento anova completo con dos FV
Tank<-as.factor(Tank)
mod4<-aov(WtGain~Diet + Tank)
anova(mod4)
summary(mod4)

#PARA GENERAR LA MATRIZ DE DISEÑO
mod5<-aov(WtGain~Diet+Tank/Diet)
X<-model.matrix(mod5)
qr(X)$rank

#PRUEBAS DE COMPARACION MULTIPLE
install.packages("agricolae")
library(agricolae)
help("agricolae")
print(HSD.test(peces$WtGain, peces$Diet, 9, 12, alpha= 0.05))

#reparticion temas exposición
set.seed(7638)
f <- factor( rep( c("G1 Pedro Estepa", 
                    "G2 Nicolás Gómez",
                    "G3 Paola Angarita",
                    "G4 Wilson Buitrago"), each = 1))
fac <- sample( f, 4)
fac
eu <- 1:4
plan <- data.frame(tema=eu, grupos=fac )
plan

#EJERCICIO DATOS DE FOTOSINTESIS
#DCA con 3 rep y 6 subrep
#se mide fotosintesis neta en micromoles de CO2 por m2 por seg
#T0=quimico, T1= quim + biologico
#las subrepeticiones son hojas seleccionadas al azar en cada UE
library(readxl)
IRGA <- read_excel("IRGA.xlsx")
View(IRGA)
#analisis exploratorio
#attach(IRGA)
summary(IRGA)
str(IRGA)
hist(IRGA$Fotosintesis)
boxplot(IRGA$Fotosintesis~IRGA$Tratamiento)
#Modelamiento
#modelo errado, no se consideran las subrepeticiones
mod_0<-aov(Fotosintesis~Tratamiento, data = IRGA)
anova(mod_0)
hist(resid(mod_0))
shapiro.test(resid(mod_0))
bartlett.test(Fotosintesis~Tratamiento, data = IRGA)
plot(mod_0)
#modelo con test F correcto
mod_1<-aov(Fotosintesis~Tratamiento + 
                   Error(Repeticion:Tratamiento),IRGA)
summary(mod_1)
#MODELANDO PROMEDIANDO LAS SEIS SUBREPETICIONES
library(readxl)
IRGA_2 <- read_excel("IRGA_2.xlsx")
View(IRGA_2)
summary(IRGA_2)
boxplot(IRGA_2$Fotosintesis~IRGA_2$Tratamiento)
hist(IRGA_2$Fotosintesis)
#modelo test F correcto
mod_2<-aov(Fotosintesis~Tratamiento, IRGA_2)
summary(mod_2)
shapiro.test(resid(mod_2))
bartlett.test(Fotosintesis~Tratamiento, IRGA_2)
plot(mod_2)

#DISEÑOS FACTORIALES COMPLETOS Y ALEATORIZADOS
#EJERCICIO 2.12 CASELLA (2010)
Shrimp<-read.table("Shrimp.txt", h=T)
camaron_1<-read.table("shrimp_1.txt", h=T)
summary(camaron)
attach(camaron)
detach(camaron)
#exploratorio
Shrimp$Temp<-as.factor(Shrimp$Temp)
Shrimp$Density<-as.factor(Shrimp$Density)
Shrimp$Salinity<-as.factor(Shrimp$Salinity)
boxplot(WeightGain~Temp)
boxplot(WeightGain~Density)
boxplot(WeightGain~Salinity)
boxplot(WeightGain~Treatment)
#modelar
mod_1<-aov(Shrimp$WeightGain~Shrimp$Temp*Shrimp$Density*Shrimp$Salinity)
anova(mod_1)
plot(TukeyHSD(mod_1))
TukeyHSD(mod_1)
library(agricolae)
print(HSD.test(Shrimp$WeightGain, Shrimp$Temp, 24,  2904))
print(HSD.test(Shrimp$WeightGain, Shrimp$Density, 24,  2904))
print(HSD.test(Shrimp$WeightGain, Shrimp$Salinity, 24,  2904))
#interacciones
interaction.plot(Shrimp$Salinity, Shrimp$Temp, Shrimp$WeightGain, mean)
interaction.plot(Shrimp$Salinity, Shrimp$Density, Shrimp$WeightGain, mean)
interaction.plot(Shrimp$Density, Shrimp$Temp, Shrimp$WeightGain, mean)
#bondad de ajuste
CV<-100*sqrt(2904)/(mean(WeightGain))
CV
#supuestos
shapiro.test(resid(mod_1))
bartlett.test(camaron_1$WeightGain~camaron_1$Temp)
bartlett.test(WeightGain~Density)
bartlett.test(camaron_1$WeightGain~camaron_1$Salinity)
bartlett.test(WeightGain~Treatment)
plot(mod_1)
camaron
library(MASS)
boxcox(mod_1)
install.packages("car")
library(car)
help(aov)

#usando lm
options(contrasts=c("contr.sum","contr.poly"))

mod_2<-aov(Shrimp$WeightGain~Shrimp$Temp*Shrimp$Density*Shrimp$Salinity)
#Finally, call the drop1 function on each model component:
drop1(mod_2, .~., test="F")



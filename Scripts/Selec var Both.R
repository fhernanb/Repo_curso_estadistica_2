# Ejemplo de selección de variables método Forward

# Pagina 111 Notas de clase prof Carlos Mario

# Se tienen los resultados de cuatro pruebas para la selección de 
# personal (X1; X2; X3; X4) que fueron aplicadas a un grupo de 20 
# aspirantes a cargos administrativos en una agencia gubernamental. 
# Para el propósito del estudio, se aceptaron a todos los aspirantes 
# para 20 cargos independiente de los resultados en las pruebas. 
# Después de 2 años desde la contratación estos empleados fueron 
# evaluados mediante un puntaje de la aptitud exhibida
# para el trabajo (Y)

datos <- read.csv("https://raw.githubusercontent.com/fhernanb/Repo_curso_estadistica_2/main/Datos/Ex01Ch03.csv")

# Voy a cambiar los nombres de las variables a y, x1, x2, x3, x4.
# Hago el cambio por comodidad

colnames(datos) <- c("y", "x1", "x2", "x3", "x4")

# Para cargar las funciones del curso
source("https://raw.githubusercontent.com/fhernanb/Repo_curso_estadistica_2/main/Funciones/funciones.R")

mod <- lm(y ~ x1 + x2 + x3 + x4, data=datos)

myStepwise(full.model=mod, alpha.to.enter=0.05, alpha.to.leave=0.05)



# Ejemplo 2 ---------------------------------------------------------------

# Aquí vamos a usar la base de datos pathoeg que contiene 9 X's
# y vamos a aplicar Forward, Backward y Both (stepwise)
# usando un nivel de significancia de 1%

# Para cargar las funciones del curso
source("https://raw.githubusercontent.com/fhernanb/Repo_curso_estadistica_2/main/Funciones/funciones.R")

# Librería con los datos
library(MPV) 

datos <- table.b3[-c(23, 25), ]
attach(datos)

mod_lleno <- lm(y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11, 
                data=datos)

# Forward
myStepwise(full.model=mod_lleno, alpha.to.enter=0.01, alpha.to.leave=0.00000001)

# Backward
myBackward(base.full=mod_lleno, alpha.to.leave=0.01)

# Both
myStepwise(full.model=mod_lleno, alpha.to.enter=0.01, alpha.to.leave=0.01)

# Ajustando los modelos finales por cada metodo

mod_for <- lm(y ~ x1, data=datos)
mod_bac <- lm(y ~ x10, data=datos)
mod_for <- lm(y ~ x1, data=datos)

# R2 adj
summary(mod_for)$adj.r.squared
summary(mod_bac)$adj.r.squared



# Usando MASS -------------------------------------------------------------

library(MASS)

mod_vacio <- lm(y ~ 1, data=datos)
mod_lleno <- lm(y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11, data=datos)

horizonte <- formula(y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11)

modforw <- stepAIC(object=mod_vacio, trace=FALSE, direction="forward", 
                   scope=horizonte)
modforw$anova

modback <- stepAIC(object=mod_lleno, trace=FALSE, direction="backward")
modback$anova

modboth <- stepAIC(object=mod_vacio, trace=FALSE, direction="both", 
                   scope=horizonte)
modboth$anova



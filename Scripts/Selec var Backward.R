# Ejemplo de selección de variables método Backward

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

# Seleccion Backward manual ------------------------------------------------

# Modelo inicial con todas las covariables
mod0 <- lm(y ~ x1 + x2 + x3 +x4, data=datos)
SSRes0 <- sum(residuals(mod0)^2)

# ----- Iteracion 1 ---------

# Modelos con 1 covariable
mod1 <- lm(y ~      x2 + x3 + x4, data=datos)
mod2 <- lm(y ~ x1      + x3 + x4, data=datos)
mod3 <- lm(y ~ x1 + x2      + x4, data=datos)
mod4 <- lm(y ~ x1 + x2 + x3     , data=datos)

# Sumas de residuales
SSRes1 <- sum(residuals(mod1)^2)
SSRes2 <- sum(residuals(mod2)^2)
SSRes3 <- sum(residuals(mod3)^2)
SSRes4 <- sum(residuals(mod4)^2)

# Suma de cuadrados extra de beta_j | parametros del modelo
SSRextra1 <- SSRes1 - SSRes0
SSRextra2 <- SSRes2 - SSRes0
SSRextra3 <- SSRes3 - SSRes0
SSRextra4 <- SSRes4 - SSRes0

# ¿Cuál es candidata a salir?
SSRextra1
SSRextra2
SSRextra3
SSRextra4

# Candidata a salir X2

# ¿Es X2 significativa?
summary(mod0)

# X2 sale del modelo porque su valor-P = 0.74072 y es mayor que 0.05

# ----- Iteracion 2 ---------

# Modelos sin X2 y sin otra covariable
mod5 <- lm(y ~ x1 + x3, data=datos)
mod6 <- lm(y ~ x1 + x4, data=datos)
mod7 <- lm(y ~ x3 + x4, data=datos)

# Sumas de residuales
SSRes5 <- sum(residuals(mod5)^2)
SSRes6 <- sum(residuals(mod6)^2)
SSRes7 <- sum(residuals(mod7)^2)

# Suma de cuadrados extra de beta_j | parametros del modelo
SSRextra5 <- SSRes5 - SSRes2
SSRextra6 <- SSRes6 - SSRes2
SSRextra7 <- SSRes7 - SSRes2

# ¿Cuál es candidata a salir?
SSRextra5
SSRextra6
SSRextra7

# Candidata a ingresar X1

# ¿Es X1 significativa?
summary(mod2)

# X1 sale del modelo porque su valor-P = 0.165384 y es menor que 0.05

# ----- Iteracion 3 ---------

# Modelos con X3, X4 y otra covariable
mod8 <- lm(y ~ x3, data=datos)
mod9 <- lm(y ~ x4, data=datos)

# Sumas de residuales
SSRes8 <- sum(residuals(mod8)^2)
SSRes9 <- sum(residuals(mod9)^2)

# Suma de cuadrados extra de beta_j | parametros del modelo
SSRextra8 <- SSRes8 - SSRes7
SSRextra9 <- SSRes9 - SSRes7

# ¿Cuál es candidata a salir?
SSRextra8
SSRextra9

# Candidata a salir X4

# ¿Es X4 significativa?
summary(mod7)

# X4 NOOOO sale del modelo porque su valor-P = 0.0040006 y es menor que 0.05

# Modelo final
mod7


# Seleccion automatica Backward --------------------------------------------

# Para cargar las funciones del curso
source("https://raw.githubusercontent.com/fhernanb/Repo_curso_estadistica_2/main/Funciones/funciones.R")

mod <- lm(y ~ x1 + x2 + x3 + x4, data=datos)

attach(datos)

myBackward(base.full=mod, alpha.to.leave=0.05)



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

# Seleccion Forward manual ------------------------------------------------

# Modelo inicial sin covariables
mod0 <- lm(y ~ 1, data=datos)
SSRes0 <- sum(residuals(mod0)^2)

# ----- Iteracion 1 ---------

# Modelos con 1 covariable
mod1 <- lm(y ~ x1, data=datos)
mod2 <- lm(y ~ x2, data=datos)
mod3 <- lm(y ~ x3, data=datos)
mod4 <- lm(y ~ x4, data=datos)

# Sumas de residuales
SSRes1 <- sum(residuals(mod1)^2)
SSRes2 <- sum(residuals(mod2)^2)
SSRes3 <- sum(residuals(mod3)^2)
SSRes4 <- sum(residuals(mod4)^2)

# Suma de cuadrados extra de beta_j | parametros del modelo
SSRextra1 <- SSRes0 - SSRes1
SSRextra2 <- SSRes0 - SSRes2
SSRextra3 <- SSRes0 - SSRes3
SSRextra4 <- SSRes0 - SSRes4

# ¿Cuál es candidata a entrar?
SSRextra1
SSRextra2
SSRextra3
SSRextra4

# Candidata a ingresar X3

# ¿Es X3 significativa?
summary(mod3)

# X3 ingresa al modelo porque su valor-P = 2.09e-07 y es menor que 0.05

# ----- Iteracion 2 ---------

# Modelos con X3 y otra covariable
mod5 <- lm(y ~ x3 + x1, data=datos)
mod6 <- lm(y ~ x3 + x2, data=datos)
mod7 <- lm(y ~ x3 + x4, data=datos)

# Sumas de residuales
SSRes5 <- sum(residuals(mod5)^2)
SSRes6 <- sum(residuals(mod6)^2)
SSRes7 <- sum(residuals(mod7)^2)

# Suma de cuadrados extra de beta_j | parametros del modelo
SSRextra5 <- SSRes3 - SSRes5
SSRextra6 <- SSRes3 - SSRes6
SSRextra7 <- SSRes3 - SSRes7

# ¿Cuál es candidata a entrar?
SSRextra5
SSRextra6
SSRextra7

# Candidata a ingresar X4

# ¿Es X4 significativa?
summary(mod7)

# X4 ingresa al modelo porque su valor-P = 0.004006 y es menor que 0.05

# ----- Iteracion 3 ---------

# Modelos con X3, X4 y otra covariable
mod8 <- lm(y ~ x3 + x4 + x1, data=datos)
mod9 <- lm(y ~ x3 + x4 + x2, data=datos)

# Sumas de residuales
SSRes8 <- sum(residuals(mod8)^2)
SSRes9 <- sum(residuals(mod9)^2)

# Suma de cuadrados extra de beta_j | parametros del modelo
SSRextra8 <- SSRes7 - SSRes8
SSRextra9 <- SSRes7 - SSRes9

# ¿Cuál es candidata a entrar?
SSRextra8
SSRextra9

# Candidata a ingresar X1

# ¿Es X1 significativa?
summary(mod8)

# X1 NOOOO ingresa al modelo porque su valor-P = 0.165384 y es mayor que 0.05

# Modelo final
mod7


# Seleccion automatica Forward --------------------------------------------

# Para cargar las funciones del curso
source("https://raw.githubusercontent.com/fhernanb/Repo_curso_estadistica_2/main/Funciones/funciones.R")

mod <- lm(y ~ x1 + x2 + x3 + x4, data=datos)

myStepwise(full.model=mod, alpha.to.enter=0.05, alpha.to.leave=0.05)



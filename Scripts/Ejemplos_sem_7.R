
library(MPV)
colnames(softdrink) <- c("tiempo", "cantidad", "distancia")

mod <- lm(tiempo ~ cantidad + distancia, data=softdrink)

# Ejemplo PH para beta_k en RLM -------------------------------------------
library(model)
beta_test(object=mod, parm="distancia", 
          ref.value=0.021, alternative="two.sided")

# Ejemplo IC para beta_k en RLM -------------------------------------------

# Part a
confint(object=mod, parm="cantidad", level=0.96)
# El cuantil
qt(p=0.02, df=22, lower.tail=FALSE)

# Part b
confint(object=mod, parm="distancia", level=0.90)
# El cuantil
qt(p=0.05, df=22, lower.tail=FALSE)


# Ejemplo PH respuesta media ----------------------------------------------

library(MPV)
colnames(softdrink) <- c("tiempo", "cantidad", "distancia")
head(softdrink)

mod <- lm(tiempo ~ cantidad + distancia, data=softdrink, x=TRUE)

X <- mod$x
x0 <- matrix(c(1, 5, 100), nrow=1)

x0 %*% solve(t(X) %*% X) %*% t(x0)

qt(p=0.015, df=22, lower.tail=FALSE)





# Ejercicio 1 -------------------------------------------------------------

x1 <- c(3, 5, 3, 2)
x2 <- c(1, 3, 2, 4)
#y <- 1 + 1 * x1 - 2 * x2 + rnorm(n=length(x1))
#y <- round(y, digits=0)
y <- c(2, -1, 0, -5)

cbind(x1, x2, y)

# a)
# Calculando estimadores con R
mod <- lm(y ~ x1+ x2)
coef(mod)

# Calculando los estimadores usando matrices
X <- cbind(1, x1, x2)
y <- matrix(y, ncol=1)

betas <- solve(t(X) %*% X) %*% t(X) %*% y
betas

# b) 
y_hat <- 2.34 + 0.62 * x1 - 2.14 * x2
data.frame(x1, x2, y, y_hat)

# c)
sigma2_hat <- sum((y-y_hat)^2) / (4-3)
sigma2_hat

# d) 
# Se interpreta beta como siempre

# e) 
ei <- y - y_hat
di <- ei / sqrt(sigma2_hat)
di <- round(di, digits=2)
data.frame(y, y_hat, ei, di)




# Anova para RLM ejemplo ventas dado publicidad y vendedores --------------

x1 <- c(5, 4, 1, 8, 10, 3)
x2 <- c(12, 8, 9, 15, 10, 6)
set.seed(12345)
ruido <- rnorm(n=6, sd=5)
y <- 2 + 8 * x1 + 7 * x2 + ruido
y <- round(y, digits=1)
cbind(y, x1, x2)

mod <- lm(y ~ x1 + x2)
summary(mod)

# Tabla para mostrar enunciado
y_hat <- round(fitted(mod), digits=1)
cbind(y, y_hat)

sigma2_hat <- sum((y-y_hat)^2) / (6-3)
sigma2_hat

mean(y)

SSR <- sum((y_hat - mean(y))^2)
SSE <- sum((y - y_hat)^2)
SST <- sum((y - mean(y))^2)

k <- 2
p <- k + 1
n <- 6

MSR <- SSR / k
MSE <- SSE / (n-p)

F0 <- MSR / MSE
F0

qf(p=1-0.07,k2, n-p)

# Prueba de la hipótesis lineal general -----------------------------------

x1 <- c(3, 5, 3, 2, 6, 3)
x2 <- c(1, 3, 2, 4, 1, 1)
x3 <- c(1, 5, 1, 2, 3, 7)
x4 <- c(4, 3, 7, 4, 6, 8)
y <- 1 - 2 * x1 - 2 * x2 + 3 * x3 + 3 * x4 + rnorm(n=length(x1))
#y <- round(y, digits=0)
y <- c(8, 8, 15, 7, 14, 39)

cbind(x1, x2, x3, x4, y)

# Full model
FM <- lm(y ~ x1 + x2 + x3 + x4)
MSE_FM <- sigma(FM)^2

# Reduced model
x12 <- x1 + x2
x34 <- x3 + x4
RM <- lm(y ~ x12 + x34)

SSE_FM <- sum((y-fitted(FM))^2)
SSR_FM <- sum((fitted(FM)-mean(y))^2)

SSE_RM <- sum((y-fitted(RM))^2)
SSR_RM <- sum((fitted(RM)-mean(y))^2)

SSH <- SSE_RM - SSE_FM
r <- 2

MSH <- SSH / r

F0 <- MSH / MSE_FM
F0

valor_critico <- qf(p=0.02, df1=r, df2=6-5, lower.tail=FALSE)
valor_critico

valor_p <- pf(q=F0, df1=r, df2=6-5, lower.tail=FALSE)
valor_p


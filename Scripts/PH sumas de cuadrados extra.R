
require(MPV)
library(olsrr)

data(table.b4) 
head(table.b4, n=4)

n <- nrow(table.b4)

mod <- lm(y ~ x1 + x2 + x3 + x4, data=table.b4)
k <- ols_step_all_possible(mod)
k$result

k$result$SSE <- (k$result$rmse)^2 * n
k$result

# Para ver la tabla
k$result[, c("n", "predictors", "SSE")]

# Ejemplo a
# Usando R
redu_mod <- lm(y ~ x2 + x3, data=table.b4)
comp_mod <- lm(y ~ x1 + x2 + x3 + x4, data=table.b4)

n <- 24 # numero de observaciones
p0 <- 3 # numero de betas en modelo reducido
p1 <- 5 # numero de betas en modelo completo

ssr_reduced  <- sum(table.b4$y) - sum(redu_mod$residuals^2)
ssr_complete <- sum(table.b4$y) - sum(comp_mod$residuals^2)
ms_res <- summary(comp_mod)$sigma^2
F0 <- ((ssr_complete - ssr_reduced) / (p1-p0)) / ms_res
F0
pf(q=F0, df1=p1-p0, df2=n-p1, lower.tail=FALSE)

anova(redu_mod, comp_mod, test="F")

# Ejemplo b
# Usando R
redu_mod <- lm(y ~ x2 + x4, data=table.b4)
comp_mod <- lm(y ~ x1 + x2 + x3 + x4, data=table.b4)

n <- 24 # numero de observaciones
p0 <- 3 # numero de betas en modelo reducido
p1 <- 5 # numero de betas en modelo completo

ssr_reduced  <- sum(table.b4$y) - sum(redu_mod$residuals^2)
ssr_complete <- sum(table.b4$y) - sum(comp_mod$residuals^2)
ms_res <- summary(comp_mod)$sigma^2
F0 <- ((ssr_complete - ssr_reduced) / (p1-p0)) / ms_res
F0
pf(q=F0, df1=p1-p0, df2=n-p1, lower.tail=FALSE)

anova(redu_mod, comp_mod, test="F")



# Esta funcion calcula IC para una obs futura

ic_obs_fut_rls <- function(x0, mod, conf.level=0.95) {
  
  alpha <- 1 - conf.level
  
  mod <- update(mod, x=TRUE, y=TRUE)
  
  x <- mod$x[, 2]
  y <- mod$y
  
  n <- length(y)
  x_bar <- mean(x)
  Sxx <- sum((x-x_bar)^2)
  
  sigma2_hat <- summary(mod)$sigma^2
  
  Y0 <- coef(mod)[1] + coef(mod)[2] * x0
  Varianza <- sigma2_hat * (1 + 1/n + (x0-x_bar)^2/Sxx)
  
  t_alpha_medio <- qt(p=alpha/2, df=n-2, lower.tail = FALSE)
  lim_inf <- Y0 - t_alpha_medio * sqrt(Varianza)
  lim_sup <- Y0 + t_alpha_medio * sqrt(Varianza)
  
  list(n=n, x_bar=x_bar, Sxx=Sxx,
       sigma2_hat=sigma2_hat,
       Y0=Y0,
       t_alpha_medio=t_alpha_medio,
       lim_inf=lim_inf, lim_sup=lim_sup)
}


file <- "https://raw.githubusercontent.com/fhernanb/datos/master/propelente"
datos <- read.table(file=file, header=TRUE)

mod <- lm(Resistencia ~ Edad, data=datos)

ic_obs_fut_rls(x0=10, mod=mod, conf.level=0.95)
ic_obs_fut_rls(x0=10, mod=mod, conf.level=0.90)


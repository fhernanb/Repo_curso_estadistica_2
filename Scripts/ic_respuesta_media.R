# Esta funcion calcula Y0 y la varianza de Y0 para un valor X=x0

ic_rta_media_rls <- function(x0, mod, conf.level=0.95) {
  
  alpha <- 1 - conf.level
  
  mod <- update(mod, x=TRUE, y=TRUE)
  
  x <- mod$x[, 2]
  y <- mod$y
  
  n <- length(y)
  x_bar <- mean(x)
  Sxx <- sum((x-x_bar)^2)
  
  sigma2_hat <- summary(mod)$sigma^2
  
  Y0 <- coef(mod)[1] + coef(mod)[2] * x0
  Var_Y0 <- sigma2_hat * (1/n + (x0-x_bar)^2/Sxx)
  
  t_alpha_medio <- qt(p=alpha/2, df=n-2, lower.tail = FALSE)
  lim_inf <- Y0 - t_alpha_medio * sqrt(Var_Y0)
  lim_sup <- Y0 + t_alpha_medio * sqrt(Var_Y0)
  
  list(n=n, x_bar=x_bar, Sxx=Sxx,
       sigma2_hat=sigma2_hat,
       Y0=Y0, Var_Y0=Var_Y0,
       t_alpha_medio=t_alpha_medio,
       lim_inf=lim_inf, lim_sup=lim_sup)
}


file <- "https://raw.githubusercontent.com/fhernanb/datos/master/propelente"
datos <- read.table(file=file, header=TRUE)

mod <- lm(Resistencia ~ Edad, data=datos)

ic_rta_media_rls(x0=13, mod=mod, conf.level=0.95)
ic_rta_media_rls(x0= 2, mod=mod, conf.level=0.95)
ic_rta_media_rls(x0=13.3625, mod=mod, conf.level=0.95)

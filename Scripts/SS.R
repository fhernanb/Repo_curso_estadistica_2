# Funcion para obtener todas las sumas de cuadrados

SS <- function(x, y) {
  x_bar <- mean(x)
  y_bar <- mean(y)
  n <- length(y)
  
  Sxx <- sum((x-x_bar)^2)
  Sxx_alt <- sum(x^2) - n * x_bar^2
  
  Syy <- sum((y-y_bar)^2)
  Syy_alt <- sum(y^2) - n * y_bar^2
  
  Sxy <- sum((x-x_bar)*(y-y_bar))
  Sxy_alt <- sum((x-x_bar)*y)
  
  # Estimations
  b1_hat <- Sxy / Sxx
  b0_hat <- y_bar - b1_hat * x_bar
  
  y_hat <- b0_hat + b1_hat * x
  ei <- y - y_hat
  sigma2_hat <- sum(ei^2) / (n-2)
  sigma_hat <- sqrt(sigma2_hat)
  
  # Standard errors
  se_b0_hat <- sqrt(sigma2_hat * sum(x^2) / (n * Sxx))
  se_b1_hat <- sqrt(sigma2_hat / Sxx)
  
  # Sum Squared
  SST <- Syy
  SST_alt <- sum(y^2) - n * y_bar^2
  
  SSR <- sum((y_hat - y_bar)^2)
  SSR_alt1 <- b1_hat * Sxy
  SRR_alt2 <- b1_hat^2 * Sxx
  
  SSres <- sum(ei^2)
  SSres_alt <- SST - b1_hat * Sxy
  
  res <- list(x_bar=x_bar, y_bar=y_bar, n=n,
              Sxx=Sxx, Sxx_alt=Sxx_alt,
              Syy=Syy, Syy_alt=Syy_alt,
              Sxy=Sxy, Sxy_alt=Sxy_alt,
              b0_hat=b0_hat, b1_hat=b1_hat,
              sigma2_hat=sigma2_hat,
              sigma_hat=sigma_hat,
              se_b0_hat=se_b0_hat,
              se_b1_hat=se_b1_hat,
              SST=SST, SST_alt=SST_alt,
              SSR=SSR, SSR_alt1=SSR_alt1, SRR_alt2=SRR_alt2,
              SSres=SSres, SSres_alt=SSres_alt)
  return(res)
}

file <- "https://raw.githubusercontent.com/fhernanb/datos/master/propelente"
datos <- read.table(file=file, header=TRUE)

SS(x=datos$Edad, y=datos$Resistencia)




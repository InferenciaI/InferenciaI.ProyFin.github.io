Fhats = sapply(1:1000, function(i){
  set.seed(i)
  X = rnorm(100) 
  return(ecdf(X))
})

puntos = 3*(1:1000-500)/500


Bandas = sapply(puntos, function(p){
  quantile(x = sapply(Fhats, function(cds){cds(p)}), probs = c(0.025,0.975))
})

Bandas = t(Bandas)

plot(pnorm(puntos) ~ puntos, type = "l", ylim = c(-0.1,1.1),
     main = "Función de Distribución N(0,1)",
     xlab = "Cuantiles", ylab = "Probabilidad acumulada")

points(Bandas[,1] ~ puntos, type = "l", col = "blue")
points(Bandas[,2] ~ puntos, type = "l", col = "red")


################################################################################


Fhats = sapply(1:1000, function(i){
  set.seed(i)
  X = rcauchy(100)
  return(ecdf(X))
})

puntos = 20*(1:1000-500)/500


Bandas = sapply(puntos, function(p){
  quantile(x = sapply(Fhats, function(cds){cds(p)}), probs = c(0.025,0.975))
})

Bandas = t(Bandas)

plot(pcauchy(puntos) ~ puntos, type = "l", ylim = c(-0.1,1.1),
     main = "Función de Distribución Cauchy(0,1)",
     xlab = "Cuantiles", ylab = "Probabilidad acumulada")

points(Bandas[,1] ~ puntos, type = "l", col = "blue")
points(Bandas[,2] ~ puntos, type = "l", col = "red")



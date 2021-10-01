############################################################################################
puntos = 3*(1:1000-500)/500
set.seed(1)

n = 100
X = rnorm(n)
Fhat = ecdf(X)

alfa = 0.05

en = sqrt(log(2/alfa)/(2*n))
UB = sapply(puntos, function(p){min(c(Fhat(p)+en,1))})
LB = sapply(puntos, function(p){max(c(Fhat(p)-en,0))})
Freal = pnorm(puntos)

plot(Fhat, verticals = TRUE, do.points = FALSE, col = "gold4",
     main = "Función de distribución N(0,1)",
     xlab = "Cuantiles de X",
     ylab = "P(X<=x)")
points(UB~puntos, type ="l", col = "firebrick4")
points(LB~puntos, type ="l", col = "darkslateblue")
points(Freal~puntos, type ="l")

############################################################################################
puntos = (1:1000-500)/500
set.seed(3)

n = 100
X = rcauchy(n)
puntos = max(abs(X))*puntos
Fhat = ecdf(X)

alfa = 0.05

en = sqrt(log(2/alfa)/(2*n))
UB = sapply(puntos, function(p){min(c(Fhat(p)+en,1))})
LB = sapply(puntos, function(p){max(c(Fhat(p)-en,0))})
Freal = pcauchy(puntos)

plot(Fhat, verticals = TRUE, do.points = FALSE, col = "gold4",
     main = "Función de distribución Cauchy(0,1)",
     xlab = "Cuantiles de X",
     ylab = "P(X<=x)")
points(UB~puntos, type ="l", col = "firebrick4")
points(LB~puntos, type ="l", col = "darkslateblue")
points(Freal~puntos, type ="l")




############################################################################################
#Simulación normal
############################################################################################
puntos = (1:1000-500)/500
alfa = 0.05
n = 100
en = sqrt(log(2/alfa)/(2*n))

contenciones = sapply(2:1001, function(i){
  set.seed(i)
  X = rnorm(n)
  Fhat = ecdf(X)
  puntos = max(abs(X) + 0.001)*puntos
  UB = sapply(puntos, function(p){min(c(Fhat(p)+en,1))})
  LB = sapply(puntos, function(p){max(c(Fhat(p)-en,0))})
  Freal = pnorm(puntos)
  limsup = (Freal<=UB)
  liminf = (LB <= Freal)
  all(apply(cbind(liminf,limsup), 1, all))
})

table(contenciones)
#contenciones
#FALSE  TRUE 
#44   956 


Bandas = sapply(puntos, function(p){
  x|quantile(x = sapply(Fhats, function(cds){cds(p)}), probs = c(0.025,0.975))
})

Bandas = t(Bandas)

plot(pnorm(puntos) ~ puntos, type = "l", ylim = c(-0.1,1.1),
     main = "Función de Distribución N(0,1)",
     xlab = "Cuantiles", ylab = "Probabilidad acumulada")

points(Bandas[,1] ~ puntos, type = "l", col = "blue")
points(Bandas[,2] ~ puntos, type = "l", col = "red")

############################################################################################
#Simulación cauchy
############################################################################################
puntos = (1:1000-500)/500
alfa = 0.05
n = 100
en = sqrt(log(2/alfa)/(2*n))

contenciones = sapply(4:1003, function(i){
  set.seed(i)
  X = rcauchy(n)
  Fhat = ecdf(X)
  puntos = max(abs(X) + 0.001)*puntos
  UB = sapply(puntos, function(p){min(c(Fhat(p)+en,1))})
  LB = sapply(puntos, function(p){max(c(Fhat(p)-en,0))})
  Freal = pcauchy(puntos)
  limsup = (Freal<=UB)
  liminf = (LB <= Freal)
  all(apply(cbind(liminf,limsup), 1, all))
})

table(contenciones)
#contenciones
#FALSE  TRUE 
#22   978 


#Convergencia del intervalo
convergencia = sapply(1:B, function(i){
  TnBs = TnB[1:i]
  
  VarB = mean((TnBs - mean(TnBs))^2)
  
  #Intervalo normal
  InfN = largo - qnorm(1-alfa/2)*sqrt(VarB)
  SupN = largo + qnorm(1-alfa/2)*sqrt(VarB)
  
  #Intervalo de los cuantiles Bootstrap
  IntervaloB = quantile(x = TnBs, probs = c(alfa/2,1-alfa/2))
  
  #Intervalo del Bootstrap pivotal
  InfBpiv = 2*largo - IntervaloB[2]
  SupBpiv = 2*largo - IntervaloB[1]
  
  resultado = list()
  resultado$InfN = InfN
  resultado$SupN = SupN
  resultado$InfB = IntervaloB[1]
  resultado$SupB = IntervaloB[2]
  resultado$InfBpiv = InfBpiv
  resultado$SupBpiv = SupBpiv
  
  return(resultado)
})
#Largo real
largoR = (qt(0.75, df = 3) - qt(0.25, df = 3))/1.34
#Intervalos normales
N = 30
plot(unlist(convergencia[1,1:N]), type = "l", col = "blue", 
     ylim = c(0.5,1.5), main = "Intervalo normal",
     xlab = "Número de simulaciones (B)",
     ylab = "Intervalo al 95%")
points(unlist(convergencia[2,1:N]), type = "l", col = "red")
points(rep(largoR,N), type = "l")

#Intervalos intercuartiles
N = 2000
plot(unlist(convergencia[3,1:N]), type = "l", col = "blue", 
     ylim = c(0.5,1.6), main = "Intervalo intercuantil Bootstrap",
     xlab = "Número de simulaciones (B)",
     ylab = "Intervalo al 95%")
points(unlist(convergencia[4,1:N]), type = "l", col = "red")
points(rep(largoR,N), type = "l")


#Intervalos intercuartiles
N = 2000
plot(unlist(convergencia[5,1:N]), type = "l", col = "blue", 
     ylim = c(0.4,1.5), main = "Intervalo pivotal Bootstrap",
     xlab = "Número de simulaciones (B)",
     ylab = "Intervalo al 95%")
points(unlist(convergencia[6,1:N]), type = "l", col = "red")
points(rep(largoR,N), type = "l")


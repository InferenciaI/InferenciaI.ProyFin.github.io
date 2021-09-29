alfa = 0.05

#Intervalo normal
set.seed(1)
n = 25
X = rt(n,df = 3)
cuantilesN = quantile(x = X, probs = c(0.25,0.75))
largo = (cuantilesN[2] - cuantilesN[1])/1.34

#Varianza Bootstrap
B = 10000
TnB = sapply(1:B, function(b){
  Xb = sample(X,n,replace = TRUE)
  cuantb = quantile(x = Xb, probs = c(0.25,0.75))
  largoB = (cuantb[2] - cuantb[1])/1.34
  return(largoB)
})

VarB = mean((TnB - mean(TnB))^2)

#Intervalo normal
InfN = largo - qnorm(1-alfa/2)*sqrt(VarB)
SupN = largo + qnorm(1-alfa/2)*sqrt(VarB)

#Intervalo de los cuantiles Bootstrap
IntervaloB = quantile(x = TnB, probs = c(alfa/2,1-alfa/2))

#Intervalo del Bootstrap pivotal
InfBpiv = 2*largo - IntervaloB[2]
SupBpiv = 2*largo - IntervaloB[1]

#Largo real
largoR = (qt(0.75, df = 3) - qt(0.25, df = 3))/1.34



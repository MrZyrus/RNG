#Primero necesitamos la funcion error de Gauss
.erf = function(x) {
  return(2*pnorm(x*sqrt(2)) - 1)
}

#Luego la funcion acumulada, de nuevo igualada a 0, despejando para "y"
.rNormal = function(mu, sigmacua, x, y) {
  return ((1/2)*(1 + .erf((x - mu)/(sqrt(sigmacua)*sqrt(2)))) - y)
}

#Por ultimo despejamos el "x" de la funcion "n" veces usando "y" generados con runif
#Notese que el intervalo no puede ser infinito por como esta definida la funcion uniroot
#Pero podemos usar 6 sigmas, como es una distribucion normal, solo 0.00034% de los valores caen fuera de 6 sigmas.
#Para todos los propositos, imposible
rN = function(n, mu, sigmacua) {
  return(replicate(n, uniroot(.rNormal, c(mu - 6*sqrt(sigmacua),mu + 6*sqrt(sigmacua)), mu = mu, sigmacua = sigmacua, y = runif(1))$root))
}
#Primero necesitamos la funcion acumulada, igualada a 0, despejando "y"
.rSemCir = function(a, r, x, y) {
  return (1/2 + (r^2*asin((x - a)/r) + (x - a)*sqrt(r^2 - (x - a)^2))/(pi*r^2) - y)
}

#Luego simplemente despejamos el "x" de la funcion "n" veces usando "y" generados con runif
rC = function(n, a, r) {
  return(replicate(n, uniroot(.rSemCir, c(a - r,a + r), a = a, r = r, y = runif(1))$root))
}
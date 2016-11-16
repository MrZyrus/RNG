#De nuevo, necesitamos la funcion acumulada, igualada a 0, despejando "y"
.rPareto = function(alfa, m, x, y) {
  return (1 - (m/x)^alfa - y)
}

#Luego simplemente despejamos el "x" de la funcion "n" veces usando "y" generados con runif
#Uniroot no puede usar inf como limite de intervalo, pero un numero casi imposible de alcanzar para
#todos los propositos es suficiente.
rP = function(n, alfa, m) {
  return(replicate(n, uniroot(.rPareto, c(m,100^100), alfa = alfa, m = m, y = runif(1))$root))
}
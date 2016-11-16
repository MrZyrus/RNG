#Hay que popular variables si no estan declaradas, util para propositos de recurrencia
.populateVariables = function() {
  return (c(rnorm(1, 0, 1), rnorm(1, 0, 1)))
}

.recurrence = function(n, a, b, x = -1, y = -1, e = 0) {
  #Lo primero es revisar si tenemos variables ya declaradas de posiblemente una recurrencia anterior
  #Generalmente si estan declaradas y son diferentes de -1, significa que esta dentro de una recurrencia
  if (x == -1 & y == -1) {
    #Si no estan declaradas, las generamos, generalmente esto se realiza una unica ves en toda la recurrencia
    #Al principio.
    aux = .populateVariables()
    x = aux[1]
    y = aux[2]
  }
  #Los primeros dos resultados no son calculados, simplemente usamos los generados
  if (n == 1) return(x)
  if (n == 2) return(y)
  #Calculamos el error para cada recurrencia
  e = runif(1, 0, 1)
  #Entramos a la recurrencia para n mayor a 3
  return(a * .recurrence(n-1, a, b, x, y, e) + b * .recurrence(n-2, a, b, x, y, e) + e)
}

#Con esta funcion retornamos un vector con todos los valores de la serie en vez de solo el ultimo
sT = function(n, a, b) {
  #Es necesario obtener las variables desde el principio para poder hacer exactamente la misma recurrencia cada vez
  variables = .populateVariables()
  series = c()
  #Despues generamos cada valor de la recurrencia
  for (i in 1:n) {
    #Hay algo de ineficiencia aqui, tecnicamente cada valor se genera n-i+1 veces, es decir
    #el primero se genera n veces, el segundo n-1 veces, y asi hasta que el ultimo solo se genera solo 1 vez
    series = append(series, .recurrence(i, a, b, variables[1], variables[2]))
  }
  return(series)
}
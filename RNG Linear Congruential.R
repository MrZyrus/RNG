.rCongLin = function(a, c, m, x0) {
  #Hay que desactivar opciones de notacion cientifica, si no tenemos errores de calculo de digitos
  #con numeros muy grandes
  options(scipen =  999)
  #Calculamos el siguiente numero basado en el inicial
  seed = (a * x0 + c) %% m
  return(
    list(
      semilla = function(xnuevo) {
        #Mismo proceso que cuando inicializamos el generador
        seed <<- (a * xnuevo + c) %% m
      },
      proximo = function() {
        #Primero tenemos que guardar el valor actual para regresarlo al final
        toReturn = seed
        #Despues simplemente generamos el siguiente valor, para progresar el generador
        seed <<- (a * seed + c) %% m
        #Por ultimo retornando el valor normalizado de 0 a 1
        return(toReturn)
      },
      mostrar = function() {
        return(seed)
      }
    )
  )
}

rCL = function(n) {
  return(replicate(n, .rCL$proximo()))
}
.rMedCua = function(k, x0) {
  #Hay que desactivar opciones de notacion cientifica, si no tenemos errores de calculo de digitos
  #con numeros muy grandes
  options(scipen =  999)
  #Primero elevamos al cuadrado y separamos los digitos elegir solo los del medio
  newSeed = strsplit(as.character(x0 ^ 2), "")
  #Despues simplemente tomamos desde el medio los digitos en la posicion mitad - k + 1, para asegurarnos
  #de que se comporte como si estuviera relleno de 0 por la izquierda, y posicion mitad + k, con este tenemos
  #2k digitos del medio
  currentSeed = as.numeric(newSeed[[1]][(length(newSeed[[1]])/2 - k + 1) : (length(newSeed[[1]])/2 + k)])
  #Luego colapsamos los numero a un unico entero
  currentSeed = as.numeric(paste(currentSeed, collapse = ""))
  return(
    list(
      semilla = function(xnuevo) {
        #Mismo proceso que cuando inicializamos el generador
        newSeed = strsplit(as.character(xnuevo ^ 2), "")
        currentSeed = as.numeric(newSeed[[1]][(length(newSeed[[1]])/2 - k + 1) : (length(newSeed[[1]])/2 + k)])
        currentSeed <<- as.numeric(paste(currentSeed, collapse = ""))
      },
      proximo = function() {
        #Primero tenemos que guardar el valor actual para regresarlo al final
        toReturn = currentSeed
        if (toReturn == 0) { return(0) }
        #Despues simplemente generamos el siguiente valor, para progresar el generador
        newSeed = strsplit(as.character(currentSeed ^ 2), "")
        currentSeed = as.numeric(newSeed[[1]][(length(newSeed[[1]])/2 - k + 1) : (length(newSeed[[1]])/2 + k)])
        currentSeed <<- as.numeric(paste(currentSeed, collapse = ""))
        #Por ultimo retornando el valor normalizado de 0 a 1
        return(toReturn/10^(2*k))
      },
      mostrar = function() {
        return(currentSeed)
      }
    )
  )
}

rMC = function(n) {
  return(replicate(n, .rMC$proximo()))
}
# Se presentan la funciones para el calculo de anualidades vencidas
# Autor: Jesus Adriel Bravo Aguilar 
# V.1.0: 8/11/24


# Valor futuro, conociendo la anualidad, tasa de interés del periodo y el número (plazo) de anualidades.

VF <- function(A, r, Tper) {
  result <- A * ((1 + r)^Tper - 1) / r * (1 + r)
  return(result)
}

# Anualidad, conociendo valor futuro, tasa del periodo y número de pagos.

A <- function(VF, r, Tper) {
  result <- (VF * r) / ((1 + r)^Tper - 1) * (1 / (1 + r))
  return(result)
}

# Fórmula para calcular el Número de pagos o plazo, conociendo valor futuro, anualidad y tasa del periodo

Tper <- function(VF, A, r) {
  result <- log((VF * r / (A * (1 + r))) + 1) / log(1 + r)
  return(result)
}

# Fórmula para calcular la tasa del periodo, conociendo valor futuro, número de pagos y monto de la anualidad

r <- function(VF, A, Tper) {
  result <- uniroot(function(r) A * (1 + r) * ((1 + r)^Tper - 1) / r - VF, lower = 0, upper = 1)$root
  return(result)
}

# Fórmula para calcular el valor actual, conociendo la anualidad, tasa de interés del periodo y el número (plazo) de anualidades.

VA <- function(A, r, Tper) {
  result <- A * (1 - (1 + r)^(-Tper)) / r * (1 + r)
  return(result)
}

# Fórmula para calcular la anualidad, conociendo valor actual, tasa del periodo y número de pagos

A <- function(VA, r, Tper) {
  result <- VA * (r / (1 - (1 + r)^(-Tper))) * (1 + r)
  return(result)
}

# Fórmula para calcular el número de pagos o plazo, conociendo valor actual, anualidad y tasa del periodo

Tper <- function(VA, A, r) {
  result <- -log(1 - (VA * r) / (A * (1 + r))) / log(1 + r)
  return(result)
}

# Fórmula para calcular la tasa del periodo, conociendo valor actual, número de pagos y monto de la anualidad

r <- function(VA, A, Tper) {
  result <- uniroot(function(r) A * (1 + r) * (1 - (1 + r)^(-Tper)) / r - VA, lower = 0, upper = 1)$root
  return(result)
}


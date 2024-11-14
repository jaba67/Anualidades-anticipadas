# Anualidades-anticipadas
## funciones de anualidades anticipadas

En el siguiente link puede ver todas las funciones que se utilizaran a continuacion para el calculo de las anualidades vencidas:

```{r}
source("https://raw.githubusercontent.com/jaba67/Anualidades-anticipadas/refs/heads/main/anualidades%20anticipadas.R")
```

A conticuacion se dara un ejemplo de uso de cada formula que esta en el link de arriba para mostrar de manera mas grafica el uso de las mismas:

### Calculo valor futuro conociendo la anualidad anticipada, tasa de interés del periodo y el número de anualidades.

se tienen los siguientes datos: 
$A$=2000
$r$=0.05
$t$=10

```{r}
# Creamos objetos con los valores de entrada:
A <- 2000
r <- 0.05
Tper <- 10

# Calculamos el valor futuro para anualidades anticipadas
VF_anticipada <- function(A, r, Tper) {
  return(A * ((1 + r)^Tper - 1) / r * (1 + r))
}

# Imprimimos el resultado:
VF_anticipada(A, r, Tper)
```

### Calculo de anualidad (A), conociendo valor futuro, tasa del periodo y número de pagos.

se tienen los siguientes datos:
$VF$=100000
$r$=0.04
$t$=8 meses

```{r}
# Creamos objetos con los valores de entrada:
VF <- 100000
r <- 0.04
Tper <- 8

# Calculamos anualidad para anualidades anticipadas
A_anticipada <- function(VF, r, Tper) {
  result <- (VF * r) / ((1 + r)^Tper - 1) * (1 + r)
  return(result)
}

# Imprimimos el resultado:
A_anticipada(VF, r, Tper)
```

### Calculo de número de pagos o plazo (Tper), conociendo valor futuro, anualidad y tasa del periodo.

se tienen los siguientes datos:
$VF$=30000
$A$=5000
$r$=0.03

```{r}
# Creamos objetos con los valores de entrada:
VF <- 30000
A <- 5000
r <- 0.03

# Calculamos el número de pagos para anualidades anticipadas:
Tper_anticipada <- function(VF, A, r) {
  result <- log((VF * r / (A * (1 + r))) + 1) / log(1 + r)
  return(result)
}

# Imprimimos el resultado:
Tper_anticipada(VF, A, r)
```

### Calculo de tasa del periodo (r), conociendo valor futuro, número de pagos y monto de la anualidad.

se tienen los siguientes datos:
$VF$=35000
$A$=4000
$Tper$=7

```{r}
# Creamos objetos con los valores de entrada:
VF <- 35000
A <- 4000
Tper <- 7

# Calculamos la tasa del periodo para anualidades vencidas:
r <- function(VF, A, Tper) {
  result <- uniroot(function(r) A * (((1 + r)^Tper - 1) / r) - VF, lower = 0, upper = 1)$root
  return(result)
}

# Imprimimos el resultado:
r(VF, A, Tper)
```

### Calculo de valor Actual (VA), conociendo la anualidad, tasa de interés del periodo y el número de anualidades.

se tienen los siguientes datos:
$r$=0.04
$A$=3000
$Tper$=12

```{r}
# Creamos objetos con los valores de entrada:
r <- 0.04
A <- 3000
Tper <- 12

# Calculamos el valor actual para anualidades anticipadas:
VA_anticipada <- function(A, r, Tper) {
  result <- A * (1 - (1 + r)^(-Tper)) / r * (1 + r)
  return(result)
}

# Imprimimos el resultado:
VA_anticipada(A, r, Tper)
```

### Calculo de anualidad (A), conociendo valor actual, tasa del periodo y número de pagos.

se tienen los siguientes datos:
$VA$=50000
$r$=0.06
$Tper$=10

```{r}
# Creamos objetos con los valores de entrada:
VA <- 50000
r <- 0.06
Tper <- 10

# Calculamos la anualidad para anualidades anticipadas:
A_anticipada <- function(VA, r, Tper) {
  result <- VA * (r / (1 - (1 + r)^(-Tper))) * (1 + r)
  return(result)
}

# Imprimimos el resultado:
A_anticipada(VA, r, Tper)
```

### Calculo de número de pagos o plazo (Tper), conociendo valor actual, anualidad y tasa del periodo.

se tienen los siguientes datos:
$VA$=20000
$A$=2500
$r$=0.05

```{r}
# Creamos objetos con los valores de entrada:
VA <- 20000
A <- 2500
r <- 0.05

# Calculamos el número de pagos para anualidades anticipadas:
Tper_anticipada <- function(VA, A, r) {
  result <- -log(1 - (VA * r) / (A * (1 + r))) / log(1 + r)
  return(result)
}

# Imprimimos el resultado:
Tper_anticipada(VA, A, r)
```

### Calculo de tasa del periodo (r), conociendo valor actual, número de pagos y monto de la anualidad.

se tienen los siguientes datos:
$VA$=30000
$A$=3000
$Tper$=15

```{r}
# Creamos objetos con los valores de entrada:
VA <- 30000
A <- 3000
Tper <- 15

# Calculamos la tasa del periodo para anualidades anticipadas:
r_anticipada <- function(VA, A, Tper) {
  result <- uniroot(function(r) A * (1 - (1 + r)^(-Tper)) / r * (1 + r) - VA, lower = 0, upper = 1)$root
  return(result)
}

# Imprimimos el resultado:
r_anticipada(VA, A, Tper)
```

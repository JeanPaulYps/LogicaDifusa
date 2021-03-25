library("readxl")

#Creacion de una funcion de hombro izquierdo
leftShoulderFunction <- function(a1,a2)
{
  leftShoulder <- function(x)
  {
    if(x <=a1)
    {
      return(1)
    }
    else if (x>=a1 & x<=a2)
    {
      return((a2-x)/(a2-a1))
    }
    else
    {
      return(0)
    }
  }
  return(leftShoulder)
}

#Creacion de una funcion de hombro derecho
rightShoulderFunction <- function(a1,a2)
{
  rightShoulder <- function(x)
  {
    if(x <=a1)
    {
      return(0)
    }
    else if (x>=a1 & x<=a2)
    {
      return((x-a1)/(a2-a1))
    }
    else
    {
      return(1)
    }
  }
  return(rightShoulder)
}

#Creacion de una funcion trapezoidal
trapezoidalFunction <- function(a1,a2,a3,a4)
{
  trapezoidal <- function(x)
  {
    if(x >= a1 & x <= a2)
    {
      return((x-a1)/(a2-a1))
    }
    else if (x>= a2 & x <= a3)
    {
      return(1)
    }
    else if(x >= a3 & x <= a4 )
    {
      return((a4-x)/(a4-a3) )
    }
    else
    {
      return(0)
    }
  }
  return(trapezoidal)
}

# Leer datos de excel
datos <- read_excel("indicadores\ 2020.xlsx")


# Leer las columnas de expectativa de vida saludable y corrupcion
expectativa <-  datos$`Healthy life expectancy`
percepcionCorrupcion <- datos$`Perceptions of corruption`

#Sacar los minimos y los maximos de la expectativa de vida saludable 
expectativaMinima <- min(expectativa)
expectativaMaxima <- max(expectativa)

#Sacar los minimos y los maximos de la percepción de corrupcion
percepcionMinima <- min(percepcionCorrupcion)
percepcionMaxima <- max(percepcionCorrupcion)


crearFuncionTrapezoidal <- function(minimo, maximo)
{
  # Creacion de la funcion trapezoidal
  
  # Se crean los limites, se crean 6 intervalos iguales
  # Los grados de pertenencia crecen del primer al tercer intervalo
  # Pertenecen igual del tercer al cuarto intervalo
  # Los grados de pertencia decrecen del cuarto al ultimo intervalo
  limites <-  seq(minimo,maximo,length.out = 6)
  trapezoidal <- trapezoidalFunction(limites[1], limites[3], limites[4], limites[6])
  return(trapezoidal)
}

crearFuncionHombroIzquierdo <- function(minimo, maximo)
{
  #Crear hombro izquierdo desde que decrece 
  #del minimo hasta la mitad del intervalo
  puntoMedio <- (minimo + maximo)/2
  hombroIzquierdo <- leftShoulderFunction(minimo, puntoMedio)
  return(hombroIzquierdo)
}

crearFuncionHombroDerecho <- function(minimo, maximo)
{
  #Crear hombro derecho desde que crece 
  #mitad hasta el final del intervalo
  puntoMedio <- (minimo + maximo)/2
  hombroDerecho <- rightShoulderFunction(puntoMedio, maximo)
  return(hombroDerecho)
}


# Funcion para crear los conjuntos difusos 
crearConjuntoDifusoDeTresVariables <- function(minimo, maximo)
{
  trapezoidal <- crearFuncionTrapezoidal(minimo, maximo)
  hombroIzquierdo <- crearFuncionHombroIzquierdo(minimo, maximo)
  hombroDerecho <- crearFuncionHombroDerecho(minimo, maximo)
  
  #Graficar los datos 
  muestra = seq(minimo, maximo, length.out =1000)
  
  # Graficacion de los conjuntos
  plot(sapply(muestra, hombroIzquierdo) ~ muestra,  main="Grados de pertenencia en el universo",
       xlab="Universo",ylab="Grados de pertenencia", 
       type="l")
  lines(sapply(muestra, trapezoidal) ~ muestra, col="blue")
  lines(sapply(muestra, hombroDerecho) ~ muestra , col="red")
}

# Graficar conjunto difuso expectativa de vida saludable
crearConjuntoDifusoDeTresVariables(expectativaMinima, expectativaMaxima)
# Graficar conjutno difuso percepcion corrupcion
crearConjuntoDifusoDeTresVariables(percepcionMinima, percepcionMaxima)



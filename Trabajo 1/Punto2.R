library("readxl")
library(stringr)

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

#Sacar los minimos y los maximos de la percepci?n de corrupcion
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
  trapezoidal <- trapezoidalFunction(limites[1], limites[3], limites[4],limites[6])
  return(trapezoidal)
}

crearFuncionHombroIzquierdo <- function(minimo, maximo)
{
  #Crear hombro izquierdo desde que decrece 
  #del minimo hasta la mitad del intervalo
  limites <-  seq(minimo,maximo,length.out = 6)
  hombroIzquierdo <- leftShoulderFunction(limites[1], limites[3])
  return(hombroIzquierdo)
}

crearFuncionHombroDerecho <- function(minimo, maximo)
{
  #Crear hombro derecho desde que crece 
  #mitad hasta el final del intervalo
  limites <-  seq(minimo,maximo,length.out = 6)
  hombroDerecho <- rightShoulderFunction(limites[4], limites[6])
  return(hombroDerecho)
}


# Funcion para crear los conjuntos difusos 
crearConjuntoDifusoDeTresVariables <- function(minimo, maximo, variable)
{
  trapezoidal <- crearFuncionTrapezoidal(minimo, maximo)
  hombroIzquierdo <- crearFuncionHombroIzquierdo(minimo, maximo)
  hombroDerecho <- crearFuncionHombroDerecho(minimo, maximo)
  
  #Graficar los datos 
  muestra = seq(minimo, maximo, length.out =1000)
  # Graficacion de los conjuntos
  plot(sapply(muestra, hombroIzquierdo) ~ muestra,  main=paste("Grados de pertenencia al universo",variable),
       xlab=str_to_sentence(variable) ,ylab="Grados de pertenencia", 
       type="l", yaxt="none")
  axis(2,seq(0,1,length.out=11), las=2)
  lines(sapply(muestra, trapezoidal) ~ muestra, col="blue")
  lines(sapply(muestra, hombroDerecho) ~ muestra , col="red")
  text(muestra[100], 1, "Poco")
  text(muestra[500], .9, "Normal", col="blue")
  text(muestra[900], 1, "Mucho", col="red" )
  abline(h=0.5, lty=2)
}

# Graficar conjunto difuso expectativa de vida saludable
crearConjuntoDifusoDeTresVariables(expectativaMinima, expectativaMaxima, "expectativa de vida saludable")
  # Graficar conjutno difuso percepcion corrupcion
crearConjuntoDifusoDeTresVariables(percepcionMinima, percepcionMaxima, "percepcion de corrupcion")



trapezoidal <- crearFuncionTrapezoidal(expectativaMinima, expectativaMaxima)
hombroIzquierdo <- crearFuncionHombroIzquierdo(expectativaMinima, expectativaMaxima)

puntoMedio <- expectativaMinima + (expectativaMaxima - expectativaMinima)*.2
puntoMedio
trapezoidal(puntoMedio)
hombroIzquierdo(puntoMedio)







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
}
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
}


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
    
   # ifelse(a1 <= x & x <= a2,
  #         (x-a1)/(a2-a1),
    #  ifelse(a3 >= x & x <= a4),
   #       (a4-x)/(a4-a3),
  #    0)
  }
  return(trapezoidal)
}

a1= 0
a2= 40
a3 =60
a4= 100

trapezoidal <- trapezoidalFunction(a1,a2,a3,a4)

trapezoidal(30)

trapezoidal(80)



muestra = seq(-10,110, length.out =1000)
muestra

res=sapply(muestra, trapezoidal)
plot(res~muestra, type='l')

trapezoidal2 <- trapezoidalFunction(0,0,0,50)

plot(sapply(muestra, trapezoidal2)~muestra, type='l')


trapezoidal3 <-  trapezoidalFunction(50,100,100,100)


hombroIzquierdo <- leftShoulderFunction(0,50)
hombroDerecho <- rightShoulderFunction(50,100)

plot(sapply(muestra, hombroIzquierdo)~muestra, type='l')
lines(sapply(muestra, hombroDerecho)~muestra)
lines(res~muestra, type='l')





#Rogelio Daniel Gonz�lez Gonz�lez
#Colegio de M�xico
#Taller de Econometr�a
#02/03/2018

#Operadores relaciones

7==4 Falso

7==7 Verdadero

7!==7

#Condicionales

a=1
b=1

#Verficando si n�meros son iguales
if(a==B)()
  print("Los n�meros son iguales")
) else(
  print("Los dos n�meros")
)

#Funciones

#Llamando Funciones

help("function") #Ayuda a encontrar cosas en la documentaci�n de R


#Cuadrado
#Input: n�mero real
#Output: el cuadrdo del n�mero real

cuadrado= function(x)(
  y=x^2
  return(x)
)

cuadrado(5)


#Raiz
#Input: N�mero real
#Output: si x espositivo regresa la raiz del n�mero, si no lo es entonces regresa un mensaje

raiz=function(x)(
  if(x<0)(
   print("el n�mero es negativo y me niego a regresarte numeros imaginarios") 
  ) else(
    return(sqrt(x))
  )
)
raiz(25)
raiz(-9)

#Chicarronera para polinomios de la forma ax^2+bx+c
#Input: tres n�meros reales
#Output: si se cumple que b^2>=4ac entonces se obtienen las raices del polinomio, sino, entonces

chicha= function(a,b,c)(
  cond= b^2>=4*a*c
  if(cond)(
    z1= (-b+sqrt(b^2+4*a*c))/2*a
    z2=
  )
)

#Loops
##While

x=1
while(x<5){
  x=x+1
  print(x)
}

##Next hace que se brinque un paso el loop
##Break termima el loop 

x=1
while(x<8){
  x=x+1
  if (x=4) next
  print(x)
}

##for

for(i in 1:30){
  print(paste("Este es el loop",i))
  print(i*i)
}


#---------
#Algoritmos en pr�ctica

#Algoritmo de Euclides para el�ximo com�n divisor
#Input: dos n�meros enteros reales a y b
# Output: el m�ximo com�n divisor de los dos n�meros

mcd=function(a, b){
  A=a
  B=b
  if(A!=0){
    while(B!=0){
      if(A>B){
        A=A-B
      } else {
        B=B-A
      }
    }
    return(A)
  }else{
    return(B)
  }
}

mcd(10,7)
mcd(0,5)

#Algoritmo para calcular MCO
#INPUT: Matriz de x variables explicativas, matriz y de variables dependientes
#Output: vector de b que minimiza la suma de residuos al cuadrado en y=xb-u

MCO=function(x,y){
  X=x
  Y=y
  xr=nrow(x)
  yr=nrow(y)
  if(xr==yr){
    b=solve(t(X)%*%X)%*%t(x)%*%Y
    return(b)
  } else{
    print{}
  }
}



##Muestras aleatorias normales
library(MASS)
n=1 #Tama�o de la muestra
mu=0 #media de U
su= 0.2 #varianza de U

mx= c(4,2,0,0,2) #vector de medias de X
sx= matrix(c(4,-1,0,0,0,
             -1,1,0,0,0,
             0,0,1,0,0
             0,0,0,9,2
             0,0,0,2,4),5) #Varianza de X

Usample= rnorm(n, mu,su) #Muestra aleatoria de U
Xsample= mvnorm(n,mx,sx) #Muestra aleatoria de X

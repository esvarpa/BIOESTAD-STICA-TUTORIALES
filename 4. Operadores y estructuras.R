#Expresiones condicionales en R
n<- -1
if(n>0){
  print("Positivo")
} else {
  if (n<0) {
    print("Negativo")
  } else {
    print("Cero")
  }
}

#Ejemplo IMC pesos distintos 56, 70, 77 y 92
peso<- 74 #Kg
estarura<-1.75 #metros
IMC<- peso/estarura^2

diagnostico<-if (IMC<18.5){
  print("Peso inferior a lo normal")
} else {
  if (IMC<=24.9){
    print("Peso normal")
  } else {
    if (IMC<=29.9){
      print("Peso superior a lo normal")
    } else {
      print("Obesidad")
    }
  }
}

#Decisión de una función
switch(diagnostico, "Peso inferior a lo normal"="Mejorar Dieta",
       "Peso normal"="Correcto", "Peso superior a lo normal"="Vigilar Dieta",
       "Obesidad"="Requiere Tratamiento Médico")



#Bucle for 
a<-5
b<-c()
for(i in 1:a){
  a<-i*3;
  b<-c(b,a)
}
print(b)


n<-48
while(n<=76){
  print(n)  
  n<-n+1
}

n<-48
m<-c()
while(n<=76){
  print(n);
  n<-n+1;
  m<-c(m,n-1)
}
m

#Extraer pares e impares de m
pares<-m%%2==0
m[pares]

impares<-m%%2!=0
m[impares]

(z<-m[m>60])

(y<-m[m<54])


####función

funcion<-function(x,y){
  z= sqrt(x^2+y^2) 
  return (z)
}

a<-4
b<-6
funcion(a,b)

library(MASS)
data("anorexia")
prewt<-c(anorexia$Prewt)
postwt<-c(anorexia$Postwt)
result<-c(postwt-prewt)
dif_peso<-0
for(i in 1:length(result)){
  if (result[i] < 0)
    dif_peso<-dif_peso+1
}
print (dif_peso)

peso<-c(56,70,77,92)
estatura<-c(1.75,1.75,1.75,1.75)
IMC<-peso/estatura^2

composicion<-c()
j<-1
final<-function(){
  for(i in 1:length(IMC))
    if(IMC[i]<18.5){
      composicion[j]<-"Peso inferior a lo normal"
      j<-j+1
    }
  else
    if(IMC[i]<=24.9){
      composicion[j]<-"Peso Normal"
      j<-j+1
    }
  else
    if(IMC[i]<=29.9){
      composicion[j]<-"Peso superior al normal"
      j<-j+1
    }
  else{
    composicion[j]<-"Obesidad"
    j<-j+1
  }
  return(composicion)
}
composicion<-final()
composicion
composicion<-as.vector(composicion)
df<-data.frame(peso, estarura, composicion)
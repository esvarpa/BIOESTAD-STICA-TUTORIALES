moneda<-c("cara","sello")

sample(moneda,1)

set.seed(1995)
#set.seed(NULL)
replicas<-replicate(1000, expr = sample(moneda,1))
prop.table(table(replicas))


resultados <- c()

for (i in 1:100) {
  muestra <- replicate(n = i, expr = sample(moneda,1))
  resultados[i] = prop.table(table(muestra))
}

resultados

plot(resultados, type = "l")
abline(h = 0.5, col = "red")

library(tidyverse)
resultados %>% 
  enframe(name = "lanzamiento", value = "proporcion") %>% 
  ggplot(aes(x = lanzamiento, y = proporcion)) +
  geom_line() +
  geom_hline(yintercept = 0.5, color = "red")

resultados %>% 
  enframe(name = "lanzamiento", value = "proporcion") %>% 
  ggplot(aes(x = proporcion)) +
  geom_density() +
  geom_vline(xintercept = 0.5, color = "red")+
  xlim(0,1)



dado<-c(1,2,3,4,5,6)
sample(dado, 1)
replicasd<-replicate(100000, expr = sample(dado,1))
round(prop.table(table(replicasd)),3)


resultados <- c()

for (i in 1:1000) {
  muestra <- replicate(n = i, expr = sample(dado,1))
  resultados[i] = prop.table(table(muestra))
}

resultados

resultados %>% 
  enframe(name = "lanzamiento", value = "proporcion") %>% 
  ggplot(aes(x = lanzamiento, y = proporcion)) +
  geom_line() +
  geom_hline(yintercept = 1/6, color = "red")

resultados %>% 
  enframe(name = "lanzamiento", value = "proporcion") %>% 
  ggplot(aes(x = proporcion)) +
  geom_density() +
  geom_vline(xintercept = 1/6, color = "red")+
  xlim(0,1)




U<-c(1/3,1/3,1/3)
B<-c(3/5,4/6,0)

PBU<-c()
j<-1
#Theorema de Bayes
funcion<-function(U,B){
  for(i in 1:length(B)){
    P <- U[i]*B[i]/sum(U*B)
    PBU[j]<-P
    j<-j+1
  }
  return(PBU)
}
funcion(U,B)

library(MASS)
fractions(funcion(U,B))

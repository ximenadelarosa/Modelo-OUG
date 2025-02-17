# Parámetros
T <- 10  # Tiempo final
n<-1000

#Llamamos a la función del movimiento browniano
brown<-function(w0,n,T, sem=123){
  delta<-T/n #Delta de la partición
  W<-c(w0) #Valor inicial
  set.seed(sem)
  for (i in 1:n) {
    x<-rnorm(1,mean=0, sd=1) #num aleatorio normal
    w<-W[i]+x*sqrt(delta) 
    W<-c(W,w)
  }
  return(W)
}

#Generamos la función que simula el proceso cir
OUG<-function(x0,k,theta,sigma,n,T,W){
  delta<-T/n
  X <- c(x0)
  for (i in 1:n) {
    x<- X[i]+k*(theta-log(X[i]))*X[i]*delta + sigma*X[i]*(W[i+1]-W[i])
    X<-c(X,x)
  }
  return(X)
  
}
W<-brown(w0=0,n,T)
#Creamos dos trayectorias cir
tiempo<-seq(from=0, to=T,by=T/n)
p1<-OUG(x0=5,k=0.2,theta=0.01,sigma =0.5,n,T,W)
p2<-OUG(x0=5,k=0.2,theta=0.01,sigma =0.5,n,T,W)

#
difusion <- function(OUG1,OUG2,T) {
  #partición del tiempo
  time <- seq(0, T, by = T/n) 
  #creamos los vectores Xt1 y Xt2
  Xt1 <- numeric(length(time))
  Xt2 <- numeric(length(time))
  #tau es el tiempo en el que se cruzan y lo inicializamos con NULL
  tau <- NULL
  Xt1<-OUG1
  Xt2<-rev(OUG2)
  #Condición del cruce
  for (i in 1:length(time)) {
    if (((Xt1[i]<= Xt2[i])&(Xt1[i+1]>= Xt2[i+1]))|((Xt2[i]<= Xt1[i])&(Xt2[i+1]>= Xt1[i+1]))) {
      tau <- time[i]
      break
    }
  }
  if (is.null(tau)) {
    tau <- T  # Si no se encuentra tau, entonces se fija en T
  }
  Zt <- ifelse(time <= tau, Xt1, Xt2)
  return(list(time = time, Zt = Zt, tau = tau))
}

# Simulación del puente de difusión
puente <- difusion(p1,p2,T)

# Graficamos 
plot(puente$time, puente$Zt, type = "l", col = "green", ylim = range(puente$Zt), ylab = "Valor", xlab = "Tiempo", main = "Simulación de Puente de Difusión")
abline(v = puente$tau, col = "red", lty = 2)
legend("topright", legend = c("k=0.2","theta=0.01", "sigma=0.5" ), lty = 1)

plot(puente$time,p1, type='l',ylab = "Valor", xlab = "Tiempo",ylim = range(p1, p2))
lines(puente$time,rev(p2), col='blue')
abline(v = puente$tau, col = "red", lty = 2)


plot(head(puente$time,n=-587),head(p1,n=-587),type='l', xlab = "Tiempo", ylab = "Valor",ylim = range(p1, p2),xlim = range(puente$time))
lines(tail(puente$time,587),tail(rev(p2),587), col='blue')
abline(v = puente$tau, col = "red", lty = 2)

print(puente$time[415])

# Parámetros
T <- 50              # Tiempo final
n <- 1000

#Generador trayectoria Browniano
brown<-function(w0,n,T){
  delta<-T/n #Delta de la partición
  W<-c(w0) #Valor inicial
  for (i in 1:n) {
    x<-rnorm(1,mean=0, sd=1) #num aleatorio normal
    w<-W[i]+x*sqrt(delta) 
    W<-c(W,w)
  }
  return(W)
}
W<-brown(w0=0,n,T)

#Generamos la función que simula el proceso oug
OUGM<-function(x0,k,theta,sigma,n,T,W){
  delta<-T/n
  X <- c(x0)
  for (i in 1:n) {
    x<-X[i]+k*(theta-log(X[i]))*X[i]*delta + sigma*X[i]*(W[i+1]-W[i])+1/2*sigma^2*X[i]*((W[i+1]-W[i])^2-delta)
    X<-c(X,x)
  }
  return(X)
}

tiempo<-seq(from=0, to=T,by=T/n)
p<-OUGM(x0=4,k=3,theta=0.5,sigma=0.2,n,T,W)
# Graficamos Xt
plot(tiempo, p, type = "l", col = "black",ylim = range(p), xlab="t", ylab = "Xt", main= "Simulacion mediante metodo de Milstein")
legend("topright", legend = c("k=3","theta=0.04","sigma=0.8", "x0=4"), lty = 1)
abline(h =1.64, col = "red", lty = 2)

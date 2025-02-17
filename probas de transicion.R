library(ggplot2)
n=500
Vasicek<-function(x0,k,theta,sigma,n,T){
  tiempo<-seq(from=0, to=T,by=T/n) #Partición del tiempo
  V<-c(x0)
  for (t in 1:n) {
    te<-theta-sigma^2/(2*k)
    eta<-te+(log(V[t])-te)*exp(-k*tiempo[t])
    nu<-sigma^2/(2*k)*(1-exp(-2*k*tiempo[t])) 
    x<-rlnorm(1,eta,sqrt(nu))
    V<-c(V,x)
  }
  df<-as.data.frame(cbind(tiempo,V))
  names(df)<-c('tiempo','V')
  return(df)
}

d3<-Vasicek(x0=7,k=0.2,theta=1.2,sigma =0.1,n,T)

ggplot(d3,aes(x=tiempo, y=V))+
  geom_line(color='green')+
  labs(y='X(t)', title = "Proceso OUG con Probas de transición")+
  geom_hline(yintercept = 3.24, linetype = "dashed", color = "blue")

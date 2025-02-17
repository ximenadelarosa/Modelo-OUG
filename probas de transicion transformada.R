library(ggplot2)
n=500
ougt<-function(x0,k,theta,sigma,n,T){
  
  tiempo<-seq(from=0, to=T,by=T/n) #Partición del tiempo
  V<-c(x0)
  for (t in 1:n) {
    te<-theta-sigma^2/(2*k)
    eta<-te+(V[t]-te)*exp(-k*tiempo[t])
    nu<-sigma^2/(2*k)*(1-exp(-2*k*tiempo[t])) 
    x<-rnorm(1,eta,sqrt(nu))
    V<-c(V,x)
  }
  df<-as.data.frame(cbind(tiempo,V))
  names(df)<-c('tiempo','V')
  return(df)
}

d3<-ougt(x0=4,k=0.2,theta=0.5,sigma =0.3,n,T)

ggplot(d3,aes(x=tiempo, y=V))+
  geom_line(color='green')+
  labs(y='X(t)', title = "Proceso Ornstein-Uhlenbeck con Probas de transición")+
  geom_hline(yintercept = 0.275, linetype = "dashed", color = "blue")


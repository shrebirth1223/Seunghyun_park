bea <- beaver2
newbea<-data.frame(time=seq(1,25,0.5)) 

newbea$pred<-predict(ye.model,newbea) 
ye.model<-lm(bea$day~time, data=bea) 
bea$pred<-predict(ye.model,bea) 

plot(bea$time,bea$pred,col="blue",pch=16) 
lines(bea$time, fitted(ye.model), col="black") 

plot(newbea$time,newbea$pred, col="green")


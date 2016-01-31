# ======= Create new forcasting data  
# firm : firm code ;  Cdata : All dataspace, incould firm's code, periods(more than 7) and variables.  

library(forecast)
forcastVar=function(firm,Cdata,period){
 firm=firm[firm$V2>7,]
 newvars={}
 for(m in 1:nrow(firm)){
  newvars0=matrix(numeric(period*ncol(Cdata)),ncol=ncol(Cdata))
  tmp=as.matrix(Cdata[Cdata$firm==firm$V1[m],])
  tmp0=rbind(tmp,newvars0)
    for(j in 1:period){
      for(i in 5:(ncol(tmp0))){
	   p=tmp0[2:(nrow(tmp)+j-1),i]-tmp0[1:(nrow(tmp)+j-2),i] # test TS is constant or not.
	   if(round(sum(p)*1e+8)==0){tmp0[(nrow(tmp)+j),i]=mean(tmp0[2:(nrow(tmp)+j-1),i])}
       if(sum(p)!=0){ 
	   vari0=auto.arima(as.numeric(tmp0[1:(nrow(tmp)+j-1),i]))
       vari=length(vari0$model$phi)+length(vari0$model$theta)+length(vari0$model$Delta)
          if(vari!=0){tmp0[(nrow(tmp)+j),i]=forecast(auto.arima(as.numeric(tmp0[1:(nrow(tmp)+j-1),i])),h=1)$mean}
          if(vari==0){tmp0[(nrow(tmp)+j),i]=forecast(auto.arima(as.numeric(tmp0[1:(nrow(tmp)+j-1),i]),d=1),h=1)$mean}
          }
	   } 
    tmp0[(nrow(tmp)+1):(nrow(tmp)+period),1]=rep(tmp[1,1],period)
    tmp0[(nrow(tmp)+1):(nrow(tmp)+period),c(2,4)]=as.matrix(TimeMapping0[TimeMapping0$mapping > tmp[nrow(tmp),4] & 
                                                             TimeMapping0$mapping < tmp[nrow(tmp),4]+period+1,])
   }  
newvars=rbind(newvars,tmp0[(nrow(tmp)+1):(nrow(tmp)+period),])
}
forcastVar=newvars
}
# Ex:forcastVar(firm,Cdata,period=8))

#  To combine the data files (s1 & s2) as one. (yyyymm  vars.)

CombidD <- function(s1,s2){
   CombidD1=cbind(s1,matrix(rep(NA,nrow(s1)*(ncol(s2)-1)),ncol=(ncol(s2)-1)))
   n=ncol(s1)+1 ;
for(i in 1:nrow(CombidD1)){
   tmp=as.matrix(s2[s2[,1]==CombidD1[i,1],2:ncol(s2)])
     if(nrow(tmp)==1){
       CombidD1[i,n:(n+ncol(tmp)-1)]=as.matrix(tmp)
      }
  }
 colnames(CombidD1)=c(colnames(s1),colnames(s2)[2:ncol(s2)])
 return(data.frame(CombidD1))
}
# Ex: CombidD0(allfirmTQ,defQ,chara=0)

# s1 : yyyymm vars. ; s2 : yyyymm vars.
CombidD0 <- function(s1,s2,chara){
CombidD1=cbind(s1,matrix(rep(chara,nrow(s1)*(ncol(s2)-1)),ncol=(ncol(s2)-1)))
n=ncol(s1)+1 ;
for(i in 1:nrow(CombidD1)){
tmp=as.matrix(s2[s2[,1]==CombidD1[i,1],2:ncol(s2)])
  if(nrow(tmp)==1){
    CombidD1[i,n:(n+ncol(tmp)-1)]=as.matrix(tmp)
   }
 }
 colnames(CombidD1)=c(colnames(s1),colnames(s2)[2:ncol(s2)])
 return(data.frame(CombidD1))
}
# Ex: CombidD0(allfirmTQ,defQ,chara=0)

# s1 : firm_code yyyymm vars. ; s2 : firm_code yyyymm vars.
CombidD1 <- function(s1,s2,chara){
CombidD1=cbind(s1,matrix(rep(chara,nrow(s1)*(ncol(s2)-2)),ncol=(ncol(s2)-2)))
n=ncol(s1)+1 ;
for(i in 1:nrow(CombidD1)){
tmp=as.matrix(s2[s2[,1]==CombidD1[i,1] & s2[,2]==CombidD1[i,2],3:ncol(s2)])
  if(nrow(tmp)==1){
    CombidD1[i,n:(n+ncol(tmp)-1)]=as.matrix(tmp)
   }
 }
 colnames(CombidD1)=c(colnames(s1),colnames(s2)[3:ncol(s2)])
 return(data.frame(CombidD1))
}
#Ex: CombidD1(alldataspace0[,1:2],Devent,chara=0)


# Find the information dstribution of news. 
# Newsdata : data space with three part. The first is date (yyyymmdd). The second is keyword. 
#            The three is times of 'Good','Bad' and 'Neutral'.
# word : The word of want to calculate the prob. which must contain Big5 column "matrix".
# Ex. for marc word : Inf=Information(newsdata=news_tmpQ1,word=matrix(Marc_keyword,ncol=1)) 
# Ex. for firm word : firmname=cbind(firm1_keyword,firm2_keyword)
#                     Inf=Information(newsdata=news_tmpQ1,word=firmname)

Information = function(newsdata,word){
firm_prob={} ;prob_tmp={};
if(ncol(word)>1){
   word=cbind(1:nrow(word),word)
   colnames(word)=c('map','firm','Big5')
   word=data.frame(word)
 for(i in 1:nrow(word)){
  if (sum(colnames(newsdata)==word$Big5[i])!=0){
      tmp=cbind(c(1:nrow(newsdata)),newsdata[,colnames(newsdata)==word$Big5[i]])
	  tmp0=tmp[tmp[,2]!=0,1] ; 
	  if(length(tmp0)!=0){
	  tmp1=sum(newsdata$GNews[tmp0]) ;  tmp2=sum(newsdata$BNews[tmp0]) ; tmp3=sum(newsdata$NNews[tmp0]) ; tmp4=length(tmp0)
	 #tmp1=sum(newsdata$GNews[tmp0])/length(tmp0)  ;  tmp2=sum(newsdata$Bews[tmp0])/length(tmp0) ; tmp3=sum(newsdata$Uews[tmp0])/length(tmp0)
	 #tmp1=sum(newsdata$GNews[tmp0])/length(tmp0)  ;  tmp2=sum(newsdata$BNews[tmp0])/length(tmp0) ; tmp3=sum(newsdata$UNews[tmp0])/length(tmp0)
	 prob_tmp=cbind(word$firm[i],tmp1,tmp2,tmp3,tmp4)
	  } else {
          prob_tmp=cbind(word$firm[i],0,0,0,0)
		  }
     } else {
      prob_tmp=cbind(word$firm[i],0,0,0,0)
   }	 
   firm_prob=rbind(firm_prob,prob_tmp)
}
firm_prob=data.frame(cbind(firm_prob[,1],round(newsdata$yyyymmdd[nrow(newsdata)]/100),firm_prob[,2:ncol(firm_prob)]))
colnames(firm_prob)=c('firm','yyyymm','Good_News','Bad_News','Neutral_News','All_News')
} else {
   word=cbind(1:nrow(word),word)
   colnames(word)=c('map','Big5')
   word=data.frame(word)
 for(i in 1:nrow(word)){
  if (sum(colnames(newsdata)==word$Big5[i])!=0){
      tmp=cbind(c(1:nrow(newsdata)),newsdata[,colnames(newsdata)==word$Big5[i]])
	  tmp0=tmp[tmp[,2]!=0,1] ; 
        if(length(tmp0)!=0){ 	  
	      tmp1=sum(newsdata$GNews[tmp0])   ;  tmp2=sum(newsdata$BNews[tmp0])  ; tmp3=sum(newsdata$NNews[tmp0]) ; tmp4=length(tmp0)
	      prob_tmp=c(sprintf('%s',word$Big5[i]),c(tmp1,tmp2,tmp3,tmp4))
	     } else {
          prob_tmp=c(sprintf('%s',word$Big5[i]),0,0,0,0)
		  }
     } else {
      prob_tmp=c(sprintf('%s',word$Big5[i]),0,0,0,0)
   }	 
   firm_prob=rbind(firm_prob,prob_tmp)
}
yyyymm=round(newsdata$yyyymmdd[nrow(newsdata)]/100)
firm_prob=data.frame(cbind(matrix(firm_prob[,1],ncol=1),yyyymm,matrix(as.numeric(firm_prob[,c(2:ncol(firm_prob))]),ncol=(ncol(firm_prob)-1))))
colnames(firm_prob)=c('Big5','yyyymm','Good_News','Bad_News','Neutral_News','All_News');
firm_prob$Good_News=as.numeric(matrix(firm_prob$Good_News,ncol=1)) 
firm_prob$Bad_News=as.numeric(matrix(firm_prob$Bad_News,ncol=1)) 
firm_prob$Neutral_News=as.numeric(matrix(firm_prob$Neutral_News,ncol=1)) 
}
return(firm_prob)
}

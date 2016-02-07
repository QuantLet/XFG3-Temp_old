# Plot the ROC curve
roc_curve=function(modelfit,code,main,windows){
if(windows==1){windows()}
a=round(performance(prediction(modelfit, code),'auc')@ y.values[[1]],3)
plot(performance(prediction(modelfit, code),"tpr","fpr"),ylab='sensitivity:true positive rate',
     xlab='(1-specificity):false positive rate',main=main,col=4) 
title(sub=paste("Area under ROC:",round(a,5)),cex.sub = 1.2, font.sub = 3, col.sub = "red")
}

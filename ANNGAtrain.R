require(RSNNS)
require(ANN)
mlpTrainingInputs<-matrix(nrow=noTrainingInputs,ncol=noInputUnits)
mlpTrainingTargets<-matrix(nrow=noTrainingInputs,ncol=noOutputUnits)
for(i in 1:length(trainingInputs))
{
  mlpTrainingInputs[i,]<-trainingInputs[[i]];
  mlpTrainingInputs[which(mlpTrainingInputs==-1)]=0;
  mlpTrainingTargets[i,]<-targets[[i]];
  mlpTrainingTargets[which(mlpTrainingTargets==-1)]=0;
}
res<-ANNGA(x=mlpTrainingInputs,y=mlpTrainingTargets,design=c(noInputUnits, noOutputUnits , noOutputUnits),population=100,mutation = 0.2,
                       crossover = 0.6,
                       minW	=-10,
                       maxW	=10,
                       maxGen  =100,
                       error   =0.001)

test<-matrix(inputPredict,nrow=1)
test[which(test==-1)]<-0
predictedoutput<-predict.ANN(res,test)
predictedoutput<-predictedoutput$predict;
predictedoutput<-round(predictedoutput)
index<-which(predictedoutput==1);
writeLines(targetChars[index]);
resultWindow<-gwindow("Predicted Output");
label<-glabel(targetChars[index],container=resultWindow)
font(label)<- list(family="times",size=120,weight="bold",style="bold")
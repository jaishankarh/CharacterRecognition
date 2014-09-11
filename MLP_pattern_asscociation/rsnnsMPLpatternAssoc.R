##very similar to the other mlp network of the other classification file..but in this file pattern associationn is done and not pattern classification

require(RSNNS)
mlpTrainingInputs<-matrix(nrow=noTrainingInputs,ncol=noInputUnits)
mlpTrainingTargets<-matrix(nrow=noTrainingInputs,ncol=noAssocOutputUnits)
for(i in 1:length(trainingInputs))
{
  mlpTrainingInputs[i,]<-trainingInputs[[i]];
  mlpTrainingInputs[which(mlpTrainingInputs==-1)]=0;
}
for(i in 1:noAssocTrainingTargets)
{
  mlpTrainingTargets[i,]<-assocTargets[[i]];
  mlpTrainingTargets[which(mlpTrainingTargets==-1)]=0;
}
for(i in 1:noAssocTrainingTargets)
{
  mlpTrainingTargets[(i+noAssocTrainingTargets),]<-assocTargets[[i]];
  mlpTrainingTargets[which(mlpTrainingTargets==-1)]=0;
}
ml<-splitForTrainingAndTest(mlpTrainingInputs, mlpTrainingTargets, ratio = 0.15)

# model<-mlp(mlpTrainingInputs,mlpTrainingTargets,size=c(noOutputUnits),
#            hiddenActFunc = "Act_Logistic", learnFuncParams = c(0.4, 0.0),
#            learnFunc = "Std_Backpropagation")
model<-mlp(ml$inputsTrain,ml$targetsTrain,size=noAssocOutputUnits,
           learnFuncParams = 0.1, maxit = 3000, inputsTest = ml$inputsTest,targetsTest = ml$targetsTest)

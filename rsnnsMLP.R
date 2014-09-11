#this package is required for the mlp network to function
require(RSNNS)

#the training inputs and targets need to be in matrix form..whereas the inputs and targets scanned from the file are put into a list so we need to convert them...because the mlp expects it that way...

mlpTrainingInputs<-matrix(nrow=noTrainingInputs,ncol=noInputUnits)
mlpTrainingTargets<-matrix(nrow=noTrainingInputs,ncol=noOutputUnits)
#the matrix is modeled in such a way that the columns present the different inputs to a single training input
#where as the rows indicate the different training inputs presented for training..
for(i in 1:length(trainingInputs))
{
  mlpTrainingInputs[i,]<-trainingInputs[[i]];
  mlpTrainingInputs[which(mlpTrainingInputs==-1)]=0;
  mlpTrainingTargets[i,]<-targets[[i]];
  mlpTrainingTargets[which(mlpTrainingTargets==-1)]=0;
}
ml<-splitForTrainingAndTest(mlpTrainingInputs, mlpTrainingTargets, ratio = 0.15)

# model<-mlp(mlpTrainingInputs,mlpTrainingTargets,size=c(noOutputUnits),
#            hiddenActFunc = "Act_Logistic", learnFuncParams = c(0.4, 0.0),
#            learnFunc = "Std_Backpropagation")
##main class/function that trains the mlp(multi layer perceptron) and returns the trained model... which is used in the prediction
model<-mlp(ml$inputsTrain,ml$targetsTrain,size=noOutputUnits,
           learnFuncParams = 0.1, maxit = 3000, inputsTest = ml$inputsTest,targetsTest = ml$targetsTest)


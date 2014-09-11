
source("./neuronClass.R")

targetChars<-c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","Z")
hiddenLayer<-vector("list",length=noOutputUnits)

initialiseHiddenLayer<-function(hidden,trainingInputs)
{
  for(i in seq(1,noOutputUnits,1))
  {
    hidden[i]<-neuron(bias=0,weights=c(rep(0,noInputUnits)),inputSignals=trainingInputs,threshold=0,netInput=0,netOutput=0)
  }
  return(hidden);
}

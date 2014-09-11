
source("./DeltaLearningRule/neuronClass.R")

targetChars<-c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","Z")
##This is the hidden which contains the vector with all the neurons...

hiddenLayer<-vector("list",length=noOutputUnits)

##this function initialises the hidden layer with the initial weights and bias and inputsignals
initialiseHiddenLayer<-function(hidden,trainingInputs)
{
  for(i in seq(1,noOutputUnits,1))
  {
    hidden[i]<-neuron(bias=0,weights=c(round(runif(noInputUnits,0,0.5),digits=2)),inputSignals=trainingInputs,threshold=0,netInput=0,netOutput=0)
  }
  return(hidden);
}

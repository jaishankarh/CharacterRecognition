##this file is the main class file which defines the neuron..and its characteristics.
#this neuron class differs from the other neuron class for the perceptron rule only in the leaarning method implemented in the learn function below..
neuron <- setClass("neuron",representation(weights="numeric",inputSignals="numeric",netInput="numeric",threshold="numeric",netOutput="numeric",bias="numeric"))
alpha<-0.1
setInputSignals<-function(neuron, inputSignals)
{
  neuron@inputSignals<-inputSignals;
  return(neuron);
}
setWeights<-function(neuron, weights)
{
  neuron@inputSignals<-inputSignals;
  return(neuron);
}
intialise<-function(neuron, bias,threshold,weights,inputSignals)
{
  neuron@bias<-bias;
  neuron@threshold<-threshold;
  neuron@weights<-weights;
  neuron@inputSignals<-inputSignals;
}
calculateNetInput<-function(neuron)
{
  neuron@netInput <- sum(neuron@weights * neuron@inputSignals) + neuron@bias;
  return(neuron);
}
calculateNetOutput<-function(neuron)
{
  if(neuron@netInput > neuron@threshold)
  {
    neuron@netOutput = 1;
  }
  else if(neuron@netInput < (0-neuron@threshold))
  {
    neuron@netOutput = -1;
  }
  else
  {
    neuron@netOutput = 0;
  }
  return(neuron);
}
##in the function below the net learns using the delta rule..
learn<-function(neuron, target)
{
    e<-target-neuron@netOutput
    neuron@bias = neuron@bias + alpha*e;
    neuron@weights <- neuron@weights + alpha*e * neuron@inputSignals;
    return(neuron);
}

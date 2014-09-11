neuron <- setClass("neuron",representation(weights="numeric",inputSignals="numeric",netInput="numeric",threshold="numeric",netOutput="numeric",bias="numeric"))
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
learn<-function(neuron, target)
{
  if(target != neuron@netOutput)
  {
    neuron@bias = neuron@bias + target;
    neuron@weights <- neuron@weights + target * neuron@inputSignals;
    return(neuron);
  }
  
  return(neuron);
}

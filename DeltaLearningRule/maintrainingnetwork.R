
#learning=FALSE;

source("./DeltaLearningRule/hiddenLayer.R")
tolerance<-0.1;

###################################################
##this function gathers all the individual output from each of the neurons and creates an output vector for easier comparison
gatherNetOutput<-function(hiddenLayer)
{
  output<-c();
  for(i in 1:noOutputUnits)
  {
    output<-c(output,hiddenLayer[[i]]@netOutput)
  }
  return(output);
}

###############################################################
##this function tries to predict on the already initialised and trained network...
#simply applies test input signals. 
#computes the netinput, then the netoutput, and finally the activations
predict<-function(hiddenLayer,inputSignals)
{
  hidden<-lapply(hidden,setInputSignals,inputSignals=inputSignals)
  hidden<-lapply(hidden,calculateNetInput)
  hidden<-lapply(hidden,calculateNetOutput)
  output<-gatherNetOutput(hidden);
  return(output);
}

hidden<-initialiseHiddenLayer(hiddenLayer,c(rep(0,noInputUnits)));

learning=TRUE;
errorindex<-1;
# in delta learning rule the learning continues until the error between the output and target is below a tolerance level deffined at the top of this file
#the errorindex is just an index value for the deltaerror vector which is used for plotting later on
while(learning)
{
  learning=FALSE;
  error<-c()
  for(i in 1:noTrainingInputs)
  {
    hidden<-lapply(hidden,setInputSignals,inputSignals=trainingInputs[[i]])
    hidden<-lapply(hidden,calculateNetInput)
    hidden<-lapply(hidden,calculateNetOutput)
    output<-gatherNetOutput(hidden);
    e<-(output-targets[[i]])
    e<-e*e
    e<-sum(e)
    if(e > 0.1)
    {
      for(j in 1:noOutputUnits)
        hidden[[j]]<-learn(hidden[[j]],targets[[i]][j]);
      learning=TRUE;
    }
    error[i]<-e
  }
  deltaError[errorindex]<-mean(error);
  errorindex=errorindex+1;
}



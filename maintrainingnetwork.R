
#learning=FALSE;

source("./hiddenLayer.R")
#######################################################################
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
#the errorindex is just an index value for the deltaerror vector which is used for plotting later on
#the learning continues until the network manages to guess all the training patterns correctly...
#the error e is calculated just for plotting purposes..
errorindex<-1
while(learning)
{
  error<-c()
  learning=FALSE;
  for(i in 1:noTrainingInputs)
  {
    hidden<-lapply(hidden,setInputSignals,inputSignals=trainingInputs[[i]])
    hidden<-lapply(hidden,calculateNetInput)
    hidden<-lapply(hidden,calculateNetOutput)
    output<-gatherNetOutput(hidden);
    e<-(output-targets[[i]])
    e<-e*e
    e<-sum(e)
    if(any(output!=targets[[i]]))
    {
      for(j in 1:noOutputUnits)
        hidden[[j]]<-learn(hidden[[j]],targets[[i]][j]);
      learning=TRUE;
    }
    error[i]<-e
  }
  myPerceptronError[errorindex]<-mean(error)
  errorindex=errorindex+1;
}



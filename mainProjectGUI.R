##main gui and source file that essentially calls all the other files...contains some of the important global defintions
##the XXXXError variables are important for error plotting

require(gWidgets)
setwd("/home/jaishankar/Documents/R_prog/Project")
options("guiToolkit"="RGtk2")
deltaError<-c();
perceptronError<-c()
myPerceptronError<-c()
hebbError<-c()

mlpTrained=FALSE
deltaTrained=FALSE
mymlpTrained=FALSE
hebbTrained=FALSE
mplpatternTrained=FALSE
mainWindow<-gwindow("ANN R Learning")
mainGroup<-ggroup(horizontal=FALSE,container=mainWindow)
trainingGroup<-ggroup(container=mainGroup)
trainSelect <- gradio(c("Delta Rule","My Perceptron Rule Implementation","Multi-Layer Perceptron","Hebb Rule Pattern Association","MLP Pattern Association"), container=trainingGroup)
targetChars<-c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","Z")


##function that handles the click in the gui...
##depending the choice sources the correct file and performs the learning only once by setting and checking a boolean variable XXXXtrained...
##finally the output display will depend on the predictedoutput which is defined and set in all the corresponding files...
startDemo<-function(h,...)
{
  rule<-svalue(trainSelect);
  if(is.na(rule))
  {
    msg<-"Sorry Choose a Rule to start with"
    gmessage(msg,title="Invalid Choice", handler=function(h,...){return()},parent=mainWindow)
  }
  source("./readFromPgmfile.R")
  source("./scanTrainingInputsFromFiles.R")
  if(rule == "My Perceptron Rule Implementation")
  {
    if(!mymlpTrained)
    {
      mlptime <<- proc.time()
      source("./maintrainingnetwork.R");
      mymlpTrained<<-TRUE;
      mlptime<-proc.time() - mlptime
      print(mlptime)
    }
    predictedoutput<-predict(hidden,inputPredict);
  }
  else if(rule == "Multi-Layer Perceptron")
  {
    if(!mlpTrained)
    {
      mlptime <<- proc.time()
      source("./rsnnsMLP.R");
      mlpTrained<<-TRUE;
      mlptime<-proc.time() - mlptime
      print(mlptime)
    }  
    test<-matrix(inputPredict,nrow=1)
    test[which(test==-1)]<-0
    predictedoutput<-predict.rsnns(model,test)
    print(predictedoutput)
    predictedoutput<-round(predictedoutput)
    
  }
  else if(rule == "Delta Rule")
  {
    if(!deltaTrained)
    {
      mlptime <<- proc.time()
      source("./DeltaLearningRule/maintrainingnetwork.R");
      deltaTrained<<-TRUE;
      mlptime<-proc.time() - mlptime
      print(mlptime)
    }
    predictedoutput<-predict(hidden,inputPredict);
  }
  else if(rule == "Hebb Rule Pattern Association")
  {
    if(!hebbTrained)
    {
      mlptime <<- proc.time()
      source("./HebbTrainingnetwork/hebbTrainingNetwork2.R");
      hebbTrained<<-TRUE;
      mlptime<-proc.time() - mlptime
      print(mlptime)
    }
    r<-c();
    ip<-inputPredict
    for(i in 1:ncol(weightVector))
    {
      r[i]<-sum(weightVector[,i]*ip);
      if(r[i]>0)
      {
        r[i]=0
      }
      else
      {
        r[i]=1
      }
    }
    
    print(r)
    p1<-matrix(r,nrow=7,byrow=TRUE)
    print(p1)
    respix<-pixmapGrey(p1,nrow=7,ncol=5,bbox=c(-1,-1,1,1))
    resultWindow<-gwindow("Predicted Output");
    
    ggraphics(width=250,height=150,ps=3, container=resultWindow)
    ##the sleep below is so that the ggraphics loads completely and fully before the plot command is called..else the plot command throws an error because there is no space to plot anything...
    Sys.sleep(2)
    plot(respix)
    ##this is called so that the plotting happens on the gui instead of some other output
    ggraphics(width=250,height=150,ps=3, container=gwindow("Your Input"))
    Sys.sleep(2)
    plot(pix)
    print(r)
    return();
  }
  else if(rule == "MLP Pattern Association")
  {
    if(!mplpatternTrained)
    {
      mlptime <<- proc.time()
      source("./MLP_pattern_asscociation/rsnnsMPLpatternAssoc.R");
      mplpatternTrained<<-TRUE;
      mlptime<-proc.time() - mlptime
      print(mlptime)
    }
    test<-matrix(inputPredict,nrow=1)
    test[which(test==-1)]<-0
    predictedoutput<-predict.rsnns(model,test)
    print(predictedoutput)
    predictedoutput<-round(predictedoutput)
    predictedoutput[which(predictedoutput==1)]<--1
    predictedoutput[which(predictedoutput==0)]<-1
    predictedoutput[which(predictedoutput==-1)]<-0
    r<-predictedoutput;
    p1<-matrix(r,nrow=7,byrow=TRUE)
    respix<-pixmapGrey(p1,nrow=7,ncol=5,bbox=c(-1,-1,1,1))
    resultWindow<-gwindow("Predicted Output");
    
    ggraphics(width=250,height=150,ps=3, container=resultWindow)
    
    Sys.sleep(2)
    plot(respix)
    ggraphics(width=250,height=150,ps=3, container=gwindow("Your Input"))
    Sys.sleep(2)
    plot(pix)
    print(r)
    return();
    }
  else
  {
    msg<-"Sorry that rule is not yet Implemented!!"
    gmessage(msg,title="Invalid Choice", handler=function(h,...){return()},parent=mainWindow)
  }
  print(predictedoutput);
  index<-which(predictedoutput==1);
  writeLines(targetChars[index]);
  charToPrint<-targetChars[index];
  if(!any(predictedoutput==1))
  {
    charToPrint<-"Sorry Could Not Guess!!"
  }
  resultWindow<-gwindow("Your Input");
  #group<-ggroup(horizontal=FALSE,container=resultWindow)
  ggraphics(width=250,height=150,ps=3, container=resultWindow)
  Sys.sleep(2)
  label<-glabel(charToPrint,container=gwindow("Predicted Output"))
  font(label)<- list(family="times",size=200,weight="bold",style="bold")
  plot(pix)
  
}
startButton<-gbutton("Start",container=mainGroup,handler=startDemo);
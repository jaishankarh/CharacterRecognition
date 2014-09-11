##this file does pattern association using hebbian learning rule..but fails quite miserably.. which i believe is due to the fact that the hebbian lerning rule is not that powerful
##the learning happens simply by doing some matrix operations..instead of doing learnign like in a usual loop manner...done in other implementations...
#this method of calculating weights in a non loop manner is possible wiht this rule only
noHebbTrainingInputs = noTrainingInputs;
noHebbTrainingTargets = noAssocTrainingTargets
noHebbOutputUnits = noAssocOutputUnits
noHebbInputUnits = noInputUnits

##again converting the list inputs and targets to matrix forms because matrix operations are performed
hebbTrainingInputs<-matrix(nrow=noHebbTrainingInputs,ncol=noHebbInputUnits)
hebbTrainingTargets<-matrix(nrow=noHebbTrainingTargets,ncol=noHebbOutputUnits)
for(i in 1:noHebbTrainingInputs)
{
  hebbTrainingInputs[i,]<-trainingInputs[[i]];
  #hebbTrainingInputs[which(hebbTrainingInputs==-1)]=0;
  
}
for(i in 1:noHebbTrainingTargets)
{
  hebbTrainingTargets[i,]<-assocTargets[[i]];
  #hebbTrainingTargets[which(hebbTrainingTargets==-1)]=0;
}
weightVector<-matrix(rep(0,noHebbInputUnits*noHebbOutputUnits), nrow=noHebbInputUnits,ncol=noHebbOutputUnits)
j<-1
#this is where the core calculations take place
for(i in 1:noHebbTrainingInputs)
{
  s<-matrix(hebbTrainingInputs[i,],ncol=1)
  t<-matrix(hebbTrainingTargets[j,],nrow=1)
  weightVector<-weightVector + (s%*%t)
  if(j >= noHebbTrainingTargets)
  {
    j=1
  }
  else
  {
    j=j+1;
  }
}


noHebbTrainingInputs = noTrainingInputs/2;
noHebbTrainingTargets = noTrainingInputs/2
noHebbOutputUnits = noInputUnits
noHebbInputUnits = noInputUnits
hebbTrainingInputs<-matrix(nrow=noHebbTrainingInputs,ncol=noHebbInputUnits)
hebbTrainingTargets<-matrix(nrow=noHebbTrainingTargets,ncol=noHebbOutputUnits)
for(i in 1:noHebbTrainingInputs)
{
  hebbTrainingInputs[i,]<-trainingInputs[[i+noHebbTrainingInputs]];
  #hebbTrainingInputs[which(hebbTrainingInputs==-1)]=0;
  
}
for(i in 1:noHebbTrainingTargets)
{
  hebbTrainingTargets[i,]<-trainingInputs[[i]];
  #hebbTrainingTargets[which(hebbTrainingTargets==-1)]=0;
}
  

weightVector<-matrix(c(rep(0,noHebbOutputUnits*noHebbInputUnits)),ncol=noHebbOutputUnits,nrow=noHebbInputUnits)
j=1;
for(i in 1:noHebbTrainingInputs)
{
  s<-matrix(hebbTrainingInputs[i,],ncol=1)
  t<-matrix(hebbTrainingTargets[j,],nrow=1)
  weightVector <- weightVector + (s%*%t)
  if(j == noHebbTrainingTargets)
  {
    j=1;
  }
  else
  {
    j=j+1;
  }
  
}
#result<-matrix(rep(0,noOutputUnits),ncol=1)

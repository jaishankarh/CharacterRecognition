##this is one of the most required files..this file scans all the inputs and stores a lot of global variables that are used by many scripts..
## the input is got from pre drawn image .pgm files that I have drawn...they are considered to be targets and input for training
##first inputs are got....then targets(for classification),....then targets (for pattern association)
require(stringr)

require(pixmap)
setwd("/home/jaishankar/Documents/R_prog/Project/letters/")
##get the files
picfiles<-list.files()
print(picfiles)
trainingInputs<-list();
targets<-list();
noOutputUnits<- 26;
noTrainingInputs<-52;
noInputUnits<-81;
noAssocOutputUnits<-35
noAssocTrainingTargets<-26
##read and convert input to bipolar form
for(i in 1:length(picfiles))
{
  picfile<-picfiles[i]
  found<-str_locate(picfile,".pgm")
  if(is.na(found[1]))
  {
    msg<-"Sorry Invalid File Input. Please restart the program and choose another file!"
    gmessage(msg,title="Invalid File", handler=function(h,...){return()})
  }else
  {
    x <- read.pnm(picfile)
    
    grey<-x@grey
    input<-as.vector(t(grey)) #to get the matrix into a one dimensional vector...t() is for transpose.. transpose is necessary..otherwise the one dimensional vector does not come as expected
    ##the values in input which contain 1 are converted to -1 because in the .pgm 1 is stored as the value for which the pixel is off and 0 is the value for which the pixel is on...
    input[which(input==1)]=-1
    input[which(input==0)]=1
    trainingInputs[[i]]<-input
    t<-c(rep(-1,noOutputUnits));
    t[i] = 1;
    targets[[i]]<-t;
  }
}
setwd("/home/jaishankar/Documents/R_prog/Project/newletters/")
picfiles<-list.files()
len<-length(picfiles)
for(i in 1:len)
{
  picfile<-picfiles[i]
  found<-str_locate(picfile,".pgm")
  if(is.na(found[1]))
  {
    msg<-"Sorry Invalid File Input. Please restart the program and choose another file!"
    gmessage(msg,title="Invalid File", handler=function(h,...){return()})
  }else
  {
    x <- read.pnm(picfile)
   
    grey<-x@grey
    input<-as.vector(t(grey)) #to get the matrix into a one dimensional vector...t() is for transpose.. transpose is necessary..otherwise the one dimensional vector does not come as expected
    input[which(input==1)]=-1
    input[which(input==0)]=1
    trainingInputs[[i+len]]<-input
    t<-c(rep(-1,noOutputUnits));
    t[i] = 1;
    targets[[i+len]]<-t;
  }
}
setwd("/home/jaishankar/Documents/R_prog/Project/HebbTrainingnetwork/HebbTargets/")
picfiles<-list.files()
assocTargets<-list();

for(i in 1:length(picfiles))
{
  picfile<-picfiles[i]
  found<-str_locate(picfile,".pgm")
  if(is.na(found[1]))
  {
    msg<-"Sorry Invalid File Input. Please restart the program and choose another file!"
    gmessage(msg,title="Invalid File", handler=function(h,...){return()})
  }else
  {
    x <- read.pnm(picfile)
    
    grey<-x@grey
    input<-as.vector(t(grey)) #to get the matrix into a one dimensional vector...t() is for transpose.. transpose is necessary..otherwise the one dimensional vector does not come as expected
    input[which(input==1)]=-1
    input[which(input==0)]=1
    assocTargets[[i]]<-input
    
  }
}

##doing this restoring of the directory is very crucial to the working of the rest of the script..because the source command used in other scripts depend on this

setwd("/home/jaishankar/Documents/R_prog/Project")
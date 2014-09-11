#a main required file that scans the test input given by the user..
#the user is supposed to input a .pgm file...that is a pic file 
##using the pixmap package and the read.pnm we read the file and extract its inputs from the grey components of the image
require(gWidgets)
require(pixmap)
require(stringr)
options("guiToolkit"="RGtk2")
picfile<-gfile()

print(picfile)
found<-str_locate(picfile,".pgm")
assign("inputPredict",c())
if(is.na(found[1]))
{
  msg<-"Sorry Invalid File Input. Please restart the program and choose another file!"
  gmessage(msg,title="Invalid File", handler=function(h,...){return()})
}
  pix <- read.pnm(picfile)
  grey<-pix@grey
  inputPredict<-as.vector(t(grey)) #to get the matrix into a one dimensional vector...t() is for transpose.. transpose is necessary..otherwise the one dimensional vector does not come as expected
  inputPredict[which(inputPredict==1)]<--1
  inputPredict[which(inputPredict==0)]<-1
  ##the above vector inputPredict is the main converted vector that contains the properly converted input from black and white pixels into a bi-polar input vector for processing

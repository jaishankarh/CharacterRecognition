setwd("/home/jaishankar/Documents/R_prog/Project")
con<-file("./fonts(a-k)","r", blocking = FALSE);
chars<-readLines(con);

HebbInputs<-matrix(nrow=3,ncol=63);
j<-1;
for(i in 1:3)
{
  
  
  temp<-strsplit(chars[j:(j+8)],"");
  temp<-unlist(temp);
  temp[which(temp==".")] = -1;
  temp[which(temp=="#")] = 1;
  temp<-as.integer(temp);
  HebbInputs[i,]<-temp;
  j=j+10;
}
close(con)
con<-file("./targetHebbfonts","r", blocking = FALSE);
chars<-readLines(con);

HebbTargets<-matrix(nrow=3,ncol=15);
j<-1;
for(i in 1:3)
{
  
  
  temp<-strsplit(chars[j:(j+4)],"");
  temp<-unlist(temp);
  temp[which(temp==".")] = -1;
  temp[which(temp=="#")] = 1;
  temp<-as.integer(temp);
  HebbTargets[i,]<-temp;
  j<-j+6;
}
close(con)
con<-file("./testinputHebb","r", blocking = FALSE);
chars<-readLines(con);

HebbTests<-matrix(nrow=3,ncol=63);
j<-1;
for(i in 1:3)
{
  
  
  temp<-strsplit(chars[j:(j+8)],"");
  temp<-unlist(temp);
  temp[which(temp==".")] = -1;
  temp[which(temp=="#")] = 1;
  temp<-as.integer(temp);
  HebbTests[i,]<-temp;
  j=j+10;
}
close(con)
weight<-matrix(rep(0,15*63),nrow=63,ncol=15)
for(i in 1:3)
{
  s<-matrix(HebbInputs[i,],ncol=1)
  t<-matrix(HebbTargets[i,],nrow=1)
  weight<-weight+(s%*%t)
}
r<-c()
for(i in 1:15)
{
  r[i]<-sum(HebbTests[3,]*weight[,i])
  if(r[i]>0)
  {
    r[i]<-1
  }
  else
  {
    r[i]<--1
  }
}
print(r)

con<-file("./fonts(a-k)","r", blocking = FALSE);
chars<-readLines(con);
newchars<-list();
trainingInputs<-list();
index<-1;
for(i in seq(1,length(chars),10))
{
  
  newchars[[index]]<-chars[i:(i+8)];
  temp<-strsplit(newchars[[index]],"");
  temp<-unlist(temp);
  temp[which(temp==".")] = -1;
  temp[which(temp=="#")] = 1;
  temp<-as.integer(temp);
  trainingInputs[[index]]<-temp;
  index<-index+1;
}
close(con)
con<-file("./targets_for_fonts(a-k)","r", blocking = FALSE);
chars<-readLines(con);
newchars<-list();
targets<-list();
index<-1;
for(i in seq(1,length(chars)))
{
  
  
  temp<-strsplit(chars[i],"");
  temp<-unlist(temp);
  temp<-temp[-which(temp==",")];
  temp[which(temp=="0")] = -1;
  temp<-as.integer(temp);
  targets[[index]]<-temp;
  index<-index+1;
}
close(con)
con<-file("./newfonts(a-k)","r", blocking = FALSE);
chars<-readLines(con);
newchars<-list();
testinput<-list();
index<-1;
for(i in seq(1,length(chars),10))
{
  
  newchars[[index]]<-chars[i:(i+8)];
  temp<-strsplit(newchars[[index]],"");
  temp<-unlist(temp);
  temp[which(temp==".")] = -1;
  temp[which(temp=="#")] = 1;
  temp<-as.integer(temp);
  testinput[[index]]<-temp;
  index<-index+1;
}
close(con)

prevwd<-getwd();
setwd("/home/jaishankar/Documents/ANN/Font patterns for Rprog/");
con<-file("fonts(a-k)","r", blocking = FALSE);
chars<-readLines(con);
newchars<-list();
input<-list();
index<-1;
for(i in seq(1,length(chars),10))
{
  
  newchars[[index]]<-chars[i:(i+8)];
  temp<-strsplit(chars[1:9],"");
  temp<-unlist(temp);
  temp[which(temp==".")] = -1;
  temp[which(temp=="#")] = 1;
  temp<-as.integer(temp);
  input[[index]]<-temp;
  index<-index+1;
}



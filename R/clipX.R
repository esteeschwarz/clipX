library(stringi)
library(clipr)
#sep=""
clipX<-function( sep = "" ){
  x<-read_clip()
  x2<-unlist(stri_split_boundaries(x,type="character"))
  x3<-x
  m1<-grep(" ",x2)
  m2<-grep("\n",x2)
  m3<-grep(",",x2)
  m4<-grep(sep,x2)
  if(sum(m1)>0)
    x3<-unlist(stri_split_regex(x," "))
  x3
  if(sum(m2)>0)
    x3<-unlist(stri_split_regex(x,"\n"))
  if(sum(m3)>0)
    x3<-unlist(stri_split_regex(x,","))
  if(sum(m4)>0&length(m4)!=max(m4))
    x3<-unlist(stri_split_regex(x,sep))
  x3
  x4<-paste0(x3,collapse = '","')
  x4<-paste0('c("',x4,'")')
  write_clip(x4)
}
# clipX(k)
# c("clipX","is","a","function","to","process","clipboard","content.","it","depends","on","the","clipr","library.")

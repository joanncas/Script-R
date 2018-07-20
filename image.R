library(imager)
library(rJava)
library(xlsx)
library(pracma)
library(rlist)
library(plyr)



linMap <- function(x, from, to)
  (x - min(x)) / max(x - min(x)) * (to - from) + from

DataImg<-read.csv("MURA-v1.1/train_image_paths.csv",header=FALSE)
Etiqueta<-read.csv("MURA-v1.1/train_labeled_studies.csv",header = FALSE)

aum1<-1
aum2<-1
eti<-list()
for(val in 1:50264){
f<-as.character.factor(DataImg[aum1,])
tx<-as.character(Etiqueta[aum2,1]) 
num<-nchar(tx)
if(strcmp(tx,substring(f,0,num))){
    eti[[aum1]]<-Etiqueta[aum2,2]
    aum1=aum1+1
}
  else{
    aum2=aum2+1
  }
}

ej<-data.frame(t(unlist(eti[1:5000])))
salida1<-list()

for(val in 1:5000){
  h<-DataImg[val,]
  f<-as.character.factor(h)
  im <- load.image(f)
  J <- resize(im, size_x = 320L, size_y = 320L)
  T<-as.vector(J)
  x<-as.integer(linMap(T,0,250)) 
  Th<-data.frame(x)
  salida1[[val]]<-Th
}

#lo<-salida1
#lo <- list(as.data.frame(unlist(salida1[[1]])),as.data.frame(unlist(salida1[[2]])))
#lo<-list(Th)
#prueba<-do.call(rbind.fill, lo)

#for(val in 1:100){
#  prueba[[2]][1:lengths(salida1[[2]])]<-prueba[[2]][(lengths(salida1[[1]])+1):length(prueba[[2]])]
#  prueba[[2]][(lengths(salida1[[1]])+1):length(prueba[[2]])]<-NA
#  prueba[is.na(prueba)] <- ""
#}

l <- list(ej,as.data.frame(salida1))
final<-do.call(rbind.fill, l)
for(val in 1:5000){
  final[[val]][2:length(final[[val+5000]])]<-final[[val+5000]][2:length(final[[val+5000]])]
}

final<-list.remove(final, 5001:10000)

write.csv(final, file="test2.csv", row.names=FALSE)


library(plyr)
library(reshape2)
library(haven)

#### Descargar datos Actualizado ####
url<-"https://almacenamiento.msp.gob.ec/index.php/s/JgWHk8QARpWbF3Z/download?path=%2F"
te=tempfile(fileext = ".zip")
download.file(url,te,mode = "wb",quiet = T)

fl<-unzip(te,list = T)[1,1]
unzip(te,exdir = dirname(te))
fl<-paste0(dirname(te),"/",fl)
fl<-list.files(fl,full.names = T)
fl<-fl[grep("\\.sav",fl)]

dat<-read_sav(fl)
e<-suppressWarnings(sapply(list.files(dirname(te),recursive = T,all.files = T,full.names = T,include.dirs = T),file.remove))
rm(e,fl,te,url)

#### ####
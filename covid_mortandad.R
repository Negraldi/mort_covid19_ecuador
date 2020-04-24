library(plyr)
library(reshape2)
library(haven)
library(earth)

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


#### Estructuro los datos ####
tb<-dat[dat$clasificacion_caso %in% c("Probable","Confirmado"),c("edad","sexo","condicion_paciente","cod_cant_res" )]
tb$cond<-tb$condicion_paciente
tb$pr<-(0+(tb$cond=="Muerto"))
tb$w<-1
tb$lpr<-log1p(tb$pr*tb$w)-log1p((1-tb$pr)*tb$w)
tb$cant<-factor(tb$cod_cant_res)
tb$prov<-factor(substr(tb$cant,1,3))
tb$sexo<-factor(tb$sexo)
tb$condicion_paciente=NULL
tb$cod_cant_res=NULL

#Uso el modelo earth binomial
ft<-earth(pr~edad+cant+prov+sexo,data=tb,degree = 2,
          glm = list(family=quasibinomial()),
          pmethod ="seqrep",thresh = 0.001)


####  Generar los datos para la prediccion #### 
nw=expand.grid(edad=seq(min(tb$edad),max(tb$edad)),
               sexo=levels(tb$sexo),
               cant=levels(tb$cant))

nw$prov=factor(substr(nw$cant,1,3),levels = levels(tb$prov))
  
#### Predicción e Intervalo de Predicción####
pr_sd<-
simplify2array(
  predict(ft$glm.list[[1]],
          as.data.frame(earth:::model.matrix.earth(ft,nw)[,-1]),
          type="link",se.fit = TRUE)[c(1,2)]
)

pr_int<-
1/(1+exp(
  -simplify2array(
    list(
      Infe=pr_sd[,1]-6*pr_sd[,2],
      Medi=pr_sd[,1],
      Supe=pr_sd[,1]+6*pr_sd[,2]
    )
  )
))
rm(pr_sd)


colnames(nw)<-c("edad", "sexo","cat","prov")
rm(tb)

write.csv(cbind(nw,pr_int),"covid_dat.csv")
file.show("covid_dat.csv")


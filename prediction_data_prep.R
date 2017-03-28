library(randomForest)
library(glmnet)
library(nlme)
library(MASS)
require(spaMM)
require(lmerTest)
require(openxlsx)
require(downloader)


##function to determine maximum drought period
max.drought<-function(x,min=0){
  x<-ifelse(as.numeric(x)<=min,0,1)
  cs<-cumsum(x)
  cs<-cs[x==0]
  if(length(cs)>0) {
  	mx<-max(table(cs))
  	} else mx<-0
  return(mx)
}


###download data and store in appropriate directory, create directory if not present

dir.create("data/processed/challenge_data/",showWarnings=FALSE, recursive=TRUE)

download("https://www.dropbox.com/s/g6f35g7n7cca0pm/challenge_training.data.predvar.xlsx?raw=1", "data/processed/challenge_data/challenge_training.data.predvar.xlsx", mode="wb")
download("https://www.dropbox.com/s/5yhqxs31u9z6tsr/challenge_validation.coords.predvar.xlsx?raw=1","data/processed/challenge_data/challenge_validation.coords.predvar.xlsx", mode="wb")
download("https://www.dropbox.com/s/c64aqkgi0crqidw/challenge_grid.coords.predvar.xlsx?raw=1", "data/processed/challenge_data/challenge_grid.coords.predvar.xlsx", mode="wb")


#####read files and process for prediction

train.data<-read.xlsx("data/processed/challenge_data/challenge_training.data.predvar.xlsx",check.names=TRUE)
train.data <-as.data.frame(data.matrix(train.data))
#get rid of extremes
train.data$fert.area<-train.data$amount.of.inorganic.fertilizer/train.data$area
train.data$fert.area[train.data$fert.area>1000]<-NA
##set fert.area to median in case of NA
train.data$fert.area[is.na(train.data$fert.area)]<-median(train.data$fert.area,na.rm=T)
##get rid of variables you will not use
train.data$vtype<-NULL
train.data$vname <-NULL
train.data$fertilizeruse <-NULL
train.data$fertilizer.type.organic <-NULL
train.data$fertilizer.type.inorganic <-NULL
train.data$amount.of.inorganic.fertilizer <-NULL
train.data$amount.of.organic.fertilizer <-NULL
train.data$unit.for.organic.fertilizer <-NULL

valid.data<-read.xlsx("data/processed/challenge_data/challenge_validation.coords.predvar.xlsx",check.names=TRUE)
valid.data <-as.data.frame(data.matrix(valid.data))
#get rid of extremes
valid.data$fert.area<-valid.data$amount.of.inorganic.fertilizer/valid.data$area
valid.data$fert.area[valid.data$fert.area>1000]<-NA
##set fert.area to median in case of NA
valid.data$fert.area[is.na(valid.data$fert.area)]<-median(valid.data$fert.area,na.rm=T)
##get rid of variables you will not use
valid.data$vtype<-NULL
valid.data$vname <-NULL
valid.data$fertilizeruse <-NULL
valid.data$fertilizer.type.organic <-NULL
valid.data$fertilizer.type.inorganic <-NULL
valid.data$amount.of.inorganic.fertilizer <-NULL
valid.data$amount.of.organic.fertilizer <-NULL
valid.data$unit.for.organic.fertilizer <-NULL


grid.data<-read.xlsx("data/processed/challenge_data/challenge_grid.coords.predvar.xlsx",check.names=TRUE)
grid.data<-as.data.frame(data.matrix(grid.data))
###remove gridpoints with na
na.vec<-apply(grid.data,1,function(x) sum(is.na(x)))
grid.data <-grid.data[which(na.vec==0),]



###set rownames to allow later matching
rownames(train.data)<-1:nrow(train.data)
rownames(valid.data)<-(max(as.numeric(rownames(train.data)))+1):(max(as.numeric(rownames(train.data)))+nrow(valid.data))
rownames(grid.data)<-(max(as.numeric(rownames(valid.data)))+1):(max(as.numeric(rownames(valid.data)))+nrow(grid.data))


##do rainfall pca 
#first join rain of 3 datesets
rain.train<-train.data[,grep("rain",colnames(train.data))]
rain.valid<-valid.data[,grep("rain",colnames(valid.data))]
rain.grid<-grid.data[,grep("rain",colnames(grid.data))]
rain.data<-rbind(rain.train,rain.valid,rain.grid)
##remove erroneous one if present
rain.data$rain20150101<-NULL

##remove rain from data
train.data<-train.data[,grep("rain",colnames(train.data),invert=T)]
valid.data<-valid.data[,grep("rain",colnames(valid.data),invert=T)]
grid.data<-grid.data[,grep("rain",colnames(grid.data),invert=T)]


pc.rain<-prcomp(rain.data)
#plot(log(pc.rain$sdev[1:100]^2))

sum(pc.rain$sdev[1:20]^2)/sum(pc.rain$sdev^2)

rain.pcs<-pc.rain$x[,1:20]
colnames(rain.pcs)<-paste("precip.",colnames(rain.pcs),sep="")

##claculate total rainfall and drought
rain.data$total.precip<-rowSums(rain.data)
rain.data$drougth.days<-apply(rain.data,1,max.drought,min=0.5)
rain.data$drought<-ifelse(rain.data$drougth.days>=7,1,0)

##join pcs 
rain.data<-cbind(rain.data,rain.pcs)



##join raindata to other data
train.data <-cbind(train.data,rain.data[match(rownames(train.data),rownames(rain.data)),])
valid.data <-cbind(valid.data, rain.data[match(rownames(valid.data),rownames(rain.data)),])
grid.data <-cbind(grid.data, rain.data[match(rownames(grid.data),rownames(rain.data)),])



##write data for prediction
write.xlsx(train.data,file="data/processed/challenge_data/challenge_training.data.prepared_data.xlsx")

write.xlsx(valid.data,file="data/processed/challenge_data/challenge_validation.data.prepared_data.xlsx")

write.xlsx(grid.data,file="data/processed/challenge_data/challenge_grid.data.prepared_data.xlsx")







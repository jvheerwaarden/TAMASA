###prepare data for analysis
source('prediction_data_prep.R')

##read prediction function
source('prediction_functions.R')


##do predictions and write results to file
###decide if prediction has to be spatial (will ditch non remote sensing predictors) and if to keep raw rain data or only PCs
spatial.predict=TRUE
keep.raw.rain=FALSE

###read data
train.data<-read.xlsx("data/processed/challenge_data/challenge_training.data.prepared_data.xlsx",check.names=TRUE)
train.data <-as.data.frame(data.matrix(train.data))

valid.data<-read.xlsx("data/processed/challenge_data/challenge_validation.data.prepared_data.xlsx",check.names=TRUE)
valid.data <-as.data.frame(data.matrix(valid.data))
valid.data$maizeyield<-NULL

##grid data for spatial prediction
grid.data<-read.xlsx("data/processed/challenge_data/challenge_grid.data.prepared_data.xlsx",check.names=TRUE)
grid.data <-as.data.frame(data.matrix(grid.data))

##prepare data for preciction
yield<-train.data$maizeyield
train.data$maizeyield<-NULL


###now go


###get rid of non-rs data if spatial
if(spatial.predict==TRUE){

train.data<-train.data[,colnames(train.data)%in%colnames(grid.data)]	
valid.data<-valid.data[,colnames(valid.data)%in%colnames(grid.data)]	
		
}


#####ditch raw daily rainfall if desired
if(keep.raw.rain==FALSE){
train.data<-train.data[,grep("rain",colnames(train.data),invert=T)]
valid.data<-valid.data[,grep("rain",colnames(valid.data),invert=T)]	
grid.data<-grid.data[,grep("rain",colnames(grid.data),invert=T)]	
	
}



####Do elastic net regresssion
pred.en<-trait.prediction(trait1=yield, env.data= train.data, pred.meth="en",alpha.en=0.1)

##print optimistic observed r-squared
cor(pred.en$trait1.pred, pred.en$trait1)^2

####Do random forest regresssion
pred.rf<-trait.prediction(trait1=yield, env.data= train.data, pred.meth="rf")

##print optimistic observed r-squared
cor(pred.rf$trait1.pred, pred.rf$trait1)^2


####Use Bayesian Additive Regression Trees ->Markus' method of choice
pred.brt<-trait.prediction(trait1=yield, env.data= train.data, pred.meth="brt")
cor(pred.brt$trait1.pred,pred.brt $trait1)^2


####Use clunky, unstable, traditional linear mixed analysis with variable selection-> Joost's rather lame but probably equially successful method of choice
pred.lmm<-trait.prediction(trait1=yield, env.data= train.data, pred.meth="lmm1", max.var.lmm=10, min.dist.lmm=0.3)

##print optimistic observed r-squared
cor(pred.lmm$trait1.pred,pred.lmm$trait1)^2


###predict validation yields
val.pred.lmm<-trait.predict(valid.data,pred.meth="lmm",pred.object= pred.lmm)
val.pred.en<-trait.predict(valid.data,pred.meth="en",pred.object= pred.en)
val.pred.rf<-trait.predict(valid.data,pred.meth="rf",pred.object= pred.rf)
val.pred.brt<-trait.predict(valid.data,pred.meth="brt",pred.object= pred.brt)

if(spatial.predict==TRUE){

###predict spatial grid yields

grid.pred.lmm<-trait.predict(grid.data,pred.meth="lmm",pred.object= pred.lmm)
grid.pred.en<-trait.predict(grid.data,pred.meth="en",pred.object= pred.en)
grid.pred.rf<-trait.predict(grid.data,pred.meth="brt",pred.object= pred.brt)
grid.pred.brt<-trait.predict(grid.data,pred.meth="brt",pred.object= pred.brt)
	
	
}


###make list of preiction objects and save to file
pred.Obj.list<-list(pred.en, pred.rf, pred.lmm, pred.brt)
names(pred.Obj.list)<-c("pred.en","pred.rf","pred.lmm", "pred.brt")
save(pred.Obj.list,file="data/processed/challenge_data/pred.Obj.list.Rdata")

####write prediction results to files
val.pred.frame<-cbind(rownames(valid.data), valid.data[,c("longitude","latitude")],val.pred.lmm, val.pred.en,val.pred.rf, val.pred.brt)
colnames(val.pred.frame)[1]<-"ID"

grid.pred.frame<-cbind(rownames(grid.data), grid.data[,c("longitude","latitude")], grid.pred.lmm, grid.pred.en, grid.pred.rf, grid.pred.brt)
colnames(grid.pred.frame)[1]<-"ID"

##write validation predictions
write.xlsx(val.pred.frame,file="data/processed/challenge_data/prediction.frame.validation.xlsx")

if(spatial.predict==TRUE){

##write grid predictions

write.xlsx(grid.pred.frame,file="data/processed/challenge_data/prediction.frame.grid.xlsx")

}
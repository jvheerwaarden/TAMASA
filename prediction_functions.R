library(randomForest)
library(glmnet)
library(nlme)
library(MASS)
require(spaMM)
require(lmerTest)
require(abind)
require(foreach)
require(doMC)
require(caret)
require(doParallel)



##trait prediction function
trait.predict<-function(env.data, pred.meth,pred.object){
prediction.object=pred.object$prediction.object
if(pred.meth =="rf"| pred.meth=="lmm"| pred.meth=="brt"){
if(pred.meth=="lmm") {env.data$dummy<-1 }	
prediction<-predict(prediction.object,data.frame(env.data))
             	}
 

if (pred.meth =="en"){  ##en
##### input :
# vector (type numeric) Y (no matrix or data-frame!) ; data.frames X and X.new
model.new.X <- model.matrix(lm(as.formula(paste(  'y~(', paste(colnames(env.data),collapse='+'),')*(',paste(colnames(env.data),collapse='+'),')' )), data=data.frame(env.data,y=1:nrow(env.data))))[,-1]
prediction<-c(predict(object= prediction.object,newx=model.new.X)) # prediction for the new X
	return(prediction)	
	
               } ##en

return(prediction)	
	
}


trait.prediction<-function(trait1,env.data, pred.meth,alpha.en=0.5, max.var.lmm=25,min.dist.lmm=0.1){

if (pred.meth=="rf"){
dat<-data.frame(env.data)
rf<-randomForest(dat,y= trait1,importance=TRUE)
trait1<-c(trait1)
trait1.pred<-c(rf$predicted)
prediction.object=rf	
}


if (pred.meth=="en"){
alpha <- alpha.en
Y <- c(trait1)
X <- as.matrix(env.data)
model.X <- model.matrix(lm(as.formula(paste(  "y~(", paste(colnames(X),collapse="+"),")*(",paste(colnames(X),collapse="+"),")" )),data=data.frame(X,y=1:nrow(X))))[,-1]
prediction.object <- cv.glmnet(x=model.X, y=Y, nfolds=10,alpha=alpha)
trait1.pred<-c(predict(object=prediction.object,newx=model.X)) # prediction for the old X
	}





if (pred.meth=="lmm1"){

##parallel variable evaluation
mix.mod.fit.par<-function(data,var,dep.var){
data$depvar<-dep.var
full.model<-paste("nlme::lme(fixed= depvar ~ ", var,", random = ~ 1 | dummy, correlation = corMatern(form = ~ longitude+latitude |dummy),data= data, method = 'ML')",sep="")
lmm1<-eval(parse(text= full.model))
pval<-anova(lmm1)$'p-value'[2]
names(pval)<-var	
return(pval)	
} ##parallel variable evaluation

##remove non varying variables
var.vec<-apply(env.data,2,var)
env.data<-env.data[,var.vec>0]
vars<-colnames(env.data)
vars<-vars[grep("dummy",vars,invert=T)]

env.data$dummy<-1
##hack to avoid problems with lme, set to global, remove at the end
trait1<<-trait1
env.data <<-env.data


#set appropriate number of cores
cores=detectCores()
registerDoMC(cores= cores)
pval <- foreach(i=1:length(vars)) %dopar% {mix.mod.fit.par(data= env.data,var=vars[i], dep.var= trait1)}  
pval.tot<-abind(pval,along=1) 
vars<-names(sort(pval.tot))

##deal with strong correlations
mat<-cor(env.data[, vars])
d<-as.dist(1-mat)
hc<-hclust(d,method="average")
ct<-cutree(hc,h=min.dist.lmm)
##remove duplicates
vars<-vars[!duplicated(ct)]

##select subset to keep p>n 
if(length(vars)>max.var.lmm) {vars<-vars[1:max.var.lmm]}

full.model<-paste("nlme::lme(fixed= trait1 ~ ",paste(vars,collapse="+"),", random = ~ 1 | dummy, correlation = corMatern(form = ~ longitude+latitude |dummy), data = env.data, method = 'ML')",sep="")

lmm1<-eval(parse(text=full.model))
mod.sel<-stepAIC(lmm1, direction="both")

trait1.pred<-predict(mod.sel)
prediction.object<-mod.sel

}	

if (pred.meth=="brt"){
# bartMachine -------------------------------------------------------------
options(java.parameters = "-Xmx8000m")
require(bartMachine)
# Start doParallel to parallelize model fitting
mc <- makeCluster(detectCores())
registerDoParallel(mc)
# Control setup
tc <- trainControl(method = "cv", returnResamp = "all", allowParallel = T)
# Fit model
typ.bar <- train(env.data, trait1,
                 method = "bartMachine", 
                 preProc = c("center", "scale"),
                 trControl = tc,
                 tuneLength = 3,
                 verbose = FALSE,
                 seed = 123)


trait1.pred<-predict(typ.bar)	
prediction.object<-typ.bar

}
	


result<-list(trait1,trait1.pred, prediction.object)
names(result)<-c("trait1","trait1.pred","prediction.object")	

if(!is.null(trait1)) rm(trait1, envir =.GlobalEnv)
if(!is.null(env.data)) rm(env.data, envir =.GlobalEnv)

return(result)	
}



if(Sys.info()[1]=="Windows")
  setwd("C:/Users/rockc_000/Documents/Personal Files/Kaggle/Higgs Boson/")
if(Sys.info()[1]=="Linux" & Sys.info()[4]=="jb")
  setwd("/media/storage/Personal Files/Kaggle/Higgs Boson/")
if(Sys.info()[1]=="Linux" & grepl("ch120",Sys.info()[4]))
  setwd("~/Kaggle/Higgs Boson")
load("Data/d2.RData")

sam = sample(1:nrow(d2), size=5000)
table( d2[sam,"cvGroup"] )

#Boosting
library(gbm)
ensem = cvModel(modelFunc=gbm, cvGroup=d2$cvGroup[sam], d=d2[sam,]
      ,form=Signal~DER_deltaeta_jet_jet + DER_mass_jet_jet + DER_prodeta_jet_jet
      ,predFunc=function(fit, newdata){
        out = predict(fit, newdata, n.trees=c(10,20,50,100,200,500))
        rownames(out) = rownames(newdata)
        return(out)
      }
      ,args=list(n.trees=500, interaction.depth=8), pred.cols=6 )
ensem = cvModel(modelFunc=gbm.fit, cvGroup=d2$cvGroup[sam], x=d2[sam,6:8], y=d2[sam,46]
      ,predFunc=function(fit, newdata){
        out = predict(fit, newdata, n.trees=c(10,20,50,100,200,500))
        rownames(out) = rownames(newdata)
        return(out)
      }
      ,args=list(n.trees=500, interaction.depth=8), pred.cols=6 )

#GAM
library(mgcv)
ensem = cvModel(modelFunc=gam, cvGroup=d2$cvGroup[sam], d=d2[sam,]
    ,predFunc=function(fit, newdata){
      out=matrix(predict(fit, newdata=newdata, type="response"), ncol=1)
      rownames(out) = rownames(newdata)
      return(out)
    }
    ,form=Signal~s(DER_deltaeta_jet_jet) + s(DER_mass_jet_jet) + s(DER_prodeta_jet_jet)
    ,args=list(family="binomial"))

#GLM
ensem = cvModel(modelFunc=glm, cvGroup=d2$cvGroup[sam], d=d2[sam,]
    ,form=Signal~DER_deltaeta_jet_jet + DER_mass_jet_jet + DER_prodeta_jet_jet
    ,args=list(family="binomial") )

#GLMNET
library(glmnet)
ensem = cvModel(modelFunc=function(x,y,family){
        glmnet(x=as.matrix(x), y=y, family=family, lambda=10^seq(1,-2,length.out=10) )
      }
      ,cvGroup=d2$cvGroup[sam], x=d2[sam,6:8], y=d2[sam,46]
      ,predFunc=function(fit, newdata){
        out = predict(fit, as.matrix(newdata), n.trees=c(10,20,50,100,200,500))
        rownames(out) = rownames(newdata)
        return(out)
      }
      ,args=list(family="binomial"), pred.cols=10 )

# Naive Bayes
library(e1071)

#Neural Network: RSNNS
library(RSNNS)
ensem = cvModel(modelFunc=rbf, cvGroup=d2$cvGroup[sam], x=d2[sam,6:8], y=d2[sam,46]
      ,args=list(size=4, maxit=50) )
summary( ensem[[1]] )

#Neural Network: nnet
library(nnet)
ensem = cvModel(modelFunc=nnet, cvGroup=d2$cvGroup[sam], x=d2[sam,6:8], y=d2[sam,46]
      ,args=list(size=4, maxit=50, linout=F) )
summary( ensem[[1]] )

#Neural Network: neuralnet
library(neuralnet)
###PROBLEMATIC!!!  neuralnet can't predict if it fails to converge, which messes up this algo.
fit = neuralnet(Signal~DER_deltaeta_jet_jet + DER_mass_jet_jet + DER_prodeta_jet_jet, d2[sam,] )
compute(fit, covariate=d2[sam,all.vars(form)[-1]])$net.result
ensem = cvModel(modelFunc=neuralnet, cvGroup=d2$cvGroup[sam], d=d2[sam,]
    ,form=Signal~DER_deltaeta_jet_jet + DER_mass_jet_jet + DER_prodeta_jet_jet
    ,predFunc=function(fit, newdata){
      compute(fit, covariate=newdata[,all.vars(form)[-1]])$net.result
    }
    ,args=list(hidden=c(4,2), linear.output=FALSE, stepmax=1e4) )
summary( ensem[[1]] )

#Principal Components Regression
library(pls)
#Warning: fits a linear regression model, does not (AFAIK) support logistic models
ensem = cvModel(modelFunc=pcr, cvGroup=d2$cvGroup[sam], d=d2[sam,]
    ,predFunc=function(fit, newdata){
      #preds are stored in an array: nrow x 1 x nVars
      preds = predict(fit, newdata)
      return(preds[,1,])
    }
    ,form=Signal~DER_deltaeta_jet_jet + DER_mass_jet_jet + DER_prodeta_jet_jet
    ,pred.cols=length(all.vars(form))-1, args=list(ncomp=length(all.vars(form))-1))
summary(ensem[[1]])
ensem = cvModel(modelFunc=plsr, cvGroup=d2$cvGroup[sam], d=d2[sam,]
    ,predFunc=function(fit, newdata){
      #preds are stored in an array: nrow x 1 x nVars
      preds = predict(fit, newdata)
      return(preds[,1,])
    }
    ,form=Signal~DER_deltaeta_jet_jet + DER_mass_jet_jet + DER_prodeta_jet_jet
    ,pred.cols=length(all.vars(form))-1, args=list(ncomp=length(all.vars(form))-1))
summary(ensem[[1]])

#Random Forests
library(randomForest)
ensem = cvModel(modelFunc=randomForest, cvGroup=d2$cvGroup[sam], d=d2[sam,]
      ,form=Signal~DER_deltaeta_jet_jet + DER_mass_jet_jet + DER_prodeta_jet_jet
      ,args=list(ntree=100) )
ensem = cvModel(modelFunc=randomForest, cvGroup=d2$cvGroup[sam], x=d2[sam,6:8]
      ,y=d2[sam,46], args=list(ntree=100) )

#RPART
library(rpart)
ensem = cvModel(modelFunc=rpart, cvGroup=d2$cvGroup[sam], d=d2[sam,]
    ,form=Signal~DER_deltaeta_jet_jet + DER_mass_jet_jet + DER_prodeta_jet_jet)
summary(ensem[[1]])

#Support Vector Machine
library(kernlab)
#Problems implementing...  Predict function does not seem to work
fit = ksvm(x=as.matrix(d2[sam,6:8][cvGroup!=-1,]), y=as.matrix(d2[sam,46][cvGroup!=-1]), type="C-svc", probabilities=T)
predict(fit, newdata=d2[sam,])
ensem = cvModel(modelFunc=svm, cvGroup=d2$cvGroup[sam], d=d2[sam,]
    ,predFunc=function(fit, newdata){
      #preds are stored in an array: nrow x 1 x nVars
      preds = predict(fit, newdata)
      return(preds[,1,])
    }
    ,form=Signal~DER_deltaeta_jet_jet + DER_mass_jet_jet + DER_prodeta_jet_jet
    ,pred.cols=length(all.vars(form))-1, args=list(ncomp=length(all.vars(form))-1))

pkgname <- "ensemble"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('ensemble')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("cvModel")
### * cvModel

flush(stderr()); flush(stdout())

### Name: cvModel
### Title: Build an Cross-Validated Model
### Aliases: cvModel
### Keywords: cross-validation ensemble

### ** Examples

d = data.frame( x1=rnorm(1000), x2=rnorm(1000), y=rnorm(1000) )
out = cvModel( modelFunc=glm
    ,cvGroup=sample(1:10, size=1000, replace=TRUE)
    ,form="y ~ x1 + x2"
    ,d=d )
library(glmnet)
out = cvModel( modelFunc=glmnet
    ,cvGroup=sample(1:10, size=1000, replace=TRUE)
    ,x=d[,1:2]
    ,y=d$y
    ,predFunc=function(fit, newdata){predict(fit, newx=newdata, s=c(4,2,1,.5))}
    ,pred.cols=4)



cleanEx()
nameEx("makeEnsem")
### * makeEnsem

flush(stderr()); flush(stdout())

### Name: makeEnsem
### Title: Combine multiple predictions into a final ensemble prediction.
### Aliases: makeEnsem
### Keywords: ensemble cross-validate

### ** Examples

d = data.frame( x1=rnorm(1000), x2=rnorm(1000), y=rnorm(1000) )
#Assume last 100 rows are test data
cvGroup = c(sample(1:10,size=900,replace=TRUE), rep(-1,100))
d$y[901:1000] = NA
out = cvModel( modelFunc=glm
    ,cvGroup=cvGroup
    ,form="y ~ x1 + x2"
    ,d=d )
library(glmnet)
out2 = cvModel( modelFunc=glmnet
    ,cvGroup=cvGroup
    ,x=d[,1:2]
    ,y=d$y
    ,predFunc=function(fit, newdata){predict(fit, newx=newdata, s=c(4,2,1,.5))}
    ,pred.cols=4)
dir.create("Submissions")
write.csv(out$ensemble, file="Submissions/glm_raw.csv", row.names=FALSE)
write.csv(out2$ensemble, file="Submissions/glmnet_raw.csv", row.names=FALSE)
ensem = makeEnsem(actual=d$y, prune=.2)



### * <FOOTER>
###
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')

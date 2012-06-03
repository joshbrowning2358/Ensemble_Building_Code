svm.ensem = function( d, test.bt, cv.group, ind_col, dep_cols
      ,rsig = 10^seq(-1,1,.25)
      ,polydeg = c(1,2,3)
      ,C.vec = 10^seq(-2,2,.5)
      ,type = "Classification"
      ,name = ""
      )
{
  ensem.svm = data.frame(matrix( 0, nrow=nrow( d ), ncol=(1+as.numeric(type=="Classification"))*(length(rsig)*length(C.vec)+length(polydeg)*length(C.vec)) ) )
  colnames(d )[ind_col] = "y"
  d = data.frame( cbind(d[,ind_col], scale(d[,dep_cols]) ) )
  num.groups= length( table( cv.group ) ) - 1
  library( kernlab )
  col.csr = 1
  for( k in C.vec )
  {
    for( j in rsig )
    {
      for( i in 1:num.groups )
      {
        if( type=="Classification" )
        {
          t = ksvm( x = as.matrix(d[cv.group != i & cv.group>0,dep_cols])
            ,y = as.factor(d[cv.group != i & cv.group>0,ind_col])
            ,kernel = "rbfdot"
            ,kpar = list( sigma = j )
            ,C = k
            ,prob.model = T
            )
          #Predict probabilities
          ensem.svm[test.bt==0 & cv.group==i,col.csr] =
              predict( t, newdata=d[cv.group==i,dep_cols], type="probabilities" )[,2]
          ensem.svm[test.bt==1,col.csr] =
              ensem.svm[test.bt==1,col.csr] +
              predict( t, newdata=d[test.bt==1,dep_cols], type="probabilities")[,2]/num.groups
          colnames( ensem.svm )[col.csr] = paste( "svm_prob_sig", round(j),"_C",round(k,3),sep="")
          if( name!="") colnames(ensem.svm)[col.csr] = paste(colnames(ensem.svm)[col.csr],"_",name,sep="")
          col.csr = col.csr + 1
          #Predict class
          ensem.svm[test.bt==0 & cv.group==i,col.csr] =
              as.numeric( predict( t, newdata=d[cv.group==i,dep_cols] ) ) - 1
          ensem.svm[test.bt==1,col.csr] =
              ensem.svm[test.bt==1,col.csr] +
              ( as.numeric( predict( t, newdata=d[test.bt==1,dep_cols] ) ) - 1) /num.groups
          colnames( ensem.svm )[col.csr] = paste( "svm_fac_sig", round(j),"_C",round(k,3),sep="")
          if( name!="") colnames(ensem.svm)[col.csr] = paste(colnames(ensem.svm)[col.csr],"_",name,sep="")
          if( name=="" ) write.csv( file=paste("ensem_svm_",proc.time()[[3]],".csv",sep=""), ensem.svm, row.names=F )
          if( name!="" ) write.csv( file=paste("ensem_svm_",name,proc.time()[[3]],".csv",sep=""), ensem.svm, row.names=F )
          col.csr = col.csr - 1
        }
        if( type=="Regression" )
        {
          t = ksvm( x = as.matrix(d[cv.group != i & cv.group>0,dep_cols])
            ,y = d[cv.group != i & cv.group>0,ind_col]
            ,kernel = "rbfdot"
            ,kpar = list( sigma = j )
            ,C = k
            )
          ensem.svm[test.bt==0 & cv.group==i,col.csr] =
            predict( t, newdata=d[cv.group==i,dep_cols])
          ensem.svm[test.bt==1,col.csr] =
            ensem.svm[test.bt==1,col.csr] +
            predict( t, newdata=d[test.bt==1,dep_cols])/num.groups
          colnames( ensem.svm )[col.csr] = paste( "svm_prob_sig", round(j),"_C",round(k,3),sep="")
          if( name!="") colnames(ensem.svm)[col.csr] = paste(colnames(ensem.svm)[col.csr],"_",name,sep="")
        }
      }
      if( type=="Classification" ) col.csr = col.csr + 2
      if( type=="Regression" ) col.csr = col.csr + 1
    }
  }
  for( k in C.vec )
  {
    for( j in polydeg )
    {
      for( i in num.groups )
      {
        if( type=="Classification" )
        {
          t = ksvm( x = as.matrix(d[cv.group != i & cv.group>0,dep_cols])
            ,y = as.factor(d[cv.group != i & cv.group>0,ind_col])
            ,kernel = "polydot"
            ,kpar = list( degree = j )
            ,C = k
            ,prob.model = T
            )
          #Predict probabilities
          ensem.svm[test.bt==0 & cv.group==i,col.csr] =
            predict( t, newdata=d[cv.group==i,dep_cols], type="probabilities" )[,2]
          ensem.svm[test.bt==1,col.csr] =
            ensem.svm[test.bt==1,col.csr] +
            predict( t, newdata=d[test.bt==1,dep_cols], type="probabilities")[,2]/num.groups
          colnames( ensem.svm )[col.csr] = paste( "svm_prob_deg", j,"_C",round(k,3),sep="")
          if( name!="") colnames(ensem.svm)[col.csr] = paste(colnames(ensem.svm)[col.csr],"_",name,sep="")
          col.csr = col.csr + 1
          #Predict class
          ensem.svm[test.bt==0 & cv.group==i,col.csr] =
            as.numeric( predict( t, newdata=d[cv.group==i,dep_cols] ) ) - 1
          ensem.svm[test.bt==1,col.csr] =
            ensem.svm[test.bt==1,col.csr] +
            ( as.numeric( predict( t, newdata=d[test.bt==1,dep_cols] ) ) - 1) /num.groups
          colnames( ensem.svm )[col.csr] = paste( "svm_fac_deg", j,"_C",round(k,3),sep="")
          if( name!="") colnames(ensem.svm)[col.csr] = paste(colnames(ensem.svm)[col.csr],"_",name,sep="")
          col.csr = col.csr - 1
        }
        if( type=="Regression" )
        {
          t = ksvm( x = as.matrix(d[cv.group != i & cv.group>0,dep_cols])
            ,y = d[cv.group != i & cv.group>0,ind_col]
            ,kernel = "polydot"
            ,kpar = list( degree = j )
            ,C = k
            )
          ensem.svm[test.bt==0 & cv.group==i,col.csr] =
            predict( t, newdata=d[cv.group==i,dep_cols] )
          ensem.svm[test.bt==1,col.csr] =
            ensem.svm[test.bt==1,col.csr] +
            predict( t, newdata=d[test.bt==1,dep_cols])/num.groups
          colnames( ensem.svm )[col.csr] = paste( "svm_deg", j,"_C",round(k,3),sep="")
          if( name!="") colnames(ensem.svm)[col.csr] = paste(colnames(ensem.svm)[col.csr],"_",name,sep="")
        }
        if( name=="" ) write.csv( file="ensem_svm.csv", ensem.svm )
        if( name!="" ) write.csv( file=paste("ensem_svm_",name,".csv",sep=""), ensem.svm )
      }
      if( type=="Classification" ) col.csr = col.csr + 2
      if( type=="Regression" ) col.csr = col.csr + 1
    }
  }
  return( ensem.svm )
}

svm.ensem.pc = function( d, test.bt, cv.group, ind_col, dep_cols
      ,rsig = 10^seq(-1,1,.25)
      ,polydeg = c(1,2,3)
      ,C.vec = 10^seq(-2,2,.5)
      ,pc.vec
      ,type = "Classification"
      )
{
  p = cbind( d[,ind_col], princomp( d[,dep_cols] )$scores )
  output = matrix(0,nrow=nrow(d), ncol=0 )
  for( i in pc.vec )
  {
    output.temp = svm.ensem( p, test.bt, cv.group, 1, 2:(i+1), rsig, polydeg, C.vec, name=paste("pc",i,sep="") )
    output = cbind(output,output.temp)
  }
  return( output )
}
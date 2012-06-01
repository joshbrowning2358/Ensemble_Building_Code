kknn.ensem	= function( d, test.bt, cv.group, ind_col, dep_cols
      ,kernel     = "triangular"    # Possible choices are "rectangular" (which is standard unweighted knn), "triangular", "epanechnikov", "biweight", "triweight", "cos", "inv" and "gaussian".
      ,comp.vec   = 1:2             # Number of principal components to use
      ,neigh.vec  = 7:10            # Should use more than 1, as they are being distance weighted (and 1 is more efficient in knn )
      ,dist.vec   = 1:2             # This is n, where distance is defined by nth root of (abs(x1)^n + abs(x2)^n + ... ).  Default is 2.
      ,type       = "Classification"
	)
{
  ensem.kknn	= data.frame(matrix( 0, nrow=nrow( d ), ncol=length(comp.vec)*length(neigh.vec)*length(dist.vec)*(1+as.numeric(type=="Classification")) ) )
  colnames(d)[ind_col]	= "y"
  d		= data.frame(scale(d))
  pc		= princomp( d[,dep_cols] )$scores
  if( length( cv.group ) != nrow(d) ) return( "cv.group isn't the right size" )
  d2		= data.frame( cbind(d$y,pc) )
  colnames(d2)[1]	= "y"
  num.groups= length( table( cv.group ) ) - 1
  library( kknn )
  col.csr	= 1
  for( l in dist.vec )
  {
    for( j in comp.vec )
    {
      for( k in neigh.vec )
      {
        for( i in 1:num.groups )
        {
          if( type=="Classification" )
          {
            kknn.train	= kknn( as.factor(y) ~ .
              ,train	= d2[cv.group != i&cv.group>0,1:(j+1)]
              ,test		= d2[cv.group == i,1:(j+1)]
              ,k		= k
              ,distance	= l
              ,kernel	= kernel )
            kknn.test = kknn( as.factor(y) ~ .
              ,train	= d2[cv.group != i&cv.group>0,1:(j+1)]
              ,test		= d2[cv.group == 0,1:(j+1)]
              ,k		= k
              ,distance	= l
              ,kernel	= kernel )
            #First, add on the model with the probabilities:
            ensem.kknn[test.bt==0 & cv.group==i,col.csr] = kknn.train$prob[,2]
            ensem.kknn[test.bt==1,col.csr] =
              ensem.kknn[test.bt==1,col.csr] +
              kknn.test$prob[,2]/num.groups
            colnames( ensem.kknn )[col.csr] = paste( "kknn_prob_neigh", k, "_comp",j,"_dist",l,sep="")
            col.csr	= col.csr + 1
            #Now, add on the model of the predicted factor:
            ensem.kknn[test.bt==0 & cv.group==i,col.csr] =
              as.numeric( kknn.train$fitted.values ) - 1
            ensem.kknn[test.bt==1,col.csr] =
              ensem.kknn[test.bt==1,col.csr] +
              1/num.groups*( as.numeric( kknn.test$fitted.values ) - 1 )
            colnames( ensem.kknn )[col.csr] = paste( "kknn_fac_neigh", k, "_comp",j,"_dist",l,sep="")
            col.csr	= col.csr - 1
          }
          if( type=="Regression" )
          {
            kknn.train  = kknn( y ~ .
              ,train    = d2[cv.group != i&cv.group>0,1:(j+1)]
              ,test     = d2[cv.group == i,1:(j+1)]
              ,k        = k
              ,distance = l
              ,kernel   = kernel )
            kknn.test   = kknn( y ~ .
              ,train    = d2[cv.group != i&cv.group>0,1:(j+1)]
              ,test     = d2[cv.group == 0,1:(j+1)]
              ,k        = k
              ,distance = l
              ,kernel   = kernel )
            #First, add on the model with the probabilities:
            ensem.kknn[test.bt==0 & cv.group==i,col.csr] = kknn.train$fitted.values
            ensem.kknn[test.bt==1,col.csr] =
              ensem.kknn[test.bt==1,col.csr] +
              kknn.test$fitted.values/num.groups
            colnames( ensem.kknn )[col.csr] = paste( "kknn_neigh", k, "_comp",j,"_dist",l,sep="")
          }
          write.csv( file=paste("ensem_kknn_",kernel,".csv", sep=""), ensem.kknn )
          print( paste( "This analysis is ", round(100*(i+(which(neigh.vec==k)-1)*num.groups+num.groups*length(neigh.vec)*(which(comp.vec==j)-1)+num.groups*length(neigh.vec)*length(comp.vec)*(which(dist.vec==l)-1))/(length(dist.vec)*length(neigh.vec)*length(comp.vec)*num.groups)),"% complete",sep="" ) )
        }
        if( type=="Classification" ) col.csr = col.csr + 2
        if( type=="Regression" ) col.csr = col.csr + 1
      }
    }
  }
  return( ensem.kknn )
}
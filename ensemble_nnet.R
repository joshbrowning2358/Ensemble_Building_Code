nn.ensem	= function( d, test.bt, cv.group, ind_col, dep_cols
	,size.vec	= c(2,3,5,8,13,21)
	,maxit.vec	= 2^(1:13)+1
	,name		= ""
	)
{
  ensem.nn	= data.frame(matrix( 0, nrow=nrow( d ), ncol=length(size.vec)*length(maxit.vec)*2 ) )
  size.vec	= sort( size.vec )
  colnames(d)[ind_col]	= "y"
  if( length( cv.group ) != nrow(d) ) return( "cv.group isn't the right size" )
  d		= data.frame( cbind(d[,ind_col], scale(d[,dep_cols]) ) )
  num.groups= length( table( cv.group ) ) - 1
  library( nnet )
  col.csr	= 1
  for( k in maxit.vec )
  {
    for( j in size.vec )
    {
      for( i in 1:num.groups )
      {
        t	= nnet( as.factor(y) ~ .
		, data=d[cv.group != i&cv.group>0,c(ind_col,dep_cols)]
		, size	= j
		, maxit	= k
		, trace	= F )
        #Probability
        ensem.nn[test.bt==0 & cv.group==i,col.csr] = 
		predict( t, newdata=d[cv.group==i,], type="raw" )
        ensem.nn[test.bt==1,col.csr] = 
		ensem.nn[test.bt==1,col.csr] +
		predict( t, newdata=d[test.bt==1,], type="raw" )*1/num.groups
        colnames( ensem.nn )[col.csr] = paste( "nn_prob_maxit", k, "_size",round(j,3),sep="")
        if( name!="") colnames(ensem.nn)[col.csr] = paste(colnames(ensem.nn)[col.csr],"_",name,sep="")
	  col.csr	= col.csr + 1
        #Class
        ensem.nn[test.bt==0 & cv.group==i,col.csr] = 
		as.numeric(predict( t, newdata=d[cv.group==i,], type="class" ))
        ensem.nn[test.bt==1,col.csr] = 
		ensem.nn[test.bt==1,col.csr] +
		as.numeric( predict( t, newdata=d[test.bt==1,], type="class" ))*1/num.groups
        colnames( ensem.nn )[col.csr] = paste( "nn_fac_maxit", k, "_size",j,sep="")
        if( name!="") colnames(ensem.nn)[col.csr] = paste(colnames(ensem.nn)[col.csr],"_",name,sep="")
	  col.csr	= col.csr - 1
        print( paste( "This analysis is ", round(100*(i+(which(size.vec==j)-1)*num.groups+(which(maxit.vec==k)-1)*num.groups*length(size.vec))/(length(maxit.vec)*length(size.vec)*num.groups),1),"% complete",sep="" ) )
        if( name=="" ) write.csv( file=paste("ensem_nn_",proc.time()[[3]],.csv",sep=""), ensem.nn, row.names=F )
        if( name!="" ) write.csv( file=paste("ensem_nn_",name,proc.time()[[3]]".csv",sep=""), ensem.nn, row.names=F )
      }
      col.csr	= col.csr + 2
    }
  }
  return( ensem.nn )
}

nn.ensem.pc	= function( d, test.bt, cv.group, ind_col, dep_cols
	,size.vec	= c(2,3,5,8,13,21)
	,maxit.vec	= 2^(1:13)+1
	,pc.vec
	)
{
  p	= cbind( d[,ind_col], princomp( d[,dep_cols] )$scores )
  output	= matrix(0,nrow=nrow(d), ncol=0 )
  for( i in pc.vec )
  {
    output.temp = nn.ensem( p, test.bt, cv.group, 1, 2:(i+1), size.vec, maxit.vec, name=paste("pc",i,sep="") )
    output	= cbind(output,output.temp)
  }
  return( output )
}
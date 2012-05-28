rf.ensem	= function( d, test.bt, cv.group, ind_col, dep_cols
	,tree.vec	= 2^(1:13)
	#Controls how many variables are sampled for each split:
	,try.vec	= sqrt( ncol( d ) )
	,name		= ""
	)
{
  ensem.rf	= data.frame(matrix( 0, nrow=nrow( d ), ncol=length(tree.vec)*length(try.vec) ) )	###
  tree.vec	= sort( tree.vec )
  colnames(d)[ind_col]	= "y"
  if( length( cv.group ) != nrow(d) ) return( "cv.group isn't the right size" )		###
  d		= data.frame( cbind(d[,ind_col], scale(d[,dep_cols]) ) )
  num.groups= length( table( cv.group ) ) - 1
  library( randomForest )
  col.csr	= 1
  for( j in try.vec )
  {
    for( k in tree.vec )
    {
      for( i in 1:num.groups )
      {
        t	= randomForest( x=d[cv.group != i & cv.group>0,dep_cols]
		, y		= as.factor(d[cv.group != i & cv.group>0, ind_col])
		, n.trees 	= k
		, mtry	= j )
        ensem.rf[cv.group==i,col.csr] =
		predict( t, newdata=d[cv.group==i,dep_cols], type="prob" )[,2]
        ensem.rf[test.bt==1,col.csr] =
		ensem.rf[test.bt==1,col.csr] +
		predict( t, newdata=d[test.bt==1,dep_cols], type="prob" )[,2]*1/num.groups
        colnames( ensem.rf )[col.csr] = paste( "rf_trees", k, "_try",round(j,0),sep="")
        if( name!="") colnames(ensem.rf)[col.csr] = paste(colnames(ensem.rf)[col.csr],"_",name,sep="")
        print( paste( "This analysis is ", round(100*(i+(which(tree.vec==k)-1)*num.groups+num.groups*length(tree.vec)*(which(try.vec==j)-1))/(length(try.vec)*length(tree.vec)*num.groups)),"% complete",sep="" ) )
      }
      if( name=="" ) write.csv( file="ensem_rf.csv", ensem.rf )
      if( name!="" ) write.csv( file=paste("ensem_rf_",name,".csv",sep=""), ensem.rf )
      col.csr	= col.csr + 1
    }
  }
  return( ensem.rf )
}

rf.ensem.pc	= function( d, test.bt, cv.group, ind_col, dep_cols
	,tree.vec	= 2^(1:13)
	#Controls how many variables are sampled for each split:
	,try.vec	= sqrt( ncol( d ) )
	,pc.vec
	)
{
  p	= cbind( d[,ind_col], princomp( d[,dep_cols] )$scores )
  output	= matrix(0,nrow=nrow(d), ncol=0 )
  for( i in pc.vec )
  {
    output.temp = rf.ensem( p, test.bt, cv.group, 1, 2:(i+1), tree.vec, try.vec, name=paste("pc",i,sep="") )
    output	= cbind(output,output.temp)
  }
  return( output )
}
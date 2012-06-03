tree.ensem	= function( d, test.bt, cv.group, ind_col, dep_cols
	,cp.vec	= 10^seq(-2,-5,-.25)
	,buc.vec	= 2^seq(1,9,.5)
	,name		= ""
	)
{
  ensem.tree= data.frame(matrix( 0, nrow=nrow( d ), ncol=length(cp.vec)*length(buc.vec) ) )
  cp.vec	= sort( cp.vec )
  colnames(d)[ind_col]	= "y"
  d		= data.frame( cbind(d[,ind_col], scale(d[,dep_cols]) ) )
  num.groups= length( table( cv.group ) ) - 1
  library( rpart )
  col.csr	= 1
  for( j in buc.vec )
  {
    for( k in cp.vec )
    {
      for( i in 1:num.groups )
      {
        t	= rpart( y ~ ., data=d[cv.group != i & cv.group>0,c(ind_col,dep_cols)]
		, control	= rpart.control( cp=k, minbucket=j, xval=0 ) )
        ensem.tree[test.bt==0 & cv.group==i,col.csr] =
		predict( t, newdata=d[cv.group==i,] )
        ensem.tree[test.bt==1,col.csr] =
		ensem.tree[test.bt==1,col.csr] +
		predict( t, newdata=d[test.bt==1,] )*1/num.groups
        colnames( ensem.tree )[col.csr] = paste( "tree_cp", round(k,3), "_buc",j,sep="")
        if( name!="") colnames(ensem.tree)[col.csr] = paste(colnames(ensem.tree)[col.csr],"_",name,sep="")
        print( paste( "This analysis is ",round(100*(i+num.groups*(which(cp.vec==k)-1)+(which(buc.vec==j)-1)*length(cp.vec)*num.groups)/(num.groups*length(buc.vec)*length(cp.vec) ), 1 ), "% complete", sep="" ) )
      }
      if( name=="" ) write.csv( file=paste("ensem_tree_",proc.time()[[3]],".csv",sep=""), ensem.tree, row.names=F )
      if( name!="" ) write.csv( file=paste("ensem_tree_",name,proc.time()[[3]],".csv",sep=""), ensem.tree, row.names=F )
      col.csr	= col.csr + 1
    }
  }
  return( ensem.tree )
}

opt.tree	= function( t )
{
  if( nrow( t$cptable ) == 1 ) {print("No splits, can't optimize");return(t)}
  xerror	= t$cptable[,4]
  xstd	= t$cptable[,5]
  mincp	= min( xerror ) + xstd[which.min( xerror )]
  print( paste( "Using a cp of ",mincp ) )
  if( mincp <  1 ) minrow = min( (1:length(xerror))[xerror <= mincp] )
  if( mincp >= 1 ) minrow = 1
  return( prune(t, cp=t$cptable[minrow,1] ) )
}

tree.ensem.opt	= function( d, test.bt, cv.group, ind_col, dep_cols
	,buc.vec	= 2^seq(1,9,.5)
	,min.cp	= 0
	,xval		= 5	#Cross validations to determine optimal cp value.  This is different from the number of cross-validations ran to predict (for the model) on all cases.
	,name		= ""
	)
{
  ensem.tree= data.frame(matrix( 0, nrow=nrow( d ), ncol=length(buc.vec) ) )
  colnames(d)[ind_col]	= "y"
  d		= data.frame( cbind(d[,ind_col], scale(d[,dep_cols]) ) )
  num.groups= length( table( cv.group ) ) - 1
  library( rpart )
  col.csr	= 1
  for( j in buc.vec )
  {
    for( i in 1:num.groups )
    {
      t	= rpart( y ~ ., data=d[cv.group != i & cv.group>0,c(ind_col,dep_cols)]
		, control	= rpart.control( cp=min.cp, minbucket=j, xval=xval ) )
      t	= opt.tree( t )
      ensem.tree[test.bt==0 & cv.group==i,col.csr] =
		predict( t, newdata=d[cv.group==i,] )
      ensem.tree[test.bt==1,col.csr] =
		ensem.tree[test.bt==1,col.csr] +
		predict( t, newdata=d[test.bt==1,] )*1/num.groups
      colnames( ensem.tree )[col.csr] = paste( "tree_opt_buc",j,sep="")
      if( name!="") colnames(ensem.tree)[col.csr] = paste(colnames(ensem.tree)[col.csr],"_",name,sep="")
      print( paste( "This analysis is ",round(100*(i+(which(buc.vec==j)-1)*num.groups)/(num.groups*length(buc.vec) ), 1 ), "% complete", sep="" ) )
    }
    if( name=="" ) write.csv( file="ensem_tree_opt.csv", ensem.tree )
    if( name!="" ) write.csv( file=paste("ensem_tree_opt_",name,".csv",sep=""), ensem.tree )
    col.csr	= col.csr + 1
  }
  return( ensem.tree )
}

tree.ensem.pc	= function( d, test.bt, cv.group, ind_col, dep_cols
	,cp.vec	= 10^seq(-2,-5,-.25)
	,buc.vec	= 2^seq(1,9,.5)
	,pc.vec
	)
{
  p	= cbind( d[,ind_col], princomp( d[,dep_cols] )$scores )
  output	= matrix(0,nrow=nrow(d), ncol=0 )
  for( i in pc.vec )
  {
    output.temp = tree.ensem( p, test.bt, cv.group, 1, 2:(i+1), cp.vec, buc.vec, name=paste("pc",i,sep="") )
    output	= cbind(output,output.temp)
  }
  return( output )
}

tree.ensem.opt.pc	= function( d, test.bt, cv.group, ind_col, dep_cols
	,buc.vec	= 2^seq(1,9,.5)
	,min.cp	= 0
	,xval		= 5	#Cross validations to determine optimal cp value.  This is different from the number of cross-validations ran to predict (for the model) on all cases.
	,pc.vec
	)
{
  p	= cbind( d[,ind_col], princomp( d[,dep_cols] )$scores )
  output	= matrix(0,nrow=nrow(d), ncol=0 )
  for( i in pc.vec )
  {
    output.temp = tree.ensem.opt( p, test.bt, cv.group, 1, 2:(i+1), min.cp, xval, name=paste("pc",i,sep="") )
    output	= cbind(output,output.temp)
  }
  return( output )
}

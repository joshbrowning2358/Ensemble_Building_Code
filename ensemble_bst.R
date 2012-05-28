bst.ensem = function( d, test.bt, cv.group, ind_col, dep_cols
      ,tree.vec = 2^(1:13)+1
      ,name = ""
      )
{
  ensem.bst = data.frame(matrix( 0, nrow=nrow( d ), ncol=length(tree.vec) ) )
  tree.vec = sort( tree.vec )
  colnames(d)[ind_col] = "y"
  if( length( cv.group ) != nrow(d) ) return( "cv.group isn't the right size" )
  d = data.frame( cbind(d[,ind_col], scale(d[,dep_cols]) ) )
  num.groups= length( table( cv.group ) ) - 1
  library( gbm )
  col.csr = 1
  for( i in 1:num.groups )
  {
    if( length( table( y ) ) == 2 ) t = gbm( y ~ ., data=d[cv.group != i&cv.group>0,c(ind_col,dep_cols)]
      , n.trees = max(tree.vec), verbose=F )
    if( length( table( y ) ) > 2 ) t = gbm( y ~ ., data=d[cv.group != i&cv.group>0,c(ind_col,dep_cols)]
      , n.trees = max(tree.vec), verbose=F, distribution = "gaussian" )
    for( k in tree.vec )
    {
      ensem.bst[test.bt==0 & cv.group==i,col.csr] =
            predict( t, newdata=d[cv.group==i,], n.trees=k, type="response" )
      ensem.bst[test.bt==1,col.csr] =
            ensem.bst[test.bt==1,col.csr] +
            predict( t, newdata=d[test.bt==1,], n.trees=k, type="response" )*1/num.groups
      colnames( ensem.bst )[col.csr] = paste( "bst_trees", k,sep="")
      if( name!="") colnames(ensem.bst)[col.csr] = paste(colnames(ensem.bst)[col.csr],"_",name,sep="")
            col.csr = col.csr + 1
    }
    col.csr = col.csr - length( tree.vec )
    print( paste( "This analysis is ", round(100*i/num.groups),"% complete",sep="" ) )
    if( name=="" ) write.csv( file="ensem_bst.csv", ensem.bst )
    if( name!="" ) write.csv( file=paste("ensem_bst_",name,".csv",sep=""), ensem.bst )
  }
  return( ensem.bst )
}


bst.ensem.opt = function( d, test.bt, cv.group, ind_col, dep_cols
      ,max.trees = 50000
      ,cv.fold = 5 #Note there are two cross-validations. This corresponds to optimizing the number of trees to use. The other cross-validation allows us to estimate predicted values for all training data.
      ,name = ""
      )
{
  ensem.bst = data.frame(matrix( 0, nrow=nrow( d ), ncol=1 ) )
  if( length( cv.group ) != nrow(d) ) return( "cv.group isn't the right size" )
  d = data.frame( cbind(d[,ind_col], scale(d[,dep_cols]) ) )
  colnames(d)[1] = "y"
  num.groups= length( table( cv.group ) ) - 1
  library( gbm )
  for( i in 1:num.groups )
  {
    if( length( table( d$y ) ) == 2 ) t = gbm( y ~ ., data=d[cv.group != i&cv.group>0,c(ind_col,dep_cols)]
      , n.trees = max.trees, verbose=F, cv.folds=cv.fold )
    if( length( table( d$y ) ) > 2 ) t = gbm( y ~ ., data=d[cv.group != i&cv.group>0,c(ind_col,dep_cols)]
      , n.trees = max.trees, verbose=F, distribution = "gaussian", cv.folds=cv.fold )
    opt.tree = which.min( t$cv.error )
    print( paste( "For cv.group",i,"using",opt.tree,"trees." ) )
    ensem.bst[test.bt==0 & cv.group==i,1] =
      predict( t, newdata=d[cv.group==i,], n.trees=opt.tree, type="response" )
    ensem.bst[test.bt==1,1] =
      ensem.bst[test.bt==1,1] +
      predict( t, newdata=d[test.bt==1,], n.trees=opt.tree, type="response" )*1/num.groups
    colnames( ensem.bst ) = paste( "bst_opt_trees", opt.tree,"_max",max.trees,sep="")
    if( name!="") colnames(ensem.bst) = paste(colnames(ensem.bst),"_",name,sep="")
    print( paste( "This analysis is ", round(100*i/num.groups),"% complete",sep="" ) )
  }
  if( name=="" ) write.csv( file="ensem_bst_opt.csv", ensem.bst )
  if( name!="" ) write.csv( file=paste("ensem_bst_opt_",name,".csv",sep=""), ensem.bst )
  return( ensem.bst )
}

bst.ensem.pc = function( d, test.bt, cv.group, ind_col, dep_cols
      ,tree.vec = 2^(10:13)+1
      ,pc.vec
      )
{
  p = cbind( d[,ind_col], princomp( d[,dep_cols] )$scores )
  output = matrix(0,nrow=nrow(d), ncol=0 )
  for( i in pc.vec )
  {
    output.temp = bst.ensem( p, test.bt, cv.group, 1, 2:(i+1), tree.vec, name=paste("pc",i,sep="") )
    output = cbind(output,output.temp)
  }
  return( output )
}

bst.ensem.opt.pc = function( d, test.bt, cv.group, ind_col, dep_cols
      ,max.trees = 50000
      ,cv.fold = 5
      ,pc.vec
      )
{
  p = cbind( d[,ind_col], princomp( d[,dep_cols] )$scores )
  output = matrix(0,nrow=nrow(d), ncol=0 )
  for( i in pc.vec )
  {
    output.temp = bst.ensem.opt( p, test.bt, cv.group, 1, 2:(i+1), max.trees, cv.fold, name=paste("pc",i,sep="") )
    output = cbind(output,output.temp)
  }
  return( output )
}
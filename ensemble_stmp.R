stmp.ensem	= function( d, test.bt, cv.group, ind_col, dep_cols
	,tree.vec	= floor(2^c(1,2,3,seq(3.5,16,.25))/2)*2+1
	,name		= ""
	)
{
  ensem.stmp= data.frame(matrix( 0, nrow=nrow( d ), ncol=length(tree.vec)*2 ) )
  tree.vec	= sort( tree.vec )
  colnames(d)[ind_col]	= "y"
  if( length( cv.group ) != nrow(d) ) return( "cv.group isn't the right size" )
  d		= data.frame( cbind(d[,ind_col], scale(d[,dep_cols]) ) )
  num.groups= length( table( cv.group ) ) - 1
  library( caTools )
  for( i in 1:num.groups )
  {
    col.csr	= 1
    t		= LogitBoost( xlearn=d[cv.group != i & cv.group>0,dep_cols]
	,ylearn	= d[cv.group != i & cv.group>0,ind_col]
	,nIter	= max(tree.vec) )
    for( j in tree.vec )
    {
      #Class
      ensem.stmp[test.bt==0 & cv.group==i,col.csr] = 
		predict( t, xtest=d[cv.group==i,dep_cols], nIter=1 )
      ensem.stmp[test.bt==1,col.csr] = 
		ensem.stmp[test.bt==1,col.csr] + 
		(predict( t, xtest=d[test.bt==1,], nIter=j))/num.groups
      print( predict( t, xtest=d[cv.group==1,]) )
      colnames( ensem.stmp )[col.csr] = paste( "stmp_fac_tree", j,sep="")
      if( name!="") colnames(ensem.stmp)[col.csr] = paste(colnames(ensem.stmp)[col.csr],"_",name,sep="")
	col.csr	= col.csr + 1
      #Probabilities
      ensem.stmp[test.bt==0 & cv.group==i,col.csr] = 
		predict( t, xtest=d[cv.group==i,dep_cols], type="raw", nIter=j )[,2]
      ensem.stmp[test.bt==1,col.csr] = 
		ensem.stmp[test.bt==1,col.csr] + 
		(predict( t, xtest=d[test.bt==1,], type="raw", nIter=j )[,2])/num.groups
      colnames( ensem.stmp )[col.csr] = paste( "stmp_prob_tree", j,sep="")
      if( name!="") colnames(ensem.stmp)[col.csr] = paste(colnames(ensem.stmp)[col.csr],"_",name,sep="")
	col.csr	= col.csr + 1
      print( paste( "This analysis is ", round(100*(which(tree.vec==j)+(i-1)*length(tree.vec))/(length(tree.vec)*num.groups)),"% complete",sep="" ) )
    }
  }
  if( name=="" ) write.csv( file=paste("ensem_stmp_",proc.time()[[3]],".csv",sep=""), ensem.stmp, row.names=F )
  if( name!="" ) write.csv( file=paste("ensem_stmp_",name,,proc.time()[[3]]".csv",sep=""), ensem.stmp, row.names=F )
  return( ensem.stmp )
}

stmp.ensem.pc	= function( d, test.bt, cv.group, ind_col, dep_cols
	,tree.vec	= floor(2^c(1,2,3,seq(3.5,16,.25))/2)*2+1
	,pc.vec
	)
{
  p	= cbind( d[,ind_col], princomp( d[,dep_cols] )$scores )
  output	= matrix(0,nrow=nrow(d), ncol=0 )
  for( i in pc.vec )
  {
    output.temp = stmp.ensem( p, test.bt, cv.group, 1, 2:(i+1), tree.vec, name=paste("pc",i,sep="") )
    output	= cbind(output,output.temp)
  }
  return( output )
}
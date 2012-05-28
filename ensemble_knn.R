knn.ensem	= function( d, test.bt, cv.group, ind_col, dep_cols
	,comp.vec = 1:10
	,neigh.vec= 1:10 )
{
  ensem.knn	= data.frame(matrix( 0, nrow=nrow( d ), ncol=length(comp.vec)*length(neigh.vec)*2 ) )
  colnames(d)[ind_col]	= "y"
  pc		= princomp( d[,dep_cols] )$scores
  if( length( cv.group ) != nrow(d) ) return( "cv.group isn't the right size" )
  d		= data.frame( cbind(d[,ind_col], scale(d[,dep_cols]) ) )
  num.groups= length( table( cv.group ) ) - 1
  library( ipred )
  col.csr	= 1
  for( k in neigh.vec )
  {
    for( j in comp.vec )
    {
      for( i in 1:num.groups )
      {
        knn.train = knn( train= as.matrix(pc[cv.group != i&cv.group>0,1:j])
		,test	= as.matrix(pc[cv.group == i,1:j])
		,cl	= as.factor( d[cv.group != i & cv.group>0,ind_col] )
		,k	= k
		,prob	= T )
        ensem.knn[test.bt==0 & cv.group==i,col.csr] =
		(as.numeric( knn.train ) -1 ) * attributes( knn.train )$prob
        knn.test = knn( train= as.matrix(pc[cv.group != i&cv.group>0,1:j])
		,test	= as.matrix(pc[cv.group == 0,1:j])
		,cl	= as.factor( d[cv.group != i & cv.group>0,ind_col] )
		,k	= k
		,prob	= T )
        ensem.knn[test.bt==1,col.csr] =
		ensem.knn[test.bt==1,col.csr] +
		(as.numeric( knn.test ) -1 ) * attributes( knn.test )$prob/num.groups
        colnames( ensem.knn )[col.csr] = paste( "knn_prob_neigh", k, "_comp",j,sep="")
	  col.csr	= col.csr + 1
        #Now, add on the model of the predicted factor:
        ensem.knn[test.bt==0 & cv.group==i,col.csr] =
		as.numeric( knn( train= as.matrix(pc[cv.group != i&cv.group>0,1:j])
		,test	= as.matrix(pc[cv.group == i,1:j])
		,cl	= as.factor( d[cv.group != i & cv.group>0,ind_col] )
		,k	= k ) ) - 1
        ensem.knn[test.bt==1,col.csr] =
		ensem.knn[test.bt==1,col.csr] +
		1/num.groups*as.numeric( knn( train= as.matrix(pc[cv.group != i&cv.group>0,1:j])
		,test	= as.matrix(pc[cv.group == 0,1:j])
		,cl	= as.factor( d[cv.group != i & cv.group>0,ind_col] )
		,k	= k ) ) - 1/num.groups
        colnames( ensem.knn )[col.csr] = paste( "knn_fac_neigh", k, "_comp",j,sep="")
	  col.csr	= col.csr - 1
        print( paste( "This analysis is ", round(100*(i+(which(comp.vec==j)-1)*num.groups+length(comp.vec)*num.groups*(which(neigh.vec==k)-1))/(length(neigh.vec)*length(comp.vec)*num.groups)),"% complete",sep="" ) )
        write.csv( file="ensem_knn.csv", ensem.knn )
      }
	print( col.csr )
      col.csr	= col.csr + 2
    }
  }
  return( ensem.knn )
}
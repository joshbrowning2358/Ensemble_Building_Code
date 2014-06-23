setwd( "~/Desktop/Insult Detection" )
source( "~/Desktop/Insult Detection/Ensemble Functions/auc.R" )

eps.round		= function( p, epsilon=.01 )
{
  min.e	= function(x) { max(x,epsilon) }
  max.e	= function(x) { min(x,1-epsilon) }
  return( sapply( sapply( p, min.e ), max.e ) )
}

num.iters	= 100
top.N		= 10
prune		= .7
base.loss	= .5
pred.rows	= 1:sum( dFinal$Insult >-1 )
#epsilon	= .01

ensem			= read.csv( file="~/Desktop/Insult Detection/ensem_ccr.csv", sep=";" )
ensem			= cbind( ensem, read.csv( file="~/Desktop/Insult Detection/ensem_rf.csv", sep=";" ) )
ensem			= cbind( ensem, read.csv( file="~/Desktop/Insult Detection/ensem_svm.csv", sep=";" ) )
ensem			= cbind( ensem, read.csv( file="~/Desktop/Insult Detection/ensem_lin.csv", sep=";" ) )
ensem			= cbind( ensem, read.csv( file="~/Desktop/Insult Detection/ensem_bst.csv", sep=";" ) )

#ensem			= cbind( ensem, read.csv( file="~/Desktop/Insult Detection/ensem_lin.csv", sep="," ) )

#  ensem	= apply( ensem, 2, eps.round )
  best.ll.vec	= rep(NA,num.iters)
  weights	= rep("",num.iters)

  #To avoid overfitting, prune the bottom end of the models (and anything with a worse than guessing metric).
  loss.vec	= rep(1,ncol(ensem))
  for( i in 1:ncol( ensem ) )
  {
    loss.vec[i]	= auc( ensem[pred.rows,i] , dFinal[pred.rows,]$Insult, iter=3000 )[[1]]
  }
  loss.vec = as.numeric( loss.vec )
  plot( loss.vec )
  ensem		= ensem[,loss.vec>=base.loss]
  loss.vec	= loss.vec[loss.vec>=base.loss]
  ensem		= ensem[,rank(loss.vec)>=length(loss.vec)*prune]
  loss.vec	= loss.vec[rank(loss.vec)>=length(loss.vec)*prune]
  plot( loss.vec )
  final		= apply( ensem[,rank(loss.vec)<=top.N], 1, mean )	#Can let in more than top.N, depending on rank
  weights	= c(colnames(ensem)[rank(loss.vec)<=top.N],weights)
  best.ll.vec	= c(rep(auc(final[pred.rows],dFinal[pred.rows,]$Insult)[[1]],top.N),best.ll.vec)
  top.N		= sum( rank(loss.vec)<=top.N )
  best.loss	= auc( final[pred.rows], dFinal[pred.rows,]$Insult, iter=3000 )[[1]]
  for( i in 1:num.iters )
  {
    for( j in 1:ncol( ensem ) )
    {
      final.temp	= final*(i-1)/i + ensem[,j]/i
      loss.temp		= auc( final.temp[pred.rows], dFinal[pred.rows,]$Insult, iter=3000 )[[1]]
      if( loss.temp > best.loss )
      {
        best.loss		= loss.temp
        final.save		= final.temp
        weights[i+top.N]	= colnames(ensem)[j]
        best.ll.vec[i+top.N]	= best.loss
      }
    }
    final		= final.save
  }
  plot( best.ll.vec )
#}

write.csv( file="ensem_20120913.csv", final[dFinal$Insult==-1], row.names=F )
save.image()

randArgs <-
function(optArgs){
  out = list()
  for(i in 1:length(optArgs)){
    vals = optArgs[[i]][[3]]
    if(optArgs[[i]][[2]]=="numeric")
      val = runif(1, min=vals[1], max=vals[2])
    if(optArgs[[i]][[2]]=="ordered")
      val = round(runif(1, min=vals[1]-.5, max=vals[2]+.5))
    if(optArgs[[i]][[2]]=="character")
      val = sample(vals, size=1)
    out[[length(out)+1]] = val
    names(out)[length(out)] = optArgs[[i]][[1]]
  }
  return(out)
}

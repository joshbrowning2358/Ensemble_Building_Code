makeOutput = function(preds, call)
{
  #Data quality checks
  if(!is(call,"character"))
    stop("!is(fname,'character')")
  if(splitByGroup & length(modelNo)!=800000)
    stop("splitByGroup & length(modelNo)!=800000.  To split by group we need modelNo!")
  if( is.matrix(preds) | is.data.frame(preds) )
    stop("is.matrix(preds) | is.data.frame(preds).  Ensure preds is a numeric vector!")
  if(splitByGroup & !is(preds,"numeric"))
    stop("splitByGroup & !is(preds,'numeric').  preds must be numeric to use splitByGroup!")
  if(!"Submissions" %in% list.files() )
    stop("No submissions directory!")

  files = list.files("Submissions")
  files = files[grepl("_raw", files)]
  ids = as.numeric( gsub("_raw.csv", "", files) )
  ids = ids[!is.na(ids)]
  newId = min(ids,0)+1
  
  write.csv(preds, paste0("Submissions/",newId,"_raw.csv"), row.names=F)
  #Insert code to produce the correctly formatted output file
  if("desc.csv" %in% list.files("Submissions") ){
    desc = read.csv("Submissions/desc.csv", stringsAsFactors=F)
    desc = rbind( desc, data.frame(id, call) )
  } else {
    desc = data.frame(id, call)
  }
  write.csv(desc, file="Submissions/desc.csv")
}

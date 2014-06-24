makeOutput = function(preds, call)
{
  #Data quality checks
  if(!is(call,"character"))
    stop("!is(fname,'character')")
  if( is.matrix(preds) | is.data.frame(preds) )
    stop("is.matrix(preds) | is.data.frame(preds).  Ensure preds is a numeric vector!")
  if(!"Submissions" %in% list.files() )
    stop("No submissions directory!")

  files = list.files("Submissions")
  files = files[grepl("_raw", files)]
  ids = as.numeric( gsub("_raw.csv", "", files) )
  ids = ids[!is.na(ids)]
  newId = max(ids,0)+1
  
  write.csv(preds, paste0("Submissions/",newId,"_raw.csv"), row.names=F)
  #Insert code to produce the correctly formatted output file
  if("desc.csv" %in% list.files("Submissions") ){
    desc = read.csv("Submissions/desc.csv", stringsAsFactors=F)
    desc = rbind( desc, data.frame(newId, call) )
  } else {
    desc = data.frame(newId, call)
  }
  write.csv(desc, file="Submissions/desc.csv", row.names=F)
}

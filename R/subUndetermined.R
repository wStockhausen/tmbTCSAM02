subUndetermined<-function(x){
  #cat("in subUndetermined\n")
  xp <- x;
  idx<-grep("ALL_",xp);
  xp[idx]<-"UNDETERMINED";
  xp<-gsub("_"," ",xp,fixed=TRUE);
  return(xp);
}

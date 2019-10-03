#'
#' @title Read model growth data file for a TCSAM02 model run
#'
#' @description Function to read model growth data file for a TCSAM02 model run.
#'
#' @param fn - file name for model growth data file
#' @param verbose - flag to print info
#'
#' @return list with growth data set
#'
#' @details None.
#'
#' @export
#'
readTCSAM_GrowthData<-function(conn){
  if (!inherits(conn,"connection")){
    if (!file.exists(conn)){
      msg<-paste0("--Error reading TCSAM growth data file.\n",
                  "--File '",conn,"' does not exist. Aborting!\n\n")
      stop(msg);
    }
    fn<-conn;
    cat(paste0("--Reading model growth data file '",fn,"'\n"))
    conn<-file(fn,open="r");
  } else {
    if (!isOpen(conn)) open(conn,open="r");
  }

  #--read file
  lst<-list();
  strv<-readLines(conn);
  close(conn);
  res  <-wtsUtilities::parseText(strv);
  k<-1;
  if (res[[k]]!="GROWTH_DATA"){
    msg<-paste0("----Error reading growth data file '",fn,"'\n",
                "----Expected keyword 'GROWTH_DATA', but got '",res[[k]],"\n");
    warning(msg,immediate.=TRUE);
  }
  k<-k+1;
  lst[["name"]]<-res[[k]]; k<-k+1;
  lst[["likeType"]]<-res[[k]]; k<-k+1;
  lst[["likeWgt"]] <-as.numeric(res[[k]]); k<-k+1;
  lst[["nXs"]]     <-as.integer(res[[k]]); k<-k+1;
  dfr<-NULL;
  for (i in 1:lst[["nXs"]]){
    x<-res[[k]]; k<-k+1;
    nObs<-as.integer(res[[k]]); k<-k+1;
    yr <-integer(nObs);
    pre<-numeric(nObs);
    pst<-numeric(nObs);
    for (j in 1:nObs){
      obs<-res[[k]]; k<-k+1;
      yr[j] <-as.integer(obs[1]);
      pre[j]<-as.numeric(obs[2]);
      pst[j]<-as.numeric(obs[3]);
    }
    dfr<-rbind(dfr,data.frame(x=rep(x,length.out=nObs),y=yr,pre=pre,pst=pst,stringsAsFactors=FALSE));
  }
  lst[["data"]]<-dfr;

  return(lst)
}

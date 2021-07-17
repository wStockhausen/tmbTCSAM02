#'
#' @title Read model maturity ogive data file for a TCSAM02 model run
#'
#' @description Function to read model maturity ogive data file for a TCSAM02 model run.
#'
#' @param fn - file name for model maturity ogive data file
#' @param verbose - flag to print info
#'
#' @return list with maturity ogive data set
#'
#' @details None.
#'
#' @export
#'
readTCSAM_MaturityOgiveData<-function(conn){
  if (!inherits(conn,"connection")){
    if (!file.exists(conn)){
      msg<-paste0("--Error reading TCSAM maturity ogive data file.\n",
                  "--File '",conn,"' does not exist. Aborting!\n\n")
      stop(msg);
    }
    fn<-conn;
    cat(paste0("--Reading model maturity ogive data file '",fn,"'\n"))
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
  if (res[[k]]!="MATURITYOGIVE_DATA"){
    msg<-paste0("----Error reading growth data file '",fn,"'\n",
                "----Expected keyword 'GROWTH_DATA', but got '",res[[k]],"\n");
    warning(msg,immediate.=TRUE);
  }
  k<-k+1;
  lst[["name"]]  <-res[[k]]; k<-k+1;
  lst[["survey"]]<-res[[k]]; k<-k+1;
  lst[["sex"]]   <-res[[k]]; k<-k+1;
  lst[["likeType"]]<-res[[k]]; k<-k+1;
  lst[["likeWgt"]] <-as.numeric(res[[k]]); k<-k+1;
  lst[["nZBs"]]    <-as.integer(res[[k]]); k<-k+1;
  lst[["zCs"]]     <-as.numeric(res[[k]]); k<-k+1;
  lst[["zBs"]]     <-(lst[["zCs"]][2:lst[["nZBs"]]]+lst[["zCs"]][1:(lst[["nZBs"]]-1)])/2.0;
  lst[["nObs"]]    <-as.integer(res[[k]]); k<-k+1;
  nObs<-lst[["nObs"]];
  yr <-integer(nObs);
  sz <-numeric(nObs);
  idx<-integer(nObs);
  N  <-numeric(nObs);
  prM<-numeric(nObs);
  for (j in 1:nObs){
    obs<-res[[k]]; k<-k+1;
    yr[j] <-as.integer(obs[1]);
    sz[j] <-as.numeric(obs[2]);
    idx[j]<-as.integer(obs[3]);
    N[j]  <-as.numeric(obs[4]);
    prM[j]<-as.numeric(obs[5]);
  }
  dfr<-data.frame(x=rep(lst[["sex"]],length.out=nObs),y=yr,sz=sz,idx=idx,N=N,prM=prM,stringsAsFactors=FALSE);
  dfr$x<-subUndetermined(dfr$x);
  lst[["data"]]<-dfr;

  return(lst)
}

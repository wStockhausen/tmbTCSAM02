#'
#' @title Extract size composition data for a TCSAM02 model run from a list of parsed text
#'
#' @description Function to extract size composition data for a TCSAM02 model run from a list of parsed text.
#'
#' @param res - list of parsed text from a model fleet data file
#' @param ks - starting index into \code{res}
#' @param verbose - flag to print info
#'
#' @return list with size composition data
#'
#' @details None.
#'
#' @export
#'
extractTCSAM_ZCsCatchData<-function(res,
                                    ks,
                                    verbose=FALSE){
  lst<-list();
  k<-ks;
  kw<-res[[k]]; k<-k+1;
  if (!(kw %in% .KEYWORDS[["SIZE_FREQUENCY_DATA_TYPES"]])){
    msg<-paste0("----Error extracting size composition data.\n",
                "----Expected one of the following keywords:\n",
                "----",paste0("'",.KEYWORDS[["SIZE_FREQUENCY_DATA_TYPES"]],"'",collapse=", "),"\n",
                "----but got '",kw,"\n");
    warning(msg,immediate.=TRUE);
    return(NULL);
  }
  lst[["data_type"]]<-kw;
  lst[["fit_type"]] <-res[[k]]; k<-k+1;
  lst[["like_type"]]<-res[[k]]; k<-k+1;
  lst[["like_wgt"]] <-as.numeric(res[[k]]); k<-k+1;
  lst[["tc"]]       <-as.numeric(res[[k]]); k<-k+1;#--tail compression
  lst[["nY"]]       <-as.integer(res[[k]]); k<-k+1;#--number of years of data
  lst[["units"]]    <-res[[k]]; k<-k+1;
  lst[["nZCs"]]     <-as.integer(res[[k]]); k<-k+1;
  lst[["zCs"]]      <-as.numeric(res[[k]]); k<-k+1;
  lst[["zBs"]]      <-(lst[["zCs"]][2:lst[["nZCs"]]]+lst[["zCs"]][1:(lst[["nZCs"]]-1)])/2.0;
  lst[["nFCs"]]     <-as.integer(res[[k]]); k<-k+1;
  #loop over factor combinations
  nZBs<-length(lst[["zBs"]]);
  dfr<-NULL;
  for (i in 1:lst[["nFCs"]]){
    fc<-res[[k]]; k<-k+1;
    for (j in 1:lst[["nY"]]){
      rw<-res[[k]]; k<-k+1;
      us<-as.integer(rw[1]);
      dm<-as.integer(rw[1]);
      yr<-as.integer(rw[1]);
      ss<-as.numeric(rw[2]);
      vls<-as.numeric(rw[3:length(rw)]);
      dfr<-rbind(dfr,
                 data.frame(x=rep(fc[1],nZBs),
                            m=rep(fc[2],nZBs),
                            s=rep(fc[3],nZBs),
                            z=lst[["zBs"]],
                            use=rep(us,nZBs),
                            dm =rep(dm,nZBs),
                            y  =rep(yr,nZBs),
                            ss =rep(ss,nZBs),
                            val=vls,
                            stringsAsFactors=FALSE));
    }#--j
  }#--i
  dfr$x<-subUndetermined(dfr$x);
  dfr$m<-subUndetermined(dfr$m);
  dfr$s<-subUndetermined(dfr$s);
  lst[["data"]]<-dfr;
  return(list(k=k,lst=lst));
}

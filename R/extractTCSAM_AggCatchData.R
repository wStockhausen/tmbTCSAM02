#'
#' @title Extract aggregated abundance or biomass catch data for a TCSAM02 model run from a list of parsed text
#'
#' @description Function to extract aggregated abundance or biomass catch data for a TCSAM02 model run from a list of parsed text.
#'
#' @param res - list of parsed text from a model fleet data file
#' @param ks - starting index into \code{res}
#' @param verbose - flag to print info
#'
#' @return list with aggregated abundance or biomass catch data
#'
#' @details None.
#'
#' @export
#'
extractTCSAM_AggCatchData<-function(res,
                                    ks,
                                    verbose=FALSE){
  lst<-list();
  k<-ks;
  kw<-res[[k]]; k<-k+1;
  if (!(kw %in% .KEYWORDS[["AGGREGATED_CATCH_DATA_TYPES"]])){
    msg<-paste0("----Error extracting aggregate catch data\n",
                "----Expected one of the following keywords:\n",
                "----",paste0("'",.KEYWORDS[["AGGREGATED_CATCH_DATA_TYPES"]],"'",collapse=", "),"\n",
                "----but got '",kw,"\n");
    warning(msg,immediate.=TRUE);
    return(NULL);
  }
  lst[["data_type"]]<-kw;
  lst[["fit_type"]] <-res[[k]]; k<-k+1;
  lst[["like_type"]]<-res[[k]]; k<-k+1;
  lst[["like_wgt"]] <-as.numeric(res[[k]]); k<-k+1;
  lst[["nY"]]       <-as.integer(res[[k]]); k<-k+1;
  lst[["units"]]    <-res[[k]]; k<-k+1;
  lst[["nFCs"]]     <-as.integer(res[[k]]); k<-k+1;
  #loop over factor combinations
  dfr<-NULL;
  for (i in 1:lst[["nFCs"]]){
    fc<-res[[k]]; k<-k+1;
    us<-integer(lst[["nY"]]);
    yr<-integer(lst[["nY"]]);
    vl<-numeric(lst[["nY"]]);
    cv<-numeric(lst[["nY"]]);
    for (j in 1:lst[["nY"]]){
      rw<-res[[k]]; k<-k+1;
      us[j]<-as.integer(rw[1]);
      yr[j]<-as.integer(rw[2]);
      vl[j]<-as.numeric(rw[3]);
      cv[j]<-as.numeric(rw[4]);
    }#--j
    dfr<-rbind(dfr,
               data.frame(x=rep(fc[1],lst[["nY"]]),
                          m=rep(fc[2],lst[["nY"]]),
                          s=rep(fc[3],lst[["nY"]]),
                          use=us,
                          y=yr,
                          val=vl,
                          cv=cv,
                          stringsAsFactors=FALSE));
  }#--i
  dfr$x<-subUndetermined(dfr$x);
  dfr$m<-subUndetermined(dfr$m);
  dfr$s<-subUndetermined(dfr$s);
  lst[["data"]]<-dfr;
  return(list(k=k,lst=lst));
}

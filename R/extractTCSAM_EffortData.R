#'
#' @title Extract catch effort data for a TCSAM02 model run from a list of parsed text
#'
#' @description Function to extract catch effort data for a TCSAM02 model run from a list of parsed text.
#'
#' @param res - list of parsed text from a model fleet data file
#' @param ks - starting index into \code{res}
#' @param verbose - flag to print info
#'
#' @return list with catch effort data
#'
#' @details None.
#'
#' @export
#'
extractTCSAM_EffortData<-function(res,
                                  ks,
                                  verbose=FALSE){
  lst<-list();
  k<-ks;
  kw<-res[[k]]; k<-k+1;
  if (!(kw %in% .KEYWORDS[["EFFORT_DATA_TYPES"]])){
    msg<-paste0("----Error extracting effort catch data\n",
                "----Expected one of the following keywords:\n",
                "----",paste0("'",.KEYWORDS[["EFFORT_DATA_TYPES"]],"'",collapse=", "),"\n",
                "----but got '",kw,"\n");
    warning(msg,immediate.=TRUE);
    return(NULL);
  }
  lst[["data_type"]]<-kw;
  lst[["interval"]] <-parseTimeBlock(res[[k]]); k<-k+1;
  lst[["like_type"]]<-res[[k]]; k<-k+1;
  lst[["like_wgt"]] <-as.numeric(res[[k]]); k<-k+1;
  lst[["units"]]    <-res[[k]]; k<-k+1;
  lst[["nY"]]       <-as.integer(res[[k]]); k<-k+1;

    yr<-integer(lst[["nY"]]);
    vl<-numeric(lst[["nY"]]);
    cv<-numeric(lst[["nY"]]);
    for (j in 1:lst[["nY"]]){
      rw<-res[[k]]; k<-k+1;
      yr[j]<-as.integer(rw[1]);
      vl[j]<-as.numeric(rw[2]);
    }#--j
  lst[["data"]]<-data.frame(y=yr,val=vl);
  return(list(k=k,lst=lst));
}

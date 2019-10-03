#'
#' @title Extract model catch data for a TCSAM02 model run from a list of parsed text
#'
#' @description Function to extract model catch data for a TCSAM02 model run from a list of parsed text.
#'
#' @param res - list of parsed text from model fleet data file
#' @param ks - starting index into \code{res}
#' @param verbose - flag to print info
#'
#' @return list with catch data
#'
#' @details None.
#'
#' @export
#'
extractTCSAM_CatchData<-function(res,
                                 ks,
                                 verbose=FALSE){
  lst<-list();
  k<-ks;
  kw<-res[[k]]; k<-k+1;
  if (kw != "CATCH_DATA"){
    msg<-paste0("----Error extracting catch data\n",
                "----Expected 'CATCH_DATA' but got '",kw,"'\n");
    warning(msg,immediate.=TRUE);
    return(NULL);
  }
  lst[["has_abundance"]]<-as.logical(res[[k]]); k<-k+1;
  lst[["has_biomass"]]  <-as.logical(res[[k]]); k<-k+1;
  lst[["has_zcs"]]      <-as.logical(res[[k]]); k<-k+1;

  #--
  if (lst[["has_abundance"]]){
    tmp1<-extractTCSAM_AggCatchData(res,k);
    if (is.null(tmp1)){
    } else {
      k<-tmp1$k;
      lst[["abundance"]]<-tmp1$lst;
    }
  }
  if (lst[["has_biomass"]]){
    tmp1<-extractTCSAM_AggCatchData(res,k);
    if (is.null(tmp1)){
    } else {
      k<-tmp1$k;
      lst[["biomass"]]<-tmp1$lst;
    }
  }
  if (lst[["has_zcs"]]){
    tmp1<-extractTCSAM_ZCsCatchData(res,k);
    if (is.null(tmp1)){
    } else {
      k<-tmp1$k;
      lst[["zcs"]]<-tmp1$lst;
    }
  }

  return(list(k=k,lst=lst));
}

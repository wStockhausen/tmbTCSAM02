#'
#' @title Extract bounded parameters info from a parsed file for a TCSAM02 model run
#'
#' @description Function to extract bounded parameters info from a parsed file for a TCSAM02 model run.
#'
#' @param res - list with parsed file results
#' @param ks - start location index
#' @param verbose - flag to print info
#'
#' @return list with elements
#' k - index of next element to parse
#' lst - list of model parameters info for recruitment parameters
#'
#' @details None.
#'
#' @export
#'
extractTCSAM_BoundedParameterInfo<-function(res,
                                            ks,
                                            param){
  lst<-list();
  k<-ks;
  kw<-res[[k]]; k<-k+1;
  if (kw != param){
    msg<-paste0("----Error extracting parameter info.\n",
                "----Expected keyword '",param,"' but got '",kw,"'.\n");
    stop(msg,immediate.=TRUE);
    return(NULL);
  }
  lst[["nPVs"]]<-as.integer(res[[k]]); k<-k+1;
  if (lst[["nPVs"]]>0){
    pvs<-list();
    for (i in 1:lst[["nPVs"]]){
      pv<-res[[k]]; k<-k+1;
      pvs[[i]]<-list(id      =as.integer(pv[1]),
                     jitter  =pv[2],
                     lower   =as.numeric(pv[3]),
                     upper   =as.numeric(pv[4]),
                     value   =as.numeric(pv[5]),
                     scale   =pv[6],
                     phase   =as.integer(pv[7]),
                     resample=pv[8],
                     prior   =extractTCSAM_PriorInfo(pv,9),
                     label   =pv[length(pv)]);
    }
    lst[["pvs"]]<-pvs;
  }

  return(list(k=k,lst=lst));
}

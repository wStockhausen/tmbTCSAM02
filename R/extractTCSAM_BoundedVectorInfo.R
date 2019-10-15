#'
#' @title Extract bounded vector parameters info from a parsed file for a TCSAM02 model run
#'
#' @description Function to extract bounded vector parameters info from a parsed file for a TCSAM02 model run.
#'
#' @param res - list with parsed file results
#' @param ks - start location index
#' @param verbose - flag to print info
#'
#' @return list with elements
#' k - index of next element to parse
#' lst - list of model vector parameters info
#'
#' @details None.
#'
#' @export
#'
extractTCSAM_BoundedVectorInfo<-function(res,
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
      pvs[[i]]<-list(id         =as.integer(pv[1]),
                     block_type =pv[2],
                     block      =pv[3],
                     read_vals  =as.logical(pv[4]),
                     jitter     =pv[5],
                     lower      =as.numeric(pv[6]),
                     upper      =as.numeric(pv[7]),
                     value      =as.numeric(pv[8]),
                     scale      =pv[9],
                     phase      =as.integer(pv[10]),
                     resample   =pv[11],
                     prior      =extractTCSAM_PriorInfo(pv,12),
                     label      =pv[length(pv)],
                     values  = NULL);
    }
    for (i in 1:lst[["nPVs"]]){
      if (pvs[[i]]$read_vals){
        pvs[[i]]$values<-as.numeric(res[[k]]); k<-k+1;
      }
    }
    lst[["pvs"]]<-pvs;
  }

  return(list(k=k,lst=lst));
}

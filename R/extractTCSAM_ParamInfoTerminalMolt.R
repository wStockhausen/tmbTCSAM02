#'
#' @title Extract model terminal molt parameters info from a parsed file for a TCSAM02 model run
#'
#' @description Function to extract model terminal molt parameters info from a parsed file for a TCSAM02 model run.
#'
#' @param res - list with parsed file results
#' @param ks - start location index
#' @param verbose - flag to print info
#'
#' @return list with elements
#' k - index of next element to parse
#' lst - list of model parameters info for terminal molt parameters
#'
#' @details None.
#'
#' @export
#'
extractTCSAM_ParamInfoTerminalMolt<-function(res,
                                            ks,
                                            verbose=FALSE){
  lst<-list();
  k<-ks;
  kw<-res[[k]]; k<-k+1;
  if (kw !="molt_to_maturity"){
    msg<-paste0("----Error extracting terminal molt parameters info.\n",
                "----Expected keyword 'molt_to_maturity' but got '",kw,"\n");
    warning(msg,immediate.=TRUE);
    return(NULL);
  }
  #--extract parameter combinations
  kw<-res[[k]]; k<-k+1;
  if (kw !="PARAMETER_COMBINATIONS"){
    msg<-paste0("----Error extracting terminal molt parameters info.\n",
                "----Expected keyword 'PARAMETER_COMBINATIONS' but got '",kw,"\n");
    warning(msg,immediate.=TRUE);
    return(NULL);
  }
  lst[["nPCs"]]<-as.integer(res[[k]]); k<-k+1;
  pcs<-list();
  for (i in 1:lst[["nPCs"]]){
    pc<-res[[k]]; k<-k+1;
    pcs[[i]]<-list(id         =as.integer(pc[1]),
                   tb         =parseTimeBlock(pc[2]),
                   x          =pc[3],
                   pvLgtPrM2M =as.integer(pc[4]),
                   label      =pc[5]);
  }
  lst[["pcs"]]<-pcs;

  #--extract info for parameters
  kw<-res[[k]]; k<-k+1;
  if (kw !="PARAMETERS"){
    msg<-paste0("----Error extracting terminal molt parameters info.\n",
                "----Expected keyword 'PARAMETERS' but got '",kw,"\n");
    warning(msg,immediate.=TRUE);
    return(NULL);
  }
  #--extract info for vector parameters
  pvs<-list();
  rsp<-extractTCSAM_BoundedVectorInfo(res,k,"pvLgtPrM2M");
  pvs[["pvLgtPrM2M"]]<-rsp$lst; k<-rsp$k;
  lst[["pvs"]]<-pvs;

  return(list(k=k,lst=lst));
}

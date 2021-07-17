#'
#' @title Extract model growth parameters info from a parsed file for a TCSAM02 model run
#'
#' @description Function to extract model growth parameters info from a parsed file for a TCSAM02 model run.
#'
#' @param res - list with parsed file results
#' @param ks - start location index
#' @param verbose - flag to print info
#'
#' @return list with elements
#' k - index of next element to parse
#' lst - list of model parameters info for growth parameters
#'
#' @details None.
#'
#' @export
#'
extractTCSAM_ParamInfoGrowth<-function(res,
                                            ks,
                                            verbose=FALSE){
  lst<-list();
  k<-ks;
  kw<-res[[k]]; k<-k+1;
  if (kw !="growth"){
    msg<-paste0("----Error extracting growth parameters info.\n",
                "----Expected keyword 'growth' but got '",kw,"\n");
    warning(msg,immediate.=TRUE);
    return(NULL);
  }
  #--extract parameter combinations
  kw<-res[[k]]; k<-k+1;
  if (kw !="PARAMETER_COMBINATIONS"){
    msg<-paste0("----Error extracting growth parameters info.\n",
                "----Expected keyword 'PARAMETER_COMBINATIONS' but got '",kw,"\n");
    warning(msg,immediate.=TRUE);
    return(NULL);
  }
  lst[["nPCs"]]<-as.integer(res[[k]]); k<-k+1;
  pcs<-list();
  for (i in 1:lst[["nPCs"]]){
    pc<-res[[k]]; k<-k+1;
    pcs[[i]]<-list(id        =as.integer(pc[1]),
                   tb        =parseTimeBlock(pc[2]),
                   x         =pc[3],
                   pGrA      =as.integer(pc[4]),
                   pGrB      =as.integer(pc[5]),
                   pGrBeta   =as.integer(pc[6]),
                   zScaleGrA =as.numeric(pc[7]),
                   zScaleGrB =as.numeric(pc[8]),
                   label     =pc[9]);
  }
  lst[["pcs"]]<-pcs;

  #--extract info for parameters
  kw<-res[[k]]; k<-k+1;
  if (kw !="PARAMETERS"){
    msg<-paste0("----Error extracting growth parameters info.\n",
                "----Expected keyword 'PARAMETERS' but got '",kw,"\n");
    warning(msg,immediate.=TRUE);
    return(NULL);
  }
  #--extract info for number parameters
  pis<-list();
  rsp<-extractTCSAM_BoundedParameterInfo(res,k,"pGrA");
  pis[["pGrA"]]<-rsp$lst; k<-rsp$k;
  rsp<-extractTCSAM_BoundedParameterInfo(res,k,"pGrB");
  pis[["pGrB"]]<-rsp$lst; k<-rsp$k;
  rsp<-extractTCSAM_BoundedParameterInfo(res,k,"pGrBeta");
  pis[["pGrBeta"]]<-rsp$lst; k<-rsp$k;
  lst[["pis"]]<-pis;

  return(list(k=k,lst=lst));
}

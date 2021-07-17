#'
#' @title Extract model natural mortality parameters info from a parsed file for a TCSAM02 model run
#'
#' @description Function to extract model natural mortality parameters info from a parsed file for a TCSAM02 model run.
#'
#' @param res - list with parsed file results
#' @param ks - start location index
#' @param verbose - flag to print info
#'
#' @return list with elements
#' k - index of next element to parse
#' lst - list of model parameters info for natural mortality parameters
#'
#' @details None.
#'
#' @export
#'
extractTCSAM_ParamInfoNaturalMortality<-function(res,
                                                 ks,
                                                 verbose=FALSE){
  lst<-list();
  k<-ks;
  kw<-res[[k]]; k<-k+1;
  if (kw !="natural_mortality"){
    msg<-paste0("----Error extracting natural mortality parameters info.\n",
                "----Expected keyword 'natural_mortality' but got '",kw,"\n");
    warning(msg,immediate.=TRUE);
    return(NULL);
  }
  #--extract reference size for scaling
  lst[["refZ"]]<-as.numeric(res[[k]]); k<-k+1;
  #--extract parameter combinations
  kw<-res[[k]]; k<-k+1;
  if (kw !="PARAMETER_COMBINATIONS"){
    msg<-paste0("----Error extracting natural mortality parameters info.\n",
                "----Expected keyword 'PARAMETER_COMBINATIONS' but got '",kw,"\n");
    warning(msg,immediate.=TRUE);
    return(NULL);
  }
  lst[["nPCs"]]<-as.integer(res[[k]]); k<-k+1;
  pcs<-list();
  for (i in 1:lst[["nPCs"]]){
    pc<-res[[k]]; k<-k+1;
    pcs[[i]]<-list(id   =as.integer(pc[1]),
                   tb   =parseTimeBlock(pc[2]),
                   x    =pc[3],
                   m    =pc[4],
                   s    =pc[5],
                   pM   =as.integer(pc[6]),
                   pDM1 =as.integer(pc[7]),
                   pDM2 =as.integer(pc[8]),
                   pDM3 =as.integer(pc[9]),
                   pDM4 =as.integer(pc[10]),
                   zScaling=as.integer(pc[11]),
                   label   =pc[12]);
  }
  lst[["pcs"]]<-pcs;

  #--extract info for parameters
  kw<-res[[k]]; k<-k+1;
  if (kw !="PARAMETERS"){
    msg<-paste0("----Error extracting natural mortality parameters info.\n",
                "----Expected keyword 'PARAMETERS' but got '",kw,"\n");
    warning(msg,immediate.=TRUE);
    return(NULL);
  }
  #--extract info for number parameters
  pis<-list();
  rsp<-extractTCSAM_BoundedParameterInfo(res,k,"pM");
  pis[["pM"]]<-rsp$lst; k<-rsp$k;
  rsp<-extractTCSAM_BoundedParameterInfo(res,k,"pDM1");
  pis[["pDM1"]]<-rsp$lst; k<-rsp$k;
  rsp<-extractTCSAM_BoundedParameterInfo(res,k,"pDM2");
  pis[["pDM2"]]<-rsp$lst; k<-rsp$k;
  rsp<-extractTCSAM_BoundedParameterInfo(res,k,"pDM3");
  pis[["pDM3"]]<-rsp$lst; k<-rsp$k;
  rsp<-extractTCSAM_BoundedParameterInfo(res,k,"pDM4");
  pis[["pDM4"]]<-rsp$lst; k<-rsp$k;
  lst[["pis"]]<-pis;

  return(list(k=k,lst=lst));
}

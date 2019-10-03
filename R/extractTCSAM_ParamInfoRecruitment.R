#'
#' @title Extract model recruitment parameters info from a parsed file for a TCSAM02 model run
#'
#' @description Function to extract model recruitment parameters info from a parsed file for a TCSAM02 model run.
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
extractTCSAM_ParamInfoRecruitment<-function(res,
                                            ks,
                                            verbose=FALSE){
  lst<-list();
  k<-ks;
  kw<-res[[k]]; k<-k+1;
  if (kw !="recruitment"){
    msg<-paste0("----Error extracting recruitment parameters info.\n",
                "----Expected keyword 'recruitment' but got '",kw,"\n");
    warning(msg,immediate.=TRUE);
    return(NULL);
  }
  kw<-res[[k]]; k<-k+1;
  if (kw !="PARAMETER_COMBINATIONS"){
    msg<-paste0("----Error extracting recruitment parameters info.\n",
                "----Expected keyword 'PARAMETER_COMBINATIONS' but got '",kw,"\n");
    warning(msg,immediate.=TRUE);
    return(NULL);
  }
  lst[["nPCs"]]<-as.integer(res[[k]]); k<-k+1;
  pcs<-list();
  for (i in 1:lst[["nPCs"]]){
    pc<-res[[k]]; k<-k+1;
    pcs[[i]]<-list(id      =as.integer(pc[1]),
                   tb      =parseTimeBlock(pc[2]),
                   pLnR    =as.integer(pc[3]),
                   pRCV    =as.integer(pc[4]),
                   pRX     =as.integer(pc[5]),
                   pRa     =as.integer(pc[6]),
                   pRb     =as.integer(pc[7]),
                   pDevsLnR=as.integer(pc[8]),
                   nllWgt  =as.numeric(pc[9]),
                   label   =pc[10]);
  }
  lst[["pcs"]]<-pcs;
  kw<-res[[k]]; k<-k+1;
  if (kw !="PARAMETERS"){
    msg<-paste0("----Error extracting recruitment parameters info.\n",
                "----Expected keyword 'PARAMETERS' but got '",kw,"\n");
    warning(msg,immediate.=TRUE);
    return(NULL);
  }
  pis<-list();
  rsp<-extractTCSAM_BoundedParameterInfo(res,k,"pLnR");
  pis[["pLnR"]]<-rsp$lst; k<-rsp$k;
  rsp<-extractTCSAM_BoundedParameterInfo(res,k,"pRCV");
  pis[["pRCV"]]<-rsp$lst; k<-rsp$k;
  rsp<-extractTCSAM_BoundedParameterInfo(res,k,"pRX");
  pis[["pRX"]]<-rsp$lst; k<-rsp$k;
  rsp<-extractTCSAM_BoundedParameterInfo(res,k,"pRa");
  pis[["pRa"]]<-rsp$lst; k<-rsp$k;
  rsp<-extractTCSAM_BoundedParameterInfo(res,k,"pRb");
  pis[["pRb"]]<-rsp$lst; k<-rsp$k;
  lst[["pis"]]<-pis;

  return(list(k=k,lst=lst));
}

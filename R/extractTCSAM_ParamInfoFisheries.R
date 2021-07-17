#'
#' @title Extract model fisheries parameters info from a parsed file for a TCSAM02 model run
#'
#' @description Function to extract model fisheries parameters info from a parsed file for a TCSAM02 model run.
#'
#' @param res - list with parsed file results
#' @param ks - start location index
#' @param verbose - flag to print info
#'
#' @return list with elements
#' k - index of next element to parse
#' lst - list of model parameters info for fisheries parameters
#'
#' @details None.
#'
#' @export
#'
extractTCSAM_ParamInfoFisheries<-function(res,
                                            ks,
                                            verbose=FALSE){
  lst<-list();
  k<-ks;
  kw<-res[[k]]; k<-k+1;
  if (kw !="fisheries"){
    msg<-paste0("----Error extracting fisheries parameters info.\n",
                "----Expected keyword 'fisheries' but got '",kw,"\n");
    warning(msg,immediate.=TRUE);
    return(NULL);
  }
  #--extract parameter combinations
  kw<-res[[k]]; k<-k+1;
  if (kw !="PARAMETER_COMBINATIONS"){
    msg<-paste0("----Error extracting fisheries parameters info.\n",
                "----Expected keyword 'PARAMETER_COMBINATIONS' but got '",kw,"\n");
    warning(msg,immediate.=TRUE);
    return(NULL);
  }
  lst[["nPCs"]]<-as.integer(res[[k]]); k<-k+1;
  pcs<-list();
  for (i in 1:lst[["nPCs"]]){
    pc<-res[[k]]; k<-k+1;
    pcs[[i]]<-list(id      =as.integer(pc[1]),
                   f       =as.integer(pc[2]),
                   tb      =parseTimeBlock(pc[3]),
                   x       =pc[4],
                   m       =pc[5],
                   s       =pc[6],
                   pHM     =as.integer(pc[7]),
                   pLnC    =as.integer(pc[8]),
                   pDC1    =as.integer(pc[9]),
                   pDC2    =as.integer(pc[10]),
                   pDC3    =as.integer(pc[11]),
                   pDC4    =as.integer(pc[12]),
                   pDevsLnC=as.integer(pc[13]),
                   pLnEffX =as.integer(pc[14]),
                   pLgtRet =as.integer(pc[15]),
                   idx.SelFcn =as.integer(pc[16]),
                   idx.RetFcn =as.integer(pc[17]),
                   useEffX    =as.integer(pc[18]),
                   label      =pc[19]);
  }
  lst[["pcs"]]<-pcs;

  #--extract info for parameters
  kw<-res[[k]]; k<-k+1;
  if (kw !="PARAMETERS"){
    msg<-paste0("----Error extracting fisheries parameters info.\n",
                "----Expected keyword 'PARAMETERS' but got '",kw,"\n");
    warning(msg,immediate.=TRUE);
    return(NULL);
  }
  #--extract info for number parameters
  pis<-list();
  rsp<-extractTCSAM_BoundedParameterInfo(res,k,"pHM");
  pis[["pHM"]]<-rsp$lst; k<-rsp$k;
  rsp<-extractTCSAM_BoundedParameterInfo(res,k,"pLnC");
  pis[["pLnC"]]<-rsp$lst; k<-rsp$k;
  rsp<-extractTCSAM_BoundedParameterInfo(res,k,"pDC1");
  pis[["pDC1"]]<-rsp$lst; k<-rsp$k;
  rsp<-extractTCSAM_BoundedParameterInfo(res,k,"pDC2");
  pis[["pDC2"]]<-rsp$lst; k<-rsp$k;
  rsp<-extractTCSAM_BoundedParameterInfo(res,k,"pDC3");
  pis[["pDC3"]]<-rsp$lst; k<-rsp$k;
  rsp<-extractTCSAM_BoundedParameterInfo(res,k,"pDC4");
  pis[["pDC4"]]<-rsp$lst; k<-rsp$k;
  lst[["pis"]]<-pis;

  #--extract info for devs parameters
  pvs<-list();
  rsp<-extractTCSAM_BoundedVectorInfo(res,k,"pDevsLnC");
  pvs[["pDevsLnC"]]<-rsp$lst; k<-rsp$k;
  lst[["pvs"]]<-pvs;

  #--extract info for more parameters
  rsp<-extractTCSAM_BoundedParameterInfo(res,k,"pLnEffX");
  pis[["pLnEffX"]]<-rsp$lst; k<-rsp$k;
  rsp<-extractTCSAM_BoundedParameterInfo(res,k,"pLgtRet");
  pis[["pLgtRet"]]<-rsp$lst; k<-rsp$k;
  lst[["pis"]]<-pis;

  return(list(k=k,lst=lst));
}

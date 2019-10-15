#'
#' @title Extract model selectivity function parameters info from a parsed file for a TCSAM02 model run
#'
#' @description Function to extract model selectivity function parameters info from a parsed file for a TCSAM02 model run.
#'
#' @param res - list with parsed file results
#' @param ks - start location index
#' @param verbose - flag to print info
#'
#' @return list with elements
#' k - index of next element to parse
#' lst - list of model parameters info for selectivity function parameters
#'
#' @details None.
#'
#' @export
#'
extractTCSAM_ParamInfoSelFunctions<-function(res,
                                             ks,
                                             verbose=FALSE){
  lst<-list();
  k<-ks;
  kw<-res[[k]]; k<-k+1;
  if (kw !="selectivities"){
    msg<-paste0("----Error extracting selectivity function parameters info.\n",
                "----Expected keyword 'selectivities' but got '",kw,"\n");
    warning(msg,immediate.=TRUE);
    return(NULL);
  }
  #--extract parameter combinations
  kw<-res[[k]]; k<-k+1;
  if (kw !="PARAMETER_COMBINATIONS"){
    msg<-paste0("----Error extracting selectivity function parameters info.\n",
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
                   pS1     =as.integer(pc[3]),
                   pS2     =as.integer(pc[4]),
                   pS3     =as.integer(pc[5]),
                   pS4     =as.integer(pc[6]),
                   pS5     =as.integer(pc[7]),
                   pS6     =as.integer(pc[8]),
                   pDevsS1 =as.integer(pc[9]),
                   pDevsS2 =as.integer(pc[10]),
                   pDevsS3 =as.integer(pc[11]),
                   pDevsS4 =as.integer(pc[12]),
                   pDevsS5 =as.integer(pc[13]),
                   pDevsS6 =as.integer(pc[14]),
                   pvNPSel =as.integer(pc[15]),
                   fsZ     =as.numeric(pc[16]),
                   selFcn  =pc[17],
                   label   =pc[18]);
  }
  lst[["pcs"]]<-pcs;

  #--extract info for parameters
  kw<-res[[k]]; k<-k+1;
  if (kw !="PARAMETERS"){
    msg<-paste0("----Error extracting selectivity function parameters info.\n",
                "----Expected keyword 'PARAMETERS' but got '",kw,"\n");
    warning(msg,immediate.=TRUE);
    return(NULL);
  }
  #--extract info for number parameters
  pis<-list();
  rsp<-extractTCSAM_BoundedParameterInfo(res,k,"pS1");
  pis[["pS1"]]<-rsp$lst; k<-rsp$k;
  rsp<-extractTCSAM_BoundedParameterInfo(res,k,"pS2");
  pis[["pS2"]]<-rsp$lst; k<-rsp$k;
  rsp<-extractTCSAM_BoundedParameterInfo(res,k,"pS3");
  pis[["pS3"]]<-rsp$lst; k<-rsp$k;
  rsp<-extractTCSAM_BoundedParameterInfo(res,k,"pS4");
  pis[["pS4"]]<-rsp$lst; k<-rsp$k;
  rsp<-extractTCSAM_BoundedParameterInfo(res,k,"pS5");
  pis[["pS5"]]<-rsp$lst; k<-rsp$k;
  rsp<-extractTCSAM_BoundedParameterInfo(res,k,"pS6");
  pis[["pS6"]]<-rsp$lst; k<-rsp$k;
  lst[["pis"]]<-pis;
  #--extract info for devs parameters
  pvs<-list();
  rsp<-extractTCSAM_BoundedVectorInfo(res,k,"pDevsS1");
  pvs[["pDevsS1"]]<-rsp$lst; k<-rsp$k;
  rsp<-extractTCSAM_BoundedVectorInfo(res,k,"pDevsS2");
  pvs[["pDevsS2"]]<-rsp$lst; k<-rsp$k;
  rsp<-extractTCSAM_BoundedVectorInfo(res,k,"pDevsS3");
  pvs[["pDevsS3"]]<-rsp$lst; k<-rsp$k;
  rsp<-extractTCSAM_BoundedVectorInfo(res,k,"pDevsS4");
  pvs[["pDevsS4"]]<-rsp$lst; k<-rsp$k;
  rsp<-extractTCSAM_BoundedVectorInfo(res,k,"pDevsS5");
  pvs[["pDevsS5"]]<-rsp$lst; k<-rsp$k;
  rsp<-extractTCSAM_BoundedVectorInfo(res,k,"pDevsS6");
  pvs[["pDevsS6"]]<-rsp$lst; k<-rsp$k;
  rsp<-extractTCSAM_BoundedVectorInfo(res,k,"pvNPSel");
  pvs[["pvNPSel"]]<-rsp$lst; k<-rsp$k;
  lst[["pvs"]]<-pvs;

  return(list(k=k,lst=lst));
}

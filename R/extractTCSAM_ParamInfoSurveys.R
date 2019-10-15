#'
#' @title Extract model surveys parameters info from a parsed file for a TCSAM02 model run
#'
#' @description Function to extract model surveys parameters info from a parsed file for a TCSAM02 model run.
#'
#' @param res - list with parsed file results
#' @param ks - start location index
#' @param verbose - flag to print info
#'
#' @return list with elements
#' k - index of next element to parse
#' lst - list of model parameters info for surveys parameters
#'
#' @details None.
#'
#' @export
#'
extractTCSAM_ParamInfoSurveys<-function(res,
                                        ks,
                                        verbose=FALSE){
  lst<-list();
  k<-ks;
  kw<-res[[k]]; k<-k+1;
  if (kw !="surveys"){
    msg<-paste0("----Error extracting surveys parameters info.\n",
                "----Expected keyword 'surveys' but got '",kw,"\n");
    warning(msg,immediate.=TRUE);
    return(NULL);
  }
  #--extract parameter combinations
  kw<-res[[k]]; k<-k+1;
  if (kw !="PARAMETER_COMBINATIONS"){
    msg<-paste0("----Error extracting surveys parameters info.\n",
                "----Expected keyword 'PARAMETER_COMBINATIONS' but got '",kw,"\n");
    warning(msg,immediate.=TRUE);
    return(NULL);
  }
  lst[["nPCs"]]<-as.integer(res[[k]]); k<-k+1;
  pcs<-list();
  for (i in 1:lst[["nPCs"]]){
    pc<-res[[k]]; k<-k+1;
    pcs[[i]]<-list(id      =as.integer(pc[1]),
                   v       =as.integer(pc[2]),
                   tb      =parseTimeBlock(pc[3]),
                   x       =pc[4],
                   m       =pc[5],
                   s       =pc[6],
                   pQ      =as.integer(pc[7]),
                   pDQ1    =as.integer(pc[8]),
                   pDQ2    =as.integer(pc[9]),
                   pDQ3    =as.integer(pc[10]),
                   pDQ4    =as.integer(pc[11]),
                   pA      =as.integer(pc[12]),
                   idx.AvlFcn =as.integer(pc[13]),
                   idx.SelFcn =as.integer(pc[14]),
                   label      =pc[15]);
  }
  lst[["pcs"]]<-pcs;

  #--extract info for parameters
  kw<-res[[k]]; k<-k+1;
  if (kw !="PARAMETERS"){
    msg<-paste0("----Error extracting surveys parameters info.\n",
                "----Expected keyword 'PARAMETERS' but got '",kw,"\n");
    warning(msg,immediate.=TRUE);
    return(NULL);
  }
  #--extract info for number parameters
#pQ  pDQ1  pDQ2  pDQ3  pDQ4  pA  idx.AvlFcn  idx.SelFcn  label
  pis<-list();
  rsp<-extractTCSAM_BoundedParameterInfo(res,k,"pQ");
  pis[["pQ"]]<-rsp$lst; k<-rsp$k;
  rsp<-extractTCSAM_BoundedParameterInfo(res,k,"pDQ1");
  pis[["pDQ1"]]<-rsp$lst; k<-rsp$k;
  rsp<-extractTCSAM_BoundedParameterInfo(res,k,"pDQ2");
  pis[["pDQ2"]]<-rsp$lst; k<-rsp$k;
  rsp<-extractTCSAM_BoundedParameterInfo(res,k,"pDQ3");
  pis[["pDQ3"]]<-rsp$lst; k<-rsp$k;
  rsp<-extractTCSAM_BoundedParameterInfo(res,k,"pDQ4");
  pis[["pDQ4"]]<-rsp$lst; k<-rsp$k;
  rsp<-extractTCSAM_BoundedParameterInfo(res,k,"pA");
  pis[["pA"]]<-rsp$lst; k<-rsp$k;
  lst[["pis"]]<-pis;

  return(list(k=k,lst=lst));
}

#'
#' @title Extract prior probability info from a character vector for a TCSAM02 model run
#'
#' @description Function to extract prior probability info from a character vector for a TCSAM02 model run.
#'
#' @param pv - character vector with parameter vector information
#' @param k - start location index for prior information
#' @param verbose - flag to print info
#'
#' @return list with elements
#' wgt - prior weight
#' type - distribution type for prior
#' params - numeric vector with parameters for prior
#' consts - numeric vector with constants for prior
#'
#' @details None.
#'
#' @export
#'
extractTCSAM_PriorInfo<-function(pv,
                                 k,
                                 verbose=FALSE){
  wgt <-pv[k]; k<-k+1;
  type<-pv[k]; k<-k+1;
  params<-"none";
  consts<-"none";
  if (type=="normal"){
    params<-as.numeric(pv[k:(k+1)]);
  } else if (type=="ar1_normal"){
    params<-as.numeric(pv[k:(k+1)]);
  }
  lst<-list(wgt=wgt,
            type=type,
            params=params,
            consts=consts);
  return(lst);
}

#'
#' @title Read model fleet data file for a TCSAM02 model run
#'
#' @description Function to read model fleet data file for a TCSAM02 model run.
#'
#' @param conn - file name for model fleet data file
#' @param verbose - flag to print info
#'
#' @return list with fleet data set
#'
#' @details None.
#'
#' @export
#'
readTCSAM_FleetData<-function(conn,
                              verbose=FALSE){
  if (!inherits(conn,"connection")){
    if (!file.exists(conn)){
      msg<-paste0("--Error reading TCSAM fleet data file.\n",
                  "--File '",conn,"' does not exist. Aborting!\n\n")
      stop(msg);
    }
    fn<-conn;
    cat(paste0("--Reading model fleet data file '",fn,"'\n"))
    conn<-file(fn,open="r");
  } else {
    if (!isOpen(conn)) open(conn,open="r");
  }

  #--read file
  lst<-list();
  strv<-readLines(conn);
  close(conn);
  res  <-wtsUtilities::parseText(strv);
  k<-1;
  kw<-res[[k]]; k<-k+1;
  if (!(kw %in% .KEYWORDS[["FLEET_TYPES"]])){
    msg<-paste0("----Error reading fleet file '",fn,"'\n",
                "----Expected one of the following keywords:\n",
                "----",paste0("'",.KEYWORDS[["FLEET_TYPES"]],"'",collapse=", "),"\n",
                "----but got '",kw,"\n");
    warning(msg,immediate.=TRUE);
  }
  lst[["fleet_type"]]<-kw;
  lst[["name"]]      <-res[[k]]; k<-k+1;
  lst[["hasIndexCatchData"]]    <-as.logical(res[[k]]); k<-k+1;
  lst[["hasRetainedCatchData"]] <-as.logical(res[[k]]); k<-k+1;
  lst[["hasDiscardCatchData"]]  <-as.logical(res[[k]]); k<-k+1;
  lst[["hasTotalCatchData"]]    <-as.logical(res[[k]]); k<-k+1;
  lst[["hasEffortData"]]        <-as.logical(res[[k]]); k<-k+1;
  if (lst[["hasIndexCatchData"]]){
    #--file has index catch data
    tmp1<-extractTCSAM_CatchData(res,k);
    if (is.null(tmp1)){
      msg<-paste0("----Error reading fleet file '",fn,"'\n",
                  "----Could not extract INDEX DATA.\n");
      warning(msg,immediate.=TRUE);
      return(NULL);
    }
    k<-tmp1$k;
    lst[["IndexCatchData"]]<-tmp1$lst;
  }
  if (lst[["hasRetainedCatchData"]]){
    #--file has retained catch data
    tmp1<-extractTCSAM_CatchData(res,k,verbose=TRUE);
    if (is.null(tmp1)){
      msg<-paste0("----Error reading fleet file '",fn,"'\n",
                  "----Could not extract RETAINED CATCH DATA.\n");
      warning(msg,immediate.=TRUE);
      return(NULL);
    }
    k<-tmp1$k;
    lst[["retainedCatchData"]]<-tmp1$lst;
  }
  if (lst[["hasDiscardCatchData"]]){
    #--file has discard catch data
    tmp1<-extractTCSAM_CatchData(res,k);
    if (is.null(tmp1)){
      msg<-paste0("----Error reading fleet file '",fn,"'\n",
                  "----Could not extract DISCARD CATCH DATA.\n");
      warning(msg,immediate.=TRUE);
      return(NULL);
    }
    k<-tmp1$k;
    lst[["discardCatchData"]]<-tmp1$lst;
  }
  if (lst[["hasTotalCatchData"]]){
    #--file has total catch data
    tmp1<-extractTCSAM_CatchData(res,k);
    if (is.null(tmp1)){
      msg<-paste0("----Error reading fleet file '",fn,"'\n",
                  "----Could not extract TOTAL CATCH DATA.\n");
      warning(msg,immediate.=TRUE);
      return(NULL);
    }
    k<-tmp1$k;
    lst[["totalCatchData"]]<-tmp1$lst;
  }
  if (lst[["hasEffortData"]]){
    #--file has catch effort data
    tmp1<-extractTCSAM_EffortData(res,k);
    if (is.null(tmp1)){
      msg<-paste0("----Error reading fleet file '",fn,"'\n",
                  "----Could not extract EFFORT DATA.\n");
      warning(msg,immediate.=TRUE);
      return(NULL);
    }
    k<-tmp1$k;
    lst[["effortData"]]<-tmp1$lst;
  }

  return(lst);
}

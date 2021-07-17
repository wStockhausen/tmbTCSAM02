#'
#' @title Read model parameters info file for a TCSAM02 model run
#'
#' @description Function to read model parameters info file for a TCSAM02 model run.
#'
#' @param conn - file name for model parameters info file
#' @param verbose - flag to print info
#'
#' @return list with model parameters info
#'
#' @details None.
#'
#' @export
#'
readTCSAM_ModelParametersInfo<-function(conn,
                              verbose=FALSE){
  if (!inherits(conn,"connection")){
    if (!file.exists(conn)){
      msg<-paste0("--Error reading TCSAM model parameters info file.\n",
                  "--File '",conn,"' does not exist. Aborting!\n\n")
      stop(msg);
    }
    fn<-conn;
    cat(paste0("--Reading model parameters info file '",fn,"'\n"))
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
  ver<-res[[k]]; k<-k+1;#model parameters info file version

  #--recruitment
  resp<-extractTCSAM_ParamInfoRecruitment(res,k);
  if (is.null(resp)) stop(paste0("----Error reading parameters info file '",fn,"'\n"));
  k<-resp$k;
  lst[["recruitment"]]<-resp$lst;

  #--natural mortality
  resp<-extractTCSAM_ParamInfoNaturalMortality(res,k);
  if (is.null(resp)) stop(paste0("----Error reading parameters info file '",fn,"'\n"));
  k<-resp$k;
  lst[["natural mortality"]]<-resp$lst;

  #--growth
  resp<-extractTCSAM_ParamInfoGrowth(res,k);
  if (is.null(resp)) stop(paste0("----Error reading parameters info file '",fn,"'\n"));
  k<-resp$k;
  lst[["growth"]]<-resp$lst;

  #--terminal molt
  resp<-extractTCSAM_ParamInfoTerminalMolt(res,k);
  if (is.null(resp)) stop(paste0("----Error reading parameters info file '",fn,"'\n"));
  k<-resp$k;
  lst[["terminal molt"]]<-resp$lst;

  #--selectivity functions
  resp<-extractTCSAM_ParamInfoSelFunctions(res,k);
  if (is.null(resp)) stop(paste0("----Error reading parameters info file '",fn,"'\n"));
  k<-resp$k;
  lst[["selectivity functions"]]<-resp$lst;

  #--fisheries
  resp<-extractTCSAM_ParamInfoFisheries(res,k);
  if (is.null(resp)) stop(paste0("----Error reading parameters info file '",fn,"'\n"));
  k<-resp$k;
  lst[["fisheries"]]<-resp$lst;

  #--fisheries
  resp<-extractTCSAM_ParamInfoSurveys(res,k);
  if (is.null(resp)) stop(paste0("----Error reading parameters info file '",fn,"'\n"));
  k<-resp$k;
  lst[["surveys"]]<-resp$lst;

  return(lst);
}

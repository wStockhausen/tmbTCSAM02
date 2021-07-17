#'
#' @title Read model configuration file for a TCSAM02 model run
#'
#' @description Function to read model configuration file for a TCSAM02 model run.
#'
#' @param fn - file name for model configuration file
#' @param verbose - flag to print info
#'
#' @return list with model configuration information
#'
#' @details None.
#'
#' @export
#'
readTCSAM_ModelConfiguration<-function(conn,
                                       verbose=FALSE){
  if (!inherits(conn,"connection")){
    if (!file.exists(conn)){
      msg<-paste0("--Error reading TCSAM model configuration file.\n",
                  "--File '",conn,"' does not exist. Aborting!\n\n")
      stop(msg);
    }
    fn<-conn;
    cat(paste0("--Reading model configuration file '",fn,"'\n"))
    conn<-file(fn,open="r");
  } else {
    if (!isOpen(conn)) open(conn,open="r");
  }

  #--read file
  strv<-readLines(conn);
  res  <-wtsUtilities::parseText(strv);
  close(conn);
  version = res[[1]];
  if (version=="2016.11.15") {mc = readMCI_2016_11_15(res);} else
  if (version=="2021.04.10") {mc = readMCI_2021_04_10(res);} else
    {stop(paste0("Model configuration",version,"not recognized\n ")); return(NULL);}
  #--set dims info for tmbTCSAM in the global environment (using "<<-")
  .DIMS[["y"]]<<-mc[["mxYr"]]-mc[["mnYr"]]+1; .DIMNAMES[["y"]]<<-as.character(mc[["mnYr"]]:mc[["mxYr"]]);
  .DIMS[["z"]]<<-mc[["nZBs"]];                .DIMNAMES[["z"]]<<-as.character(mc[["zBs"]]);
  .DIMS[["f"]]<<-mc[["nFsh"]];                .DIMNAMES[["f"]]<<-mc[["fshNames"]];
  .DIMS[["v"]]<<-mc[["nSrv"]];                .DIMNAMES[["v"]]<<-mc[["srvNames"]];

  return(mc);
}#--readTCSAM_ModelConfiguration

readMCI_2021_04_10<-function(res,verbose=TRUE){
  if (verbose) cat("#--------Using 'readMCI_2021_04_10' to read MCI-------\n");
  if (verbose) for (i in 1:length(res)) cat(res[[i]],"\n")
  mc = list();
  k = 1;
  mc[["version"]]  <-res[[k]]; k<-k+1;
  mc[["case"]]     <-res[[k]]; k<-k+1;
  mc[["mnYr"]]     <-as.integer(res[[k]]); k<-k+1;
  mc[["mxYr"]]     <-as.integer(res[[k]]); k<-k+1;   #--assessment year (final pop model numbers at size are given for July 1, assessment year)
  mc[["mnYrOFL"]]  <-as.integer(res[[k]]); k<-k+1;   #--min year for calculating OFL-related average recruitment
  mc[["nYrRecOffset"]]<-as.integer(res[[k]]); k<-k+1;#--offset for calculating average recruitment
  mc[["mxZsByX"]]     <-as.numeric(res[[k]]); k<-k+1;#--max sizes, by sex
  mc[["mxZRec"]]   <-as.numeric(res[[k]]); k<-k+1;   #--max size for recruitment distribution
  mc[["nZBs"]]     <-as.integer(res[[k]]); k<-k+1;   #--number of size bins in the model
  mc[["zCs"]]      <-as.numeric(res[[k]]); k<-k+1;   #--size bin cutpoints
  mc[["zBs"]]      <-(mc[["zCs"]][2:(mc[["nZBs"]]+1)]+mc[["zCs"]][1:mc[["nZBs"]]])/2.0;
  mc[["nFsh"]]     <-as.integer(res[[k]]); k<-k+1;   #--number of fisheries
  mc[["fshNames"]] <-character(mc$nFsh);
  for (f in 1:mc$nFsh) {mc[["fshNames"]][f]<-res[[k]];  k<-k+1;}
  mc[["nSrv"]] <- as.integer(res[[k]]); k<-k+1;      #--number of surveys
  mc[["srvNames"]]<-character(mc$nSrv);
  for (s in 1:mc$nSrv) {mc[["srvNames"]][s]<-res[[k]];  k<-k+1;}
  #--
  mc[["run_OpModOnly"]]<-as.logical(res[[k]]); k<-k+1;
  mc[["fit_toPriors"]] <-as.logical(res[[k]]); k<-k+1;
  mc[["fn_paramsInfo"]]<-res[[k]]; k<-k+1;
  mc[["fn_datasets"]]  <-res[[k]]; k<-k+1;
  mc[["fn_options"]]   <-res[[k]]; k<-k+1;
  mc[["jitter"]]       <-subForKey(res[[k]],.KV_MAP$ON_OFF); k<-k+1;
  mc[["jit_fac"]]      <-as.numeric(res[[k]]); k<-k+1;
  mc[["prior_resamp"]] <-subForKey(res[[k]],.KV_MAP$ON_OFF); k<-k+1;
  mc[["prior_vif"]]    <-as.numeric(res[[k]]); k<-k+1;
  return(mc);
}

readMCI_2016_11_15<-function(res){
  mc = list();
  k = 1;
  mc[["version"]]  <-res[[k]]; k<-k+1;
  mc[["case"]]     <-res[[k]]; k<-k+1;
  mc[["mnYr"]]     <-as.integer(res[[k]]); k<-k+1;
  mc[["mxYr"]]     <-as.integer(res[[k]]); k<-k+1;
  mc[["nZBs"]]     <-as.integer(res[[k]]); k<-k+1;
  mc[["zCs"]]      <-as.numeric(res[[k]]); k<-k+1;#size bin cutpoints
  mc[["zBs"]]      <-(mc[["zCs"]][2:(mc[["nZBs"]]+1)]+mc[["zCs"]][1:mc[["nZBs"]]])/2.0;
  mc[["nFsh"]]     <-as.integer(res[[k]]); k<-k+1;
  mc[["fshNames"]] <-character(mc$nFsh);
  for (f in 1:mc$nFsh) {mc[["fshNames"]][f]<-res[[k]];  k<-k+1;}
  mc[["nSrv"]] <- as.integer(res[[k]]); k<-k+1;
  mc[["srvNames"]]<-character(mc$nSrv);
  for (s in 1:mc$nSrv) {mc[["srvNames"]][s]<-res[[k]];  k<-k+1;}
  #--
  mc[["run_OpModOnly"]]<-as.logical(res[[k]]); k<-k+1;
  mc[["fit_toPriors"]] <-as.logical(res[[k]]); k<-k+1;
  mc[["fn_paramsInfo"]]<-res[[k]]; k<-k+1;
  mc[["fn_datasets"]]  <-res[[k]]; k<-k+1;
  mc[["fn_options"]]   <-res[[k]]; k<-k+1;
  mc[["jitter"]]       <-subForKey(res[[k]],.KV_MAP$ON_OFF); k<-k+1;
  mc[["jit_fac"]]      <-as.numeric(res[[k]]); k<-k+1;
  mc[["prior_resamp"]] <-subForKey(res[[k]],.KV_MAP$ON_OFF); k<-k+1;
  mc[["prior_vif"]]    <-as.numeric(res[[k]]); k<-k+1;
  return(mc);
}


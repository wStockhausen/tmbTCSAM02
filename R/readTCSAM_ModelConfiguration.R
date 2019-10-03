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
  mc<-list();
  strv<-readLines(conn);
  res  <-wtsUtilities::parseText(strv);
  k<-1;
  mc[["version"]]  <-res[[k]]; k<-k+1;
  mc[["case"]] <-res[[2]]; k<-k+1;
  mc[["mnYr"]] <-as.integer(res[[k]]); k<-k+1;
  mc[["mxYr"]] <-as.integer(res[[k]]); k<-k+1;
  mc[["nZBs"]] <-as.integer(res[[k]]); k<-k+1;
  mc[["zCs"]]  <-as.numeric(res[[k]]); k<-k+1;#size bin cutpoints
  mc[["zBs"]]  <-(mc[["zCs"]][2:(mc[["nZBs"]]+1)]+mc[["zCs"]][1:mc[["nZBs"]]])/2.0;
  mc[["nFsh"]] <-as.integer(res[[k]]); k<-k+1;
  mc[["fshNames"]]<-character(mc$nFsh);
  for (f in 1:mc$nFsh) {mc[["fshNames"]][f]<-res[[k]];  k<-k+1;}
  mc[["nSrv"]] <- as.integer(res[[k]]); k<-k+1;
  mc[["srvNames"]]<-character(mc$nSrv);
  for (s in 1:mc$nSrv) {mc[["srvNames"]][s]<-res[[k]];  k<-k+1;}
  #--set dim info in the global environment (using "<<-")
  .DIMS[["Y"]]<<-mc[["mxYr"]]-mc[["mnYr"]]; .DIMNAMES[["Y"]]<<-as.character(mc[["mnYr"]]:mc[["mxYr"]]);
  .DIMS[["Z"]]<<-mc[["nZBs"]];              .DIMNAMES[["Z"]]<<-as.character(mc[["zBs"]]);
  .DIMS[["F"]]<<-mc[["nFsh"]];              .DIMNAMES[["F"]]<<-mc[["fshNames"]];
  .DIMS[["V"]]<<-mc[["nSrv"]];              .DIMNAMES[["V"]]<<-mc[["srvNames"]];
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

  #--read fn_datasets
  topDir<-"./";
  if (exists("fn")) topDir<-dirname(fn);
  cat(paste0("--topDir = '",topDir,"'\n"));
  datasets<-readTCSAM_Datasets(file.path(topDir,mc$fn_datasets));

  close(conn);
  return(list(mc=mc,datasets=datasets));
}#--readTCSAM_ModelConfiguration


#'
#' @title Read model datasets file for a TCSAM02 model run
#'
#' @description Function to read model datasets file for a TCSAM02 model run.
#'
#' @param fn - file name for model datasets file
#' @param verbose - flag to print info
#'
#' @return list with input data sets.
#'
#' @details None.
#'
#' @export
#'
readTCSAM_AllDatasets<-function(conn){
  if (!inherits(conn,"connection")){
    if (!file.exists(conn)){
      msg<-paste0("--Error reading TCSAM datasets file.\n",
                  "--File '",conn,"' does not exist. Aborting!\n\n")
      stop(msg);
    }
    fn<-conn;
    cat(paste0("--Reading model datasets file '",fn,"'\n"))
    conn<-file(fn,open="r");
  } else {
    if (!isOpen(conn)) open(conn,open="r");
  }

  #--read file
  ds<-readTCSAM_DatasetNames(conn);

  #--paths are relative to datasets file directory
  topDir<-"./";
  if (exists("fn")) topDir<-dirname(fn);
  cat(paste0("--topDir = '",topDir,"'\n"));

  #--read biological data file
  bd<-readTCSAM_BioData(file.path(topDir,ds[["fn_BioInfo"]]));

  #--read fishery datasets
  fds<-list();
  if (ds$nFshs>0) for (i in 1:ds$nFshs) {
    fnp<-ds[["fn_Fshs"]][i];
    fds[[fnp]]<-readTCSAM_FleetData(file.path(topDir,fnp));
  }

  #--read survey datasets
  sds<-list();
  if (ds$nSrvs>0) for (i in 1:ds$nSrvs) {
    fnp<-ds[["fn_Srvs"]][i];
    sds[[fnp]]<-readTCSAM_FleetData(file.path(topDir,fnp));
  }

  #--read molt increment data files
  mids<-list();
  if (ds$nMIDs>0) for (i in 1:ds$nMIDs) {
    fnp<-ds[["fn_MIDs"]][i];
    mids[[fnp]]<-readTCSAM_GrowthData(file.path(topDir,fnp));
  }

  #--read chela height data files
  # mids<-list();
  # if (ds$nMIDs>0) for (i in 1:ds$nMIDs) {
  #   fnp<-ds[["fn_MIDs"]][i];
  #   mids[[fnp]]<-readTCSAM_ChelaHeightData(file.path(topDir,fnp));
  # }

  #--read maturity ogive data files
  mods<-list();
  if (ds$nMODs>0) for (i in 1:ds$nMODs) {
    fnp<-ds[["fn_MODs"]][i];
    mods[[fnp]]<-readTCSAM_MaturityOgiveData(file.path(topDir,fnp));
  }

  close(conn);
  return(list(info=ds,
              bio_datasets=bd,
              fishery_datasets=fds,
              survey_datasets=sds,
              molt_increment_datsets=mids,
              maturity_ogive_datasets=mods));
}#--readTCSAM_Datasets

#'
#' @title Read model datasets file for a TCSAM02 model run
#'
#' @description Function to read model datasets file for a TCSAM02 model run.
#'
#' @param fn - file name for model datasets file
#' @param verbose - flag to print info
#'
#' @return list with input data sets.
#'
#' @details None.
#'
#' @export
#'
readTCSAM_DatasetNames<-function(conn){
  if (!inherits(conn,"connection")){
    if (!file.exists(conn)){
      msg<-paste0("--Error reading TCSAM datasets file.\n",
                  "--File '",conn,"' does not exist. Aborting!\n\n")
      stop(msg);
    }
    fn<-conn;
    cat(paste0("--Reading model datasets file '",fn,"'\n"))
    conn<-file(fn,open="r");
  } else {
    if (!isOpen(conn)) open(conn,open="r");
  }

  #--read file
  ds<-list();
  strv<-readLines(conn);
  res  <-wtsUtilities::parseText(strv);
  k<-1;
  ds[["fn_BioInfo"]]  <-res[[k]]; k<-k+1;
  ds[["nFshs"]]       <-as.integer(res[[k]]); k<-k+1;
  for (i in 1:ds$nFshs) {ds[["fn_Fshs"]][i]<-res[[k]];  k<-k+1;}
  ds[["nSrvs"]]       <-as.integer(res[[k]]); k<-k+1;
  for (i in 1:ds$nSrvs) {ds[["fn_Srvs"]][i]<-res[[k]];  k<-k+1;}
  ds[["nMIDs"]]       <-as.integer(res[[k]]); k<-k+1;
  if (ds$nMIDs>0) for (i in 1:ds$nMIDs) {ds[["fn_MIDs"]][i]<-res[[k]];  k<-k+1;}
  ds[["nCHDs"]]       <-as.integer(res[[k]]); k<-k+1;
  if (ds$nCHDs>0) for (i in 1:ds$nCHDs) {ds[["fn_CHDs"]][i]<-res[[k]];  k<-k+1;}
  ds[["nMODs"]]       <-as.integer(res[[k]]); k<-k+1;
  if (ds$nMODs>0) for (i in 1:ds$nMODs) {ds[["fn_MODs"]][i]<-res[[k]];  k<-k+1;}

  return(ds);
}

#'
#' @title Read input files for a TCSAM02 model run
#'
#' @description Function to read input files for a TCSAM02 model run.
#'
#' @param fn - file name for model configuration file
#' @param verbose - flag to print info
#'
#' @return list with input files information
#'
#' @details None.
#'
#' @export
#'
readTCSAM_InputFiles<-function(fn,
                               verbose=TRUE){
  if (!file.exists(fn)){
    msg<-paste0("--Error reading TCSAM input files.\n",
                "--File '",fn,"' does not exist. Aborting!\n\n")
    stop(msg);
  }

  conn<-file(fn,open="r");
  mc<-readTCSAM_ModelConfiguration(conn);
  close(conn);

  retutn(mc);
}


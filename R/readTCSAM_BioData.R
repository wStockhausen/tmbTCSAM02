readTCSAM_BioData<-function(conn){
  if (!inherits(conn,"connection")){
    fn<-conn;
    if (!file.exists(conn)){
      msg<-paste0("--Error reading TCSAM bio data file.\n",
                  "--File '",conn,"' does not exist. Aborting!\n\n")
      stop(msg);
    }
    cat(paste0("--Reading model bio data file '",conn,"'\n"))
    conn<-file(conn,open="r");
  } else {
    if (!isOpen(conn)) open(conn,open="r");
  }

  #--read file
  lst<-list();
  strv<-readLines(conn);
  res  <-wtsUtilities::parseText(strv);
  k<-1;
  keyword<-res[[k]]; k<-k+1;
  if (toupper(keyword)!="BIO_DATA")
  lst[["rec_lag"]]  <-as.numeric(res[[k]]); k<-k+1;
  lst[["nZBs"]]     <-as.integer(res[[k]]); k<-k+1;
  lst[["zBs"]]       <-as.numeric(res[[k]]); k<-k+1;
  if (any(lst$zBs!=as.numeric(.DIMNAMES[["Z"]]))){
    msg<-paste0("--Error in bio data file '",fn,"'\n",
                "--input size bins do not match model size bins!\n",
                "--input size bins: ",paste(lst[["zBs"]],collapse=", "),"\n",
                "--model size bins: ",paste(.DIMNAMES[["Z"]],collapse=", "),"\n");
    warning(msg);
  }
  #--weight-at-size
  lst[["wAtZ_units"]]<-res[[k]]; k<-k+1;            #units
  lst[["wAtZ_nFCs"]]<-as.integer(res[[k]]); k<-k+1; #number of factor combinations
  wAtZ<-array(dim=c(.DIMS[["X"]],.DIMS[["M"]],.DIMS[["S"]],.DIMS[["Z"]]),
              dimnames=list(X=.DIMNAMES[["X"]],
                            M=.DIMNAMES[["M"]],
                            S=.DIMNAMES[["S"]],
                            Z=.DIMNAMES[["Z"]]))
  for (i in 1:lst[["wAtZ_nFCs"]]){
    fc<-res[[k]]; k<-k+1;
    v <-as.numeric(res[[k]]); k<-k+1;
    for (s in .DIMNAMES[["S"]]) wAtZ[fc[1],fc[2],s,]<-v;
  }
  lst[["wAtZ"]]<-wAtZ;
  lst[["typicalFshTime"]] <-as.numeric(res[[k]]); k<-k+1;
  lst[["typicalMateTime"]]<-as.numeric(res[[k]]); k<-k+1;
  lst[["nATs"]]           <-as.integer(res[[k]]); k<-k+1;#number of atypical years
  if (lst[["nATs"]]>0){
    for (i in 1:lst[["nATs"]]){
      v <-as.numeric(res[[k]]); k<-k+1;
    }
  }

  close(conn);
  return(lst);
}#--readTCSAM_BioData


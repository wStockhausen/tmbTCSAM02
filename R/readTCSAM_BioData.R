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
  if (toupper(keyword)!="BIO_DATA") stop(paste0("Keyword 'BIO_DATA' is misssing from purported bio data file ",conn,"\n"));
  lst[["rec_lag"]]  <-as.numeric(res[[k]]); k<-k+1;
  lst[["nZBs"]]     <-as.integer(res[[k]]); k<-k+1;
  lst[["zBs"]]       <-as.numeric(res[[k]]); k<-k+1;
  if (any(lst$zBs!=as.numeric(.DIMNAMES[["z"]]))){
    msg<-paste0("--Error in bio data file '",fn,"'\n",
                "--input size bins do not match model size bins!\n",
                "--input size bins: ",paste(lst[["zBs"]],collapse=", "),"\n",
                "--model size bins: ",paste(.DIMNAMES[["z"]],collapse=", "),"\n");
    warning(msg);
  }
  #--weight-at-size
  lst[["wAtZ_units"]]<-res[[k]]; k<-k+1;            #units
  lst[["wAtZ_nFCs"]]<-as.integer(res[[k]]); k<-k+1; #number of factor combinations
  wAtZ<-array(dim=c(.DIMS[["x"]],.DIMS[["m"]],.DIMS[["s"]],.DIMS[["z"]]),
              dimnames=list(X=.DIMNAMES[["x"]],
                            M=.DIMNAMES[["m"]],
                            S=.DIMNAMES[["s"]],
                            Z=.DIMNAMES[["z"]]))
  for (i in 1:lst[["wAtZ_nFCs"]]){
    fc<-res[[k]]; k<-k+1;
    v <-as.numeric(res[[k]]); k<-k+1;
    for (s in .DIMNAMES[["s"]]) wAtZ[fc[1],fc[2],s,]<-v;
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


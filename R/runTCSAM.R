#'
#' @title Run a TCSAM02 model
#'
#' @description Function to make a TCSAM02 model run.
#'
#' @param fn - file name for model configuration file
#' @param compile - flag to compile tmbTCSAM dynamic library
#' @param verbose - flag to print info
#'
#' @return ??
#'
#' @details None.
#'
#' @export
#'
runTCSAM<-function(fn_mc,
                   compile=TRUE,
                   verbose=FALSE){

  #read the input files
  if (TRUE){
    library(tmbTCSAM02);
    fn_mc="~/Programming/R/GitPackages/tmbTCSAM02/inst/example/M19F03.Configuration.inp";
    compile=TRUE;
    verbose=FALSE;
    source("R/setGlobals.R")
    setGlobals();
    mc<-readTCSAM_ModelConfiguration(fn_mc,
                                     verbose=verbose);
  }

  #set up tmbTCSAM inputs
  dims_zsmxray<-c(.DIMS[["z"]],
                  .DIMS[["s"]],
                  .DIMS[["m"]],
                  .DIMS[["x"]],
                  .DIMS[["r"]],
                  .DIMS[["a"]],
                  .DIMS[["y"]]);
  dimnames_zsmxray<-list(z=.DIMNAMES[["z"]],
                         s=.DIMNAMES[["s"]],
                         m=.DIMNAMES[["m"]],
                         x=.DIMNAMES[["x"]],
                         r=.DIMNAMES[["r"]],
                         a=.DIMNAMES[["a"]],
                         y=.DIMNAMES[["y"]]);
  testArray_zsmxray<-array(data=1:prod(dims_zsmxray),dim=dims_zsmxray,dimnames=dimnames_zsmxray);
  inp_mc<-list(scenario=mc$mc$case,
               dims=dims_zsmxray,
               mnYr=mc$mc$mnYr,
               mxYr=mc$mc$mxYr,
               age_a=0:(length(.DIMNAMES[["a"]])-1),
               yrs_y=as.numeric(.DIMNAMES[["y"]]),
               zBs_z=as.numeric(.DIMNAMES[["z"]]),
               zCs_z=mc$mc$zCs,
               nFsh=mc$mc$nFsh,
               nSrv=mc$mc$nSrv);
  data<-list(debug=1,
             stMC=inp_mc,
             testArray_zsmxray=testArray_zsmxray,
             recInfo=
             );
  params<-list(#pRec_MnLnR=0,
               pZ50=100,
               pSlp=0.1
              );
  map.params<-list();

  #compile (if necessary) and load dynamic library
  if (compile){
    objFile<-"inst/tmbTCSAM/src/tmbTCSAM.o";
    if (file.exists(objFile)) file.remove(objFile);
    if (TMB::compile("inst/tmbTCSAM/src/tmbTCSAM.cpp")!=0){
      msg<-"Could not compile code.\n";
      stop(msg);
    }
  }
  dyn.load(TMB::dynlib("inst/tmbTCSAM/src/tmbTCSAM"));

  #create the objective function
  objFun<-TMB::MakeADFun(data=data,
                         parameters=params,
                         map=map.params,
                         DLL="tmbTCSAM",
                         checkParameterOrder=TRUE);

  #optimize the parameters
  system.time(opt   <- TMBhelper::Optimize(objFun));

  if (is.loaded("tmbTCSAM")) dyn.unload("tmbTCSAM");
}

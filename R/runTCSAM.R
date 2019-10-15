runTCSAM<-function(mcFile,
                   compile=TRUE){
  #set up TCSAM inputs
  data<-list(zBs_z=seq(27.5,182.5,5)
             );
  params<-list(pZ50=100,
               pSlp=0.1
              );
  map.params<-list();

  #compile (if necessary) and load dynamic library
  if (compile){
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
}

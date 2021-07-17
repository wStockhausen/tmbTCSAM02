#----------testing-----------
#require(tmbTCSAM02);
fns = list.files(path="./R",pattern="*.R",full.names=TRUE);
for (fn in fns) source(fn);

setGlobals();
ls(all.names=TRUE);

#fn_mc = "./inst/exampleOld/M21.13.MCI.inp";
fn_mc = "./inst/example21_13/M21.13.MCI.inp";
mc = readTCSAM_ModelConfiguration(fn_mc);

#--read fn_datasets
topDir = dirname(fn_mc);
cat(paste0("--topDir = '",topDir,"'\n"));
ds = readTCSAM_DatasetNames(file.path(topDir,mc$fn_datasets));

#--read datasets
  #----read biological data file
  bd = readTCSAM_BioData(file.path(topDir,ds[["fn_BioInfo"]]));

  #----read fishery datasets
  fds = list();
  if (ds$nFshs>0) for (i in 1:ds$nFshs) {
    fnp = ds[["fn_Fshs"]][i];
    fds[[fnp]] = readTCSAM_FleetData(file.path(topDir,fnp),verbose=TRUE);
  }

  #----read survey datasets
  sds = list();
  if (ds$nSrvs>0) for (i in 1:ds$nSrvs) {
    fnp = ds[["fn_Srvs"]][i];
    sds[[fnp]] = readTCSAM_FleetData(file.path(topDir,fnp));
  }

  #----read molt increment data files
  mids = list();
  if (ds$nMIDs>0) for (i in 1:ds$nMIDs) {
    fnp = ds[["fn_MIDs"]][i];
    mids[[fnp]] = readTCSAM_GrowthData(file.path(topDir,fnp));
  }

  #----read chela height data files
  # mids = list();
  # if (ds$nMIDs>0) for (i in 1:ds$nMIDs) {
  #   fnp = ds[["fn_MIDs"]][i];
  #   mids[[fnp]] = readTCSAM_ChelaHeightData(file.path(topDir,fnp));
  # }

  #--read maturity ogive data files
  mods = list();
  if (ds$nMODs>0) for (i in 1:ds$nMODs) {
    fnp = ds[["fn_MODs"]][i];
    mods[[fnp]] = readTCSAM_MaturityOgiveData(file.path(topDir,fnp));
  }

#--read fn_paramsInfo
topDir = "./";
if (exists("fn")) topDir = dirname(fn);
cat(paste0("--topDir = '",topDir,"'\n"));
paramsInfo = readTCSAM_ModelParametersInfo(file.path(topDir,mc$fn_paramsInfo));


tst = readTCSAM_ModelParametersInfo(file.path(dirname(fn_mc),mc$mc$fn_paramsInfo));

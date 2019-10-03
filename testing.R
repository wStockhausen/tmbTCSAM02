#----------testing-----------
require(tmbTCSAM02);
setGlobals();
ls(all.names=TRUE);

fn_mc<-"./inst/example/M19F03.Configuration.inp";
mc1<-readTCSAM_ModelConfiguration(fn_mc);
fn_ds<-file.path(dirname(fn_mc),mc1$fn_datasets);
fn_mid<-"./inst/example/M19F01.Data.2019.Growth.EBS.inp";
mid<-readTCSAM_GrowthData(fn_mid);

tst<-readTCSAM_ModelParametersInfo(file.path(dirname(fn_mc),mc1$mc$fn_paramsInfo));

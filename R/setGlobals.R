#'
#' @title Set global values
#'
#' @description Function to set global values, like key/value maps and dimensions.
#'
#' @details Must be called prior to any other functions. The following global values are created:
#' \itemize{
#'   \item{.KV_MAP - list of named value vectors with keys as the associated names}
#'   \item{.DIMS - list of model dimensions}
#'   \item{.DIMNAMES - list of names associated with the model dimensions}
#' }
#'
#' @export
#'
setGlobals<-function(){
  #--define keywords
  KEYWORDS<-list();
  KEYWORDS[["FLEET_TYPES"]]                <-c("SURVEY","FISHERY");
  KEYWORDS[["AGGREGATED_CATCH_DATA_TYPES"]]<-c("AGGREGATE_ABUNDANCE","AGGREGATE_BIOMASS");
  KEYWORDS[["SIZE_FREQUENCY_DATA_TYPES"]]  <-"SIZE_FREQUENCY_DATA";
  KEYWORDS[["EFFORT_DATA_TYPES"]]          <-"EFFORT_DATA";
  .KEYWORDS<<-KEYWORDS;#make global variable

  #--define key/value maps
  #----the values are defined as a named vector
  #----the keys are the names associated with the values
  KV_MAP<-list();
  KV_MAP[["ON_OFF"]]<-c(TRUE,FALSE);
  names(KV_MAP[["ON_OFF"]])<-c("ON","OFF");
  .KV_MAP<<-KV_MAP;#make global variable

  DIMS<-list();   DIMNAMES<-list();
  DIMS[["y"]]<-1; DIMNAMES[["y"]]<-"UNDETERMINED";   #model years
  DIMS[["r"]]<-1; DIMNAMES[["r"]]<-"EBS";            #model regions
  DIMS[["a"]]<-1; DIMNAMES[["a"]]<-"UNDETERMINED";   #model age classes
  DIMS[["x"]]<-3; DIMNAMES[["x"]]<-c("MALE",    "FEMALE",    "UNDETERMINED");#model sexes
  DIMS[["m"]]<-3; DIMNAMES[["m"]]<-c("IMMATURE", "MATURE",   "UNDETERMINED");#model maturity state
  DIMS[["s"]]<-3; DIMNAMES[["s"]]<-c("NEW SHELL","OLD SHELL","UNDETERMINED");#model shell conditions
  DIMS[["z"]]<-1; DIMNAMES[["z"]]<-"UNDETERMINED";   #model size bins
  .DIMS<<-DIMS;  .DIMNAMES<<-DIMNAMES;#make global variables

  return(NULL);
}

#'
#' @title Substitute value for key
#'
#' @description Function to substitute values for key words.
#'
#' @param ks - vector of key words
#' @param map_kv - map from keyword to value
#'
#' @details None.
#'
#' @export
#'
subForKey<-function(ks,map_kv){
  res<-map_kv[ks];
  return(res);
}


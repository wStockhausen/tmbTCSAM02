#'
#' @title Parse a formatted time block
#'
#' @description Function to parse a formatted time block.
#'
#' @param str - the time block (as text) to parse
#' @param verbose - flag to print info
#'
#' @return numeric vector with parsed time block
#'
#' @details The input time block should follow one of the following formats:
#' \code{"1.23"}
#' \code{"100:110"}
#' \code{"[110:120]"}
#' \code{"[110:120; 130]"}
#' \code{"[110:120; 130:140]"}
#'
#' @export
#'
parseTimeBlock<-function(str,
                     verbose=FALSE){
  suppressWarnings(tb<-as.numeric(str));
  if (is.na(tb)){
    tb<-NULL;
    strp<-gsub("[","",str,fixed=TRUE);
    strp<-gsub("]","",strp,fixed=TRUE);
    vec<-strsplit(strp,";",fixed=TRUE)[[1]];
    n<-length(vec);
    for (i in 1:n){
      vec1<-strsplit(vec[i],":",fixed=TRUE)[[1]];
      if (length(vec1)>1){
        mn<-as.numeric(vec1[1]);
        if (mn==-1) mn<-as.numeric(.DIMNAMES[["y"]])[1];
        mx<-as.numeric(vec1[2]);
        if (mx==-1) mx<-as.numeric(.DIMNAMES[["y"]])[length(.DIMNAMES[["y"]])];
        if (mx==-2) mx<-as.numeric(.DIMNAMES[["y"]])[length(.DIMNAMES[["y"]])]+1;
        tb<-c(tb,mn:mx);
      } else if (length(vec1)==1){
        val<-as.numeric(vec1[1]);
        tb<-c(tb,val);
      } else {
        msg<-paste0("Error parsing time block '",str,"'.\n");
        warning(msg);
        return(NULL);
      }
    }
  }
  return(tb);
}

#' apply function for PCollection objects
#'
#' `r lifecycle::badge("stable")` \cr
#' This is the PatchMastR analog to "lapply" for PCollection objects
#'
#' @param X a \linkS4class{PCollection} object
#' @param FUN the function to be applied
#' @return A \linkS4class{PCollection} object
#' @param ReturnPMobject whether to return results as a \linkS4class{PCollection}  Default is \var{FALSE}, then returns a list
#' @exportMethod lapply
setMethod("lapply",
          "PCollection",
          function(X, FUN, ReturnPMobject=F){
            dat<-lapply(X@Series,FUN)
            if(ReturnPMobject){
              X@Series<-dat
               if(!validPCollection(X)){
                 stop(paste("Applying to PCollection", deparse(substitute(object)), "failed. No valid PCollection object returned"))
               }
              return(X)
            }else{
              dat<-t(as.data.frame(dat))
              rownames(dat)<-X@Names
              dat
            }
          }
)

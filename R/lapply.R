#' apply function for PMCollection objects
#'
#' This is the PatchMastR analog to "lapply" for PMCollection objects
#'
#' @param X a PMCollection object
#' @param FUN the function to be applied
#' @return A \link[=PMCollection]{PMCollection} object
#' @param ReturnPMCollection whether to return results as a PMCollection. Default is \var{FALSE}, then returns a list
#' @exportMethod lapply
setMethod("lapply",
          "PMCollection",
          function(X, FUN, ReturnPMCollection=F){
            dat<-lapply(X@Series,FUN)
            if(ReturnPMCollection){
              X@Series<-dat
               if(!PatchMasteR:::validPMCollection(X)){
                 stop(paste("Applying to PMCollection", deparse(substitute(object)), "failed. No valid PMCollection object returned"))
               }
              print("OK")
              return(X)
            }else{
              dat<-t(as.data.frame(dat))
              rownames(dat)<-X@Names
              dat
            }
          }
)

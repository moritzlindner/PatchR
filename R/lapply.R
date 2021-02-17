#' apply function for PMCollection objects
#'
#' This is the PatchMastR analog to "lapply" for PMCollection objects
#'
#' @param X a \linkS4class{PMCollection} object
#' @param FUN the function to be applied
#' @return A \linkS4class{PMCollection} object
#' @param ReturnPMObject whether to return results as a \linkS4class{PMCollection}  Default is \var{FALSE}, then returns a list
#' @exportMethod lapply
setMethod("lapply",
          "PMCollection",
          function(X, FUN, ReturnPMObject=F){
            dat<-lapply(X@Series,FUN)
            if(ReturnPMObject){
              X@Series<-dat
               if(!PatchMasteR:::validPMCollection(X)){
                 stop(paste("Applying to PMCollection", deparse(substitute(object)), "failed. No valid PMCollection object returned"))
               }
              return(X)
            }else{
              dat<-t(as.data.frame(dat))
              rownames(dat)<-X@Names
              dat
            }
          }
)

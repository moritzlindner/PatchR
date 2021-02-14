#' apply function for PMExperiment objects
#'
#' This is the PatchMastR analog to "lapply" for PMExperiment objects
#'
#' @param X a PMExperiment object
#' @param FUN the function to be applied
#' @return A \link[=PMExperiment]{PMExperiment} object
#' @param ReturnPMExperiment whether to return results as a PMExperiment. Default is \var{FALSE}, then returns a list
#' @exportMethod lapply
setMethod("lapply",
          "PMExperiment",
          function(X, FUN, ReturnPMExperiment=F){
            dat<-lapply(X@Series,FUN)
            if(ReturnPMExperiment){
              X@Series<-dat
               if(!PatchMasteR:::validPMExperiment(X)){
                 stop(paste("Applying to PMExperiment", deparse(substitute(object)), "failed. No valid PMExperiment object returned"))
               }
              print("OK")
              return(X)
            }else{
              as.data.frame(dat)
            }
          }
)

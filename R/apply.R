#' apply function for PMRecording objects
#'
#' This is the PatchMastR analog to "apply"
#'
#' @param X a PMRecording object
#' @param MARGIN a vector giving the subscripts which the function will be applied along. Understands "Time", "Sweep","Trace", or 1-3 resp.
#' @param FUN the function to be applied
#' @param ReturnPMObject whether to return results as a PMRecording with an additional, computed trace. Default is \var{FALSE}, then returns a matrix.
#' @return A matrix or \link[=PMRecording]{PMRecording} object
#' @exportMethod apply
setMethod("apply",
          "PMRecording",
          function(X,
                   MARGIN,
                   FUN,
                   ReturnPMObject=F){
            # translate Margins
            if(is.character(MARGIN)){
              MARG<-MARGIN
              if(MARGIN=="Time"){
                MARGIN<-1
              }
              if(MARGIN=="Sweep"){
                MARGIN<-2
              }
              if(MARGIN=="Trace"){
                MARGIN<-3
              }
            }else{
              if(MARGIN==1){
                MARG<-"Time"
              }
              if(MARGIN==2){
                MARG<-"Sweep"
              }
              if(MARGIN==3){
                MARG<-"Trace"
              }
            }
            print(paste("Function ",as.character(substitute(mean))[1]," applied along", MARG))

            #simplify and apply
            DAT<-simplify2array(X@Data)
            margins<-1:length(dim(DAT))
            out<-apply(DAT,margins[!(margins %in% MARGIN)],FUN)
            if(MARGIN==1){
              colnames(out)<-getTraceNames(X)
              rownames(out)<-getSweepNames(X)
            }
            if(MARGIN==2){
              out<-cbind(X@TimeTrace,out)
              colnames(out)<-c(paste0("Time [",X@TimeUnit,"]"),getTraceNames(X))
            }
            if(MARGIN==3){
              if(!ReturnPMObject){
                out<-cbind(X@TimeTrace,out)
                colnames(out)<-c(paste0("Time [",X@TimeUnit,"]"),getSweepNames(X))
              }else
                X<-addTrace(object=X,Trace=as.character(substitute(mean))[1],Unit="NA",mtx=out )
              out<-X
            }
            return(out)
          }
          )


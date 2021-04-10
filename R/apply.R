#' apply function for PRecording objects
#'
#' This is the PatchR analog to \code{apply}
#'
#' @param X A \linkS4class{PRecording}  object
#' @param MARGIN A vector giving the subscripts which the function will be applied along. Understands \code{Time}, \code{Sweep},\code{Trace}, or \code{1-3} resp.
#' @param FUN The function to be applied
#' @param ReturnPMObject Whether to return results as a \linkS4class{PRecording} with an additional, computed trace only works with \code{MARGIN="Trace"} or if applying \code{FUN} leaves dimensions unchanged. Default is \var{FALSE}, then returns a matrix.
#' @seealso \link[base:apply]{base::apply()}, \link[PatchR:lapply]{PatchR::lapply()}
#' @return A \link[base:matrix]{matrix}  or \linkS4class{PRecording} object.
#' @exportMethod apply
setMethod("apply",
          "PRecording",
          function(X,
                   MARGIN,
                   FUN,
                   ReturnPMObject=F,
                   Verbose=T){
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
            if(Verbose){message(paste("Function ",paste(eval(deparse(FUN)),collapse = '')," applied along", MARG))}

            #simplify and apply
            DAT<-simplify2array(X@Data)
            margins<-1:length(dim(DAT))
            out<-apply(DAT,margins[!(margins %in% MARGIN)],FUN)
            if(isTRUE(all.equal(dim(out),dim(DAT)))){
              dimnames(out)<-dimnames(DAT)
              if(ReturnPMObject){
                warning("Updating data in PRecording!")
                out<-lapply(seq(dim(out)[3]), function(y) out[ , , y])
                names(out)<-names(X@Data)
                X@Data<-out
                out<-X
              }
            }else{
              if(MARGIN==1){
                colnames(out)<-GetTraceNames(X)
                rownames(out)<-GetSweepNames(X)
              }
              if(MARGIN==2){
                out<-cbind(X@TimeTrace,out)
                colnames(out)<-c(paste0("Time [",X@TimeUnit,"]"),GetTraceNames(X))
              }
              if(MARGIN==3){
                if(!ReturnPMObject){
                  out<-cbind(X@TimeTrace,out)
                  colnames(out)<-c(paste0("Time [",X@TimeUnit,"]"),GetSweepNames(X))
                }else{
                  X<-AddTrace(object=X,Trace=as.character(substitute(mean))[1],Unit="NA",mtx=out )
                  out<-X
                }
              }
            }
            return(out)
          }
          )


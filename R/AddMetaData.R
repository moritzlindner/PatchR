
#' Adds metadata to a PRecording or PCollection object
#'
#' This function adds metadata to a \linkS4class{PRecording} or \linkS4class{PCollection} object by filling the \var{MetaData} slot. The function calling \code{AddMetaData} is stored in the \var{.MetaDataFx} slot.
#'
#' @param object A \linkS4class{PRecording} or \linkS4class{PCollection} object.
#' @param values The values to be added. Can be anything convertible into a \var{matrix}.
#' @param title The title(s) for the metadata column(s).
#' @return A \linkS4class{PRecording} or \linkS4class{PCollection} object, respectively.
#' @details
#' \strong{The MetaData slot} \cr
#'  In a \var{PRecording} object the \var{MetaData} slot is a \var{matrix} with each row corresponding to a sweep. \cr
#'  In a \var{PCollection} object the \var{MetaData} slot is a \var{matrix} with each row corresponding to a \var{PRecording} stored in the object.
#'  Column names must be unique.
#' @seealso \linkS4class{PRecording}, \linkS4class{PCollection}, \link[PatchR:apply]{PatchR::apply()}, \link[=lapply]{PatchR::lapply()}, \link[base::as.matrix()]{as.matrix()}
#' @exportMethod AddMetaData
setGeneric(name="AddMetaData",
           def=function(object,
                        values,
                        title=colnames(values),
                        Verbose=T)
           {
             standardGeneric("AddMetaData")
           }
)
setMethod("AddMetaData",
          "PRecording",
          function(object,
                   values,
                   title=colnames(values),
                   Verbose=T){

            if(any(duplicated(title))){
              stop("Duplicate MetaData names not allowed")
            }
            if(any(title %in% colnames(object@MetaData))){
              stop("MetaData names already in use")
            }
            values<-as.matrix(values)
            colnames(values)<-title
            if(Verbose){message("Adding metadata column(s) ",title)}
            if(all(dim(object@MetaData)==0)){
              object@MetaData<-values
              colnames(object@MetaData)<-as.vector(title)
            }else{
              colnames(object@MetaData)<-cbind(rownames(object@MetaData),title)
            }

            if(!PatchR:::validPRecording(object)){
              stop(paste("Adding Metadata to PRecording", deparse(substitute(object)), "failed. incorrect dimensison"))
            }
            object
          }
)


#' @exportMethod AddMetaData
setMethod("AddMetaData",
          "PCollection",
          function(object,
                   values,
                   title=colnames(values),
                   Verbose=T){

            if(any(duplicated(title))){
              stop("Duplicate MetaData names not allowed")
            }
            if(any(title %in% colnames(object@MetaData))){
              stop("MetaData names already in use")
            }

            values<-as.matrix(values)
            colnames(values)<-title
            if(Verbose){message("Adding metadata column(s) ",title)}
            if(all(dim(object@MetaData)==0)){
              object@MetaData<-values
              object@.MetaDataFx[[1]]<-sys.calls()[[1]]
            }else{
              object@MetaData<-cbind(object@MetaData,values)
              object@.MetaDataFx<-append(object@.MetaDataFx,sys.calls()[[1]])
            }

            if(!PatchR:::validPCollection(object)){
              stop(paste("Adding Metadata to PCollection", deparse(substitute(object)), "failed. incorrect dimensison"))
            }
            object
          }
)

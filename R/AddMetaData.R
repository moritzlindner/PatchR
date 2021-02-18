setGeneric(name="AddMetaData",
           def=function(object,
                        values,
                        title=colnames(values))
           {
             standardGeneric("AddMetaData")
           }
)
#' Adds metadata to an PCollection object
#'
#' This function adds metadata to an PCollection object. The MetaData slot is a matrix with one row per sweep in PRecording and one row per Series in PCollections. The calling function is stored in the .MetaDataFx slot.
#'
#' @param object A \link[=PRecording]{PRecording} or \link[=PCollection]{PCollection} object
#' @param values to be added
#' @param title title for metadata column(s).
#' @return A \link[=PRecording]{PRecording} or \link[=PCollection]{PCollection}  object, respectively
#' @exportMethod AddMetaData
setMethod("AddMetaData",
          "PRecording",
          function(object,
                   values,
                   title=colnames(values)){

            if(any(duplicated(title))){
              stop("Duplicate MetaData names not allowed")
            }
            if(any(title %in% colnames(object@MetaData))){
              stop("MetaData names already in use")
            }
            values<-as.matrix(values)
            colnames(values)<-title
            message("Adding metadata column(s) ",title)
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
                   title=colnames(values)){

            if(any(duplicated(title))){
              stop("Duplicate MetaData names not allowed")
            }
            if(any(title %in% colnames(object@MetaData))){
              stop("MetaData names already in use")
            }

            values<-as.matrix(values)
            colnames(values)<-title
            message("Adding metadata column(s) ",title)
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

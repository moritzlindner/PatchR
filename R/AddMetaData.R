setGeneric(name="AddMetaData",
           def=function(object,
                        values,
                        title=colnames(values))
           {
             standardGeneric("AddMetaData")
           }
)
#' Adds metadata to an PMExperiment object
#'
#' This function adds metadata to an PMExperiment object. The MetaData slot is a matrix with one row per sweep in PMSeries and one row per Series in PMExperiments. The calling function is stored in the .MetaDataFx slot.
#'
#' @param object A \link[=PMSeries]{PMSeries} or \link[=PMExperiment]{PMExperiment} object
#' @param values to be added
#' @param title title for metadata column(s).
#' @return A \link[=PMSeries]{PMSeries} or \link[=PMExperiment]{PMExperiment}  object, respectively
#' @exportMethod AddMetaData
setMethod("AddMetaData",
          "PMSeries",
          function(object,
                   values,
                   title=colnames(values)){

            if(any(duplicated(title))){
              stop("Duplicate MetaData names not allowed")
            }
            if(any(title %in% colnames(object@MetaData))){
              stop("MetaData names already in use")
            }

            print(sys.calls()[[1]])
            values<-as.matrix(values)
            colnames(values)<-title
            cat("adding metadata column(s)",title)
            if(all(dim(object@MetaData)==0)){
              object@MetaData<-values
              colnames(object@MetaData)<-as.vector(title)
            }else{
              colnames(object@MetaData)<-cbind(rownames(object@MetaData),title)
            }

            if(!PatchMasteR:::validPPMSeries(object)){
              stop(paste("Adding Metadata to PMSeries", deparse(substitute(object)), "failed. incorrect dimensison"))
            }
            object
          }
)


#' @exportMethod AddMetaData
setMethod("AddMetaData",
          "PMExperiment",
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
            print(values)
            cat("adding metadata column(s)",title)
            if(all(dim(object@MetaData)==0)){
              object@MetaData<-values
              object@.MetaDataFx[[1]]<-sys.calls()[[1]]
            }else{
              object@MetaData<-cbind(object@MetaData,values)
              object@.MetaDataFx<-append(object@.MetaDataFx,sys.calls()[[1]])
            }

            if(!PatchMasteR:::validPMExperiment(object)){
              stop(paste("Adding Metadata to PMExperiment", deparse(substitute(object)), "failed. incorrect dimensison"))
            }
            object
          }
)

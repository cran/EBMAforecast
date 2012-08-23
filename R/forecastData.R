#' An ensemble forecasting data object
#'
#' Objects of class \code{ForecastData} are used in the \code{calibrateEnsemble} function. Datasets should be converted into an object of class \code{ForecastData} using the \code{makeForecastData} function. Individual slots of the \code{ForecastData} object can be accessed and changed using the \code{get} and \code{set} functions respectively. Missing observations in the prediction calibration set are allowed.
#'
#'
#' A data object of the class 'ForecastData' has the following slots: 
#' \itemize{
#' \item \code{predCalibration} An array containing the predictions of all component models for the observations in the calibration period.
#' \item \code{predTest} An array containing the predictions of all component models for all the observations in the test period.
#' \item \code{outcomeCalibration} A vector containing the true values of the dependent variable for all observations in the calibration period. 
#' \item \code{outcomeTest} A vector containing the true values of the dependent variable for all observations in the test period.
#' \item \code{modelNames} A character vector containing the names of all component models. }
#'
#' @author  Michael D. Ward <\email{michael.d.ward@@duke.edu}> and Jacob M. Montgomery <\email{jacob.montgomery@@wustl.edu}> and Florian M. Hollenbach <\email{florian.hollenbach@@duke.edu}>  
#'
#' @references Montgomery, Jacob M., Florian M. Hollenbach and Michael D. Ward. (2012). Improving Predictions Using Ensemble Bayesian Model Averaging. \emph{Political Analysis}. \bold{20}: 271-291.
#'
#' @examples
#' \dontrun{
#' data(calibrationSample)
#' 
#' data(testSample) 
#' this.ForecastData <- makeForecastData(.predCalibration=calibrationSample[,c("LMER", "SAE", "GLM")],
#' .outcomeCalibration=calibrationSample[,"Insurgency"],.predTest=testSample[,c("LMER", "SAE", "GLM")],
#' .outcomeTest=testSample[,"Insurgency"], .modelNames=c("LMER", "SAE", "GLM"))
#' 
#' ### to acces individual slots in the ForecastData object
#' getPredCalibration(this.ForecastData)
#' getOutcomeCalibration(this.ForecastData)
#' getPredTest(this.ForecastData)
#' getOutcomeTest(this.ForecastData)
#' getModelNames(this.ForecastData)
#' 
#' ### to assign individual slots, use set functions
#'
#' setPredCalibration(this.ForecastData)<-calibrationSample[,c("LMER", "SAE", "GLM")]
#' setOutcomeCalibration(this.ForecastData)<-calibrationSample[,"Insurgency"]
#' setPredTest(this.ForecastData)<-testSample[,c("LMER", "SAE", "GLM")]
#' setOutcomeTest(this.ForecastData)<-testSample[,"Insurgency"]
#' setModelNames(this.ForecastData)<-c("LMER", "SAE", "GLM")
#' }
#'
#' @seealso ensembleBMA
#' @aliases ForecastData-class initialize,ForecastData-method setPredCalibration,ForecastData-method setOutcomeCalibration,ForecastData-method setPredTest,ForecastData-method setOutcomeTest,ForecastData-method setModelNames,ForecastData-method makeForecastData,ANY-method print,ForecastData-method setModelNames<-,ForecastData-method setOutcomeCalibration<-,ForecastData-method setOutcomeTest<-,ForecastData-method setPredCalibration<-,ForecastData-method setPredTest<-,ForecastData-method show,ForecastData-method  getModelNames,ForecastData-method getOutcomeCalibration,ForecastData-method getOutcomeTest,ForecastData-method getPredCalibration,ForecastData-method getPredTest,ForecastData-method
#' @rdname ForecastData	
#' @export
setClass(Class="ForecastData",
         representation = representation(
           predCalibration="array",
           predTest="array",
           outcomeCalibration="numeric",
           outcomeTest="numeric",
           modelNames="character"),
         prototype=prototype(
           predCalibration=array(NA, dim=c(0,0,0)),
           predTest=array(NA, dim=c(0,0,0)), 
           outcomeCalibration=numeric(),
           outcomeTest=numeric(),
           modelNames=character()),
         validity=function(object){
         	if(length(object@predCalibration)>0 | length(object@predTest)>0 ){
           		if(nrow(object@predCalibration)!=length(object@outcomeCalibration))
             	{stop("The number of predictions and outcomes do not match in the calibration set.")}
           	}
            if(length(object@predTest)>0 | length(object@outcomeTest)>0){ 
           		if(nrow(object@predTest)!=length(object@outcomeTest))
             	{warning("The number of predictions and outcomes do not match in the test set.", call.=FALSE)}
           	} 
           	if(length(object@predTest)>0){
             	if(ncol(object@predTest)!=ncol(object@predCalibration))
               	{stop("The number of prediction models in the calibration and test set are different.")}    
             	if(dim(object@predTest)[3]!=dim(object@predCalibration)[3])
               	{stop("The number of exchangeable draws per model in the calibration and test are different.")}
           	}
           	if(sum(is.na(object@outcomeCalibration)) > 0)
             {stop("There are NAs in the outcome calibration set, these observations should be deleted from the data.")}
           	if(sum(is.na(object@outcomeTest)) > 0)
             {stop("There are NAs in the outcome test set, these observations should be deleted from the data.")}
            }  
)


##
#' @export
setMethod("initialize", "ForecastData", function(.Object, ...) {
  value = callNextMethod()
  validObject(value)
  return(value)
})


#' @export
setClass(Class="ForecastDataLogit",
         contains="ForecastData",
         validity=function(object){
           if(any(object@outcomeCalibration!=1 & object@outcomeCalibration!=0 & !is.na(object@outcomeCalibration))) 
             {stop("The outcomes for the binary model should be either 0 or 1 (Not true for outcome calibration set).")}	
           if(any(object@outcomeTest!=1 & object@outcomeTest!=0 & !is.na(object@outcomeTest))) 
             {stop("The outcomes for the binary model should be either 0 or 1 (Not true for outcome test set).")}	
           if(any(object@predCalibration<0 & !is.na(object@predCalibration)) | (any(object@predCalibration>1 & !is.na(object@predCalibration))))                 {stop("The predictions for the binary model should be between 0 or 1 (Not true for prediction calibration set).")}	
           if(any(object@predTest<0 & !is.na(object@predTest)) | any(object@predTest>1 & !is.na(object@predTest)) )
             {stop("The predictions for the binary model should be between 0 or 1 (Not true for prediction test set).")}	
           if(any(object@predCalibration==0, na.rm=TRUE) | any(object@predCalibration==1, na.rm=TRUE)) 
             {stop("The predictions for the binary model cannot be exactly 0 or 1 (Not true for prediction calibration set).")}	
           if(any(object@predTest==0, na.rm=TRUE) | any(object@predTest==1, na.rm=TRUE)) 
             {stop("The predictions for the binary model cannot be exactly 0 or 1 (Not true for prediction test set).")}	
           
                 }
         )

#' @export
setClass(Class="ForecastDataNormal",
         contains="ForecastData")


##
setAs(from="ForecastData", to="ForecastDataLogit",
      def=function(from){
        new("ForecastDataLogit",
            predCalibration=from@predCalibration,
            predTest=from@predTest,
            outcomeCalibration=from@outcomeCalibration,
            outcomeTest=from@outcomeTest,
            modelNames=from@modelNames)
}
)

##
setAs(from="ForecastData", to="ForecastDataNormal",
      def=function(from){
        new("ForecastDataNormal",
            predCalibration=from@predCalibration,
            predTest=from@predTest,
            outcomeCalibration=from@outcomeCalibration,
            outcomeTest=from@outcomeTest,
            modelNames=from@modelNames)
      }
      )



#' @rdname ForecastData
#' @export
setGeneric("getPredCalibration",function(object="ForecastData") standardGeneric("getPredCalibration"))

#' @export
setMethod("getPredCalibration","ForecastData",function(object){
	return(object@predCalibration)
}
)

#' @rdname ForecastData
#' @export
setGeneric("getPredTest",function(object="ForecastData") standardGeneric("getPredTest"))
#' @export
setMethod("getPredTest","ForecastData",
	function(object){
		return(object@predTest)
		}
)


#' @rdname ForecastData
#' @export
setGeneric("getOutcomeCalibration",function(object="ForecastData") standardGeneric("getOutcomeCalibration"))
#' @export
setMethod("getOutcomeCalibration","ForecastData",
	function(object="ForecastData"){
		return(object@outcomeCalibration)
		}
)


#' @rdname ForecastData
#' @export
setGeneric("getOutcomeTest",function(object="ForecastData") standardGeneric("getOutcomeTest"))
#' @export
setMethod("getOutcomeTest","ForecastData",
	function(object="ForecastData"){
		return(object@outcomeTest)
		}
)


#' @rdname ForecastData
#' @export
setGeneric("getModelNames",function(object="ForecastData") standardGeneric("getModelNames"))
#' @export
setMethod("getModelNames","ForecastData",
	function(object="ForecastData"){
		return(object@modelNames)
		}
)

#' @rdname ForecastData
#' @export
setGeneric("setPredCalibration<-",function(object,value){standardGeneric("setPredCalibration<-")})

setReplaceMethod(
	f="setPredCalibration",
	signature="ForecastData",
	definition=function(object,value){
          if(class(value)=="data.frame"){value <- as.matrix(value)}
          if(class(value)=="matrix"){value <- array(value, dim=c(nrow(value), ncol(value), 1))}
          object@predCalibration = value
          validObject(object)
          return(object)
	}
)

#' @rdname ForecastData
#' @export
setGeneric("setPredTest<-",function(object,value){standardGeneric("setPredTest<-")})

setReplaceMethod(
	f="setPredTest",
	signature="ForecastData",
	definition=function(object,value){
          if(class(value)=="data.frame"){value <- as.matrix(value)}
          if(class(value)=="matrix"){value <- array(value, dim=c(nrow(value), ncol(value), 1))}
          object@predTest<- value
		validObject(object)
		return(object)
	}
)

#' @rdname ForecastData
#' @export
setGeneric("setOutcomeCalibration<-",function(object,value){standardGeneric("setOutcomeCalibration<-")})

setReplaceMethod(
	f="setOutcomeCalibration",
	signature="ForecastData",
	definition=function(object,value){
		object@outcomeCalibration <- value
		validObject(object)
		return(object)
	}
)

#' @rdname ForecastData
#' @export
setGeneric("setOutcomeTest<-",function(object,value){standardGeneric("setOutcomeTest<-")})

setReplaceMethod(
	f="setOutcomeTest",
	signature="ForecastData",
	definition=function(object,value){
		object@outcomeTest<-value
		validObject(object)
		return(object)
	}
)

#' @rdname ForecastData
#' @export
setGeneric("setModelNames<-",function(object,value){standardGeneric("setModelNames<-")})

setReplaceMethod(
	f="setModelNames",
	signature="ForecastData",
	definition=function(object,value){
		object@modelNames <-value
		colnames(object@predCalibration)<-value
		colnames(object@predTest)<-value
		validObject(object)
		return(object)
	}
)




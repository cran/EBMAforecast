	
#' @export
setClass(Class="ForecastData",
         representation = representation(
           predCalibration="matrix",
           predTest="matrix",
           outcomeCalibration="matrix",
           outcomeTest="matrix",
           modelNames="character"),
         prototype=prototype(
           predCalibration=matrix(NA, nrow=0, ncol=0),
           predTest=matrix(NA, nrow=0, ncol=0), 
           outcomeCalibration=matrix(NA, nrow=0, ncol=0),
           outcomeTest=matrix(NA, nrow=0, ncol=0),
           modelNames=character()),
         validity=function(object){
           if(nrow(object@predCalibration)!=nrow(object@outcomeCalibration))
             {stop("The number of predictions and outcomes do not match in the calibration set.")}
           if(nrow(object@predTest)!=nrow(object@outcomeTest))
             {stop("The number of predictions and outcomes do not match in the test set.")}  
           if(ncol(object@outcomeTest)>1 || ncol(object@outcomeCalibration)>1)
             {stop("The outcomes should be organized as a matrix with only one column.")}
            if(ncol(object@predTest)!=ncol(object@predCalibration))
             {stop("The number of prediction models in the calibration and test set are different.")}    
           if(sum(is.na(object@predCalibration[,])) > 0)
           	 {stop("There are NAs in the prediction calibration set, unfortunately the package does not work with NAs yet. Soon to come.")}
           if(sum(is.na(object@predTest[,])) > 0)
           	 {stop("There are NAs in the prediction test set, unfortunately the package does not work with NAs yet. Soon to come.")}
		   if(sum(is.na(object@outcomeCalibration[,])) > 0)
           	 {stop("There are NAs in the outcome calibration set, unfortunately the package does not work with NAs yet. Soon to come.")}
           if(sum(is.na(object@outcomeTest[,])) > 0)
           	 {stop("There are NAs in the outcome test set, unfortunately the package does not work with NAs yet. Soon to come.")}
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
         	for(i in 1:nrow(object@outcomeCalibration)){
         	if(object@outcomeCalibration[i,]!=1 & object@outcomeCalibration[i,]!=0)
         	{stop("The outcomes for the binary model should be either 0 or 1 (Not true for outcome calibration set).")}	
         	}
         	for(i in 1:nrow(object@outcomeTest)){
         	if(object@outcomeTest[i,]!=1 & object@outcomeTest[i,]!=0)
         	{stop("The outcomes for the binary model should be either 0 or 1 (Not true for outcome test set).")}	
         	}
         	if(min(object@predCalibration)<0 || max(object@predCalibration)>1)
         	{stop("The predictions for the binary model should be between 0 or 1 (Not true for prediction calibration set).")}	
         	if(min(object@predTest)<0 || max(object@predTest)>1)
         	{stop("The predictions for the binary model should be between 0 or 1 (Not true for prediction test set).")}	
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



#' Build a ensemble forecasting data object
#'
#' This function uses the user provided component model forecasts and dependent variable observations to create an object of class \code{ForecastData}, which can then be used to calibrate and fit the Ensemble.
#'
#' @param .predAll An n by p matrix of data.frame containing predictions, for both the calibration and test observations.
#' @param .outcomeAll A vector of length n, containing the true values of the dependent variable, for both the calibration and test observations.
#' @param .inOut A dichotomous vector of length n, where 1 indicates that the observation is in the test sample and 0 that it is in the calibration sample.
#' @param .predCalibration A matrix with the number of rows being the number of observations in the calibration period and a column with calibration period predictions for each model.
#' @param .predTest A matrix with the number of rows being the number of observations in the test period and a column with test period predictions for each model.
#' @param .outcomeCalibration A vector with the true values of the dependent variable for each observation in the calibration period.  
#' @param .outcomeTest A vector with the true values of the dependent variable for each observation in the test period.
#' @param .modelNames A vector of length p with the names of the component models.  
#' @param ... Additional arguments not implemented
#'
#' @return A data object of the class 'ForecastData' with the following slots: 
#' \item{predCalibration}{A matrix containing the predictions of all component models for all observations in the calibration period.} 
#' \item{predTest}{A matrix containing the predictions of all component models for all observations in the test period.}
#' \item{outcomeCalibration}{A vector containing the true values of the dependent variable for all observations in the calibration period.} 
#' \item{outcomeTest}{A vector containing the true values of the dependent variable for all observations in the test period.}
#' \item{modelNames}{A character vector containing the names of all component models.  If no model names are specified, names will be assigned automatically.}
#' @author  Michael D. Ward <\email{michael.d.ward@@duke.edu}> and Jacob M. Montgomery <\email{jacob.montgomery@@wustl.edu}>
#'
#' @references Montgomery, Jacob M., Florian M. Hollenbach and Michael D. Ward. (2012). Improving Predictions Using Ensemble Bayesian Model Averaging. \emph{Political Analysis}. Forthcoming.
#'
#' @examples  data(calibrationSample)
#' 
#' data(testSample) 
#' this.ForecastData <- makeForecastData(.predCalibration=calibrationSample[,c("LMER", "SAE", "GLM")],
#' .outcomeCalibration=calibrationSample[,"Insurgency"],.predTest=testSample[,c("LMER", "SAE", "GLM")],
#' .outcomeTest=testSample[,"Insurgency"], .modelNames=c("LMER", "SAE", "GLM"))
#' 
#' @seealso ensembleBMA, other functions
#' @aliases getModelNames,ForecastData-method getOutcomeCalibration,ForecastData-method getOutcomeTest,ForecastData-method getPredCalibration,ForecastData-method getPredTest,ForecastData-method initialize,ForecastData-method makeForecastData,ANY-method print,ForecastData-method setModelNames<-,ForecastData-method setOutcomeCalibration<-,ForecastData-method setOutcomeTest<-,ForecastData-method setPredCalibration<-,ForecastData-method setPredTest<-,ForecastData-method show,ForecastData-method ForeCastData-class
#' @rdname makeForecastData
#' @export
setGeneric(name="makeForecastData",
           def=function(.predAll=NULL,
             .outcomeAll=NULL,
             .inOut=NULL,
            .predCalibration=matrix(NA, nrow=0, ncol=0),
             .predTest=matrix(NA, nrow=0, ncol=0),
             .outcomeCalibration=matrix(NA, nrow=0, ncol=0),
            .outcomeTest=matrix(NA, nrow=0, ncol=0),
             .modelNames=character(),
             ...)
           {standardGeneric("makeForecastData")}
           )



#' @export
setMethod(f="makeForecastData",
          definition=function(
            .predAll,
            .outcomeAll,
            .inOut,
            .predCalibration,
            .predTest,
            .outcomeCalibration,
            .outcomeTest,
            .modelNames)
          {
            .predCalibration <- as.matrix(.predCalibration); .predTest <- as.matrix(.predTest)
            .outcomeCalibration <- as.matrix(.outcomeCalibration);   .outcomeTest <- as.matrix(.outcomeTest)
            if(!is.null(.predAll)){.predCalibration <- as.matrix(.predAll[.inOut==0,])
                                   .predTest <- as.matrix(.predAll[.inOut==1,])}
            if(!is.null(.outcomeAll)){.outcomeCalibration <- as.matrix(.outcomeAll[.inOut==0])
                                      .outcomeTest <- as.matrix(.outcomeAll[.inOut==1])}
            if(length(.modelNames)<ncol(.predCalibration)){
              .modelNames <- paste("Model", 1:ncol(.predCalibration))
            }
            colnames(.predCalibration) <- .modelNames; rownames(.predCalibration) <- 1:nrow(.predCalibration)
            if (length(.predTest)>0){
              colnames(.predTest) <- .modelNames
              rownames(.predTest) <- 1:nrow(.predTest)
            }
            colnames(.outcomeCalibration) <- "Outcome"; rownames(.outcomeCalibration) <- 1:nrow(.outcomeCalibration)
            if(length(.outcomeTest)>0){colnames(.outcomeTest) <- "Outcome"; rownames(.outcomeTest) <- 1:nrow(.outcomeTest)}
            
            return(new("ForecastData", predCalibration=.predCalibration, predTest=.predTest,
                       outcomeCalibration=.outcomeCalibration, outcomeTest=.outcomeTest, modelNames=.modelNames))

          }
          )


#' @export
setMethod(
		f="print",
		signature="ForecastData",
		definition=function(x, digits=3, ...){
			cat("*** Class ForecastData, method Print *** \n");
			cat("* Prediction Calibration = \n"); print(x@predCalibration, na.print="", digits=digits);
			cat("* Prediction Test = \n"); print(x@predTest, na.print="", digits=digits);
				cat("* Outcome Calibration = \n");print(x@outcomeCalibration, na.print="", digits=digits);
				cat("* Outcome Test = \n");print(x@outcomeTest, na.print="", digits=digits);
				cat("* Model Names = \n ");print(x@modelNames, na.print="");
			}
			)

#' @export
setMethod(
		f="show",
		signature="ForecastData",
		definition=function(object){
			cat("*** Class ForecastData, method Show *** (limited to the first ten values of each vector)\n");
		    	nrow=min(10,nrow(object@predCalibration))
			cat("Prediction Calibration = \n"); print(object@predCalibration[1:nrow,1:ncol(object@predCalibration)], na.print="", digits=2);
			cat("* Prediction Test =\n"); print(object@predTest[1:nrow,1:ncol(object@predTest)], na.print="", digits=2);
 			cat("* Outcome Calibration = \n"); print(object@outcomeCalibration[1:nrow,1:ncol(object@outcomeCalibration)],na.print="", digits=2);
 			cat("* Outcome Test = \n");print(object@outcomeTest[1:nrow,1:ncol(object@outcomeTest)], na.print="", digits=2);
				cat("* Model Names = \n "); print(object@modelNames,na.print="");
				cat("*** End Show (Forecast Data) *** \n")

			}
)

#' @rdname makeForecastData
#' @export
setGeneric("getPredCalibration",function(object="ForecastData") standardGeneric("getPredCalibration"))
#' @export
setMethod("getPredCalibration","ForecastData",
	function(object="ForecastData"){
		return(object@predCalibration)
		}
)

#' @rdname makeForecastData
#' @export
setGeneric("getPredTest",function(object="ForecastData") standardGeneric("getPredTest"))
#' @export
setMethod("getPredTest","ForecastData",
	function(object){
		return(object@predTest)
		}
)

#' @rdname makeForecastData
#' @export
setGeneric("getOutcomeCalibration",function(object="ForecastData") standardGeneric("getOutcomeCalibration"))
#' @export
setMethod("getOutcomeCalibration","ForecastData",
	function(object="ForecastData"){
		return(object@outcomeCalibration)
		}
)

#' @rdname makeForecastData
#' @export
setGeneric("getOutcomeTest",function(object="ForecastData") standardGeneric("getOutcomeTest"))
#' @export
setMethod("getOutcomeTest","ForecastData",
	function(object="ForecastData"){
		return(object@outcomeTest)
		}
)

#' @rdname makeForecastData
#' @export
setGeneric("getModelNames",function(object="ForecastData") standardGeneric("getModelNames"))
#' @export
setMethod("getModelNames","ForecastData",
	function(object="ForecastData"){
		return(object@modelNames)
		}
)

#' @rdname makeForecastData
#' @export
setGeneric("setPredCalibration<-",function(object,value){standardGeneric("setPredCalibration<-")})

setReplaceMethod(
	f="setPredCalibration",
	signature="ForecastData",
	definition=function(object,value){
		object@predCalibration = as.matrix(value)
		validObject(object)
		return(object)
	}
)

#' @rdname makeForecastData
#' @export
setGeneric("setPredTest<-",function(object,value){standardGeneric("setPredTest<-")})

setReplaceMethod(
	f="setPredTest",
	signature="ForecastData",
	definition=function(object,value){
		object@predTest<- as.matrix(value)
		validObject(object)
		return(object)
	}
)

#' @rdname makeForecastData
#' @export
setGeneric("setOutcomeCalibration<-",function(object,value){standardGeneric("setOutcomeCalibration<-")})

setReplaceMethod(
	f="setOutcomeCalibration",
	signature="ForecastData",
	definition=function(object,value){
		object@outcomeCalibration <- as.matrix(value)
		validObject(object)
		return(object)
	}
)

#' @rdname makeForecastData
#' @export
setGeneric("setOutcomeTest<-",function(object,value){standardGeneric("setOutcomeTest<-")})

setReplaceMethod(
	f="setOutcomeTest",
	signature="ForecastData",
	definition=function(object,value){
		object@outcomeTest<-as.matrix(value)
		validObject(object)
		return(object)
	}
)

#' @rdname makeForecastData
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




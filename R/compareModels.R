
#' @export
setClass(Class="CompareModels",
         representation = representation(
           fitStatistics="matrix",
           period="character",
           threshold="numeric",
           baseModel="numeric"),
         prototype=prototype(
           fitStatistics=matrix(NA, nrow=0, ncol=0),
           period=character(),
           threshold=numeric(),
           baseModel=numeric()
           ),
         validity=function(object){
           if(object@period!="test" & object@period!="calibration"){
             stop("Period must either be for 'calibration' or 'test'")
           }
         }
         )

##
#' Function for comparing multiple models based on predictive performance
#'
#' This function produces statistics to compare the predictive performance of the different models in included as well as for the EBMA model for either the calibration or the test period. It currently calculates the area under the ROC (\code{auc}), the \code{brier} score, the percent of observations predicted correctly (\code{percCorrect}), as well as the proportional reduction in error compared to some baseline model (\code{pre}).
#'
#' @param .forecastData An object of class 'ForecastData'. 
#' @param .period Can take value of "calibration" or "test" and indicates period for which statistics should be calculated.
#' @param .fitStatistics A vector naming statistics that should be calculated.  Possible values include "auc", "brier", "percCorrect", "pre". 
#' @param .threshold The threshold used to calculate when a "positive" prediction is made by the model.
#' @param .baseModel Vector containing predictions used to calculate proportional reduction of error ("pre").
#' @param ... Not implemented
#'
#' @return A data object of the class 'CompareModels' with the following slots:
#' \item{fitStatistics}{The output of the fit statistics for each model}
#' \item{period}{The period, "calibration" or "test", for which the statistics were calculated}
#' \item{threshold}{The threshold used to calculate when a "positive" prediction is made by the model.}
#' \item{baseModel}{Vector containing predictions used to calculate proportional reduction of error ("pre").}
#'
#' @author  Michael D. Ward <\email{michael.d.ward@@duke.edu}> and Jacob M. Montgomery <\email{jacob.montgomery@@wustl.edu}>
#'
#' @references Montgomery, Jacob M., Florian M. Hollenbach and Michael D. Ward. (2012). Improving Predictions Using Ensemble Bayesian Model Averaging. \emph{Political Analysis}. Forthcoming.
#'
#' 
#' @examples data(calibrationSample)
#' 
#' data(testSample) 
#' 
#' this.ForecastData <- makeForecastData(.predCalibration=calibrationSample[,c("LMER", "SAE", "GLM")],
#' .outcomeCalibration=calibrationSample[,"Insurgency"],.predTest=testSample[,c("LMER", "SAE", "GLM")],
#' .outcomeTest=testSample[,"Insurgency"], .modelNames=c("LMER", "SAE", "GLM"))
#' 
#' this.ensemble <- calibrateEnsemble(this.ForecastData, model="logit", tol=0.0001, maxIter=25000, exp=3)
#' 
#' compareModels(this.ensemble,"calibration")
#' 
#' compareModels(this.ensemble,"test") 

#' @seealso ensembleBMA, other functions
#' @aliases compareModels,ForecastData-method CompareModels-class
#' @export
setGeneric(name="compareModels",
           def=function(.forecastData,
             .period="calibration",
             .fitStatistics=c("brier", "auc", "perCorrect", "pre"),
             .threshold=.5,
            .baseModel=0
             , ...)
           {standardGeneric("compareModels")}
           )




#' @export
setMethod(f="compareModels",
          signature(.forecastData="ForecastData"),
          definition=function(.forecastData, .period, .fitStatistics, .threshold, .baseModel)
          {
            if(.period == "calibration")
              {
                preds <- .forecastData@predCalibration
                y <- .forecastData@outcomeCalibration
              }
            else
              {
                preds <- .forecastData@predTest
                y <- .forecastData@outcomeTest
              }
            
            num.models <- ncol(preds)
            num.obs <- nrow(preds)
            if(length(.baseModel)==1){baseModel <- rep(.baseModel, num.obs)}

            out <- new("CompareModels",                       
                       period=.period,
                       threshold=.threshold,
                       baseModel=.baseModel
                       )
            outMat <- matrix(NA, nrow=num.models, ncol=length(.fitStatistics))
            colnames(outMat) <- .fitStatistics
            
            if("brier" %in%.fitStatistics){
              my.fun <- function(x){mean((x-y)^2)}
              outMat[,"brier"] <-aaply(preds, 2,.fun=my.fun, .expand=TRUE)
                                             }
            if("auc" %in% .fitStatistics){
              my.fun <- function(x){Hmisc::somers2(x, y)[1]}
              outMat[,"auc"] <- aaply(preds, 2,.fun=my.fun, .expand=TRUE)}
            if("perCorrect" %in% .fitStatistics){
              my.fun <- function(x){mean((x>.threshold)*y + (x<.threshold)*(1-y))}
              outMat[,"perCorrect"] <- aaply(preds, 2,.fun=my.fun, .expand=TRUE)
            }
            if("pre" %in% .fitStatistics) {
              my.fun <- function(x){
                num.wrong <- num.obs-sum((x>.threshold)*y + (x<.threshold)*(1-y))
                baseline.wrong <- num.obs-sum(baseModel==y)
                (baseline.wrong - num.wrong)/baseline.wrong
              }
              outMat[,"pre"] <- aaply(preds, 2,.fun=my.fun, .expand=TRUE)
            }
            
          out@fitStatistics <- outMat
            rownames(outMat) <- colnames(preds)
          return(outMat)
        }
)
# TODO: Need to make it so that some fit statistics are "ruled out" for some kinds of outcomes.
# TODO: Need to make the compareModels() function throw an error when asked to evaluate over period where the data is not sufficient.

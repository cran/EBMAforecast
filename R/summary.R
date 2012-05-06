##

#' @export
setClass(Class="SummaryForecastData",
         representation=representation(
           summaryData="matrix"        
           ),
         prototype=prototype(
           summaryData=matrix(NA, nrow=0, ncol=0)
           ),
         )

##
#' Summary Function
#'
#' This function summarizes the Ensemble models that have been fit previously by the user.
#'
#' @param object An object of class "FDatFitLogit"
#' @param period The period for which the summary should be provided, either "calibration" or "test".
#' @param fitStatistics A vector naming statistics that should be calculated.  Possible values include "auc", "brier", "percCorrect", "pre". 
#' @param threshold The threshold used to calculate when a "positive" prediction is made by the model.
#' @param baseModel Vector containing predictions used to calculate proportional reduction of error ("pre").
#' @param showCoefs A logical indicating whether model ceofficients from the ensemble should be shown.
#' @param ... Not implemented
#' @method summary FDatFitLogit
#'
#' @return A data object of the class 'SummaryForecastData' with the following slots:
#' \item{summaryData}{Under the default, the function produces a matrix containing one row for each model plus one row for the EBMA forecast.  The first column is always the model weights assigned to the component models.  The second and third columns are for the model parameters for the transformation of the component models.  The remaining columns are the requested fit statistics for all models, as calculated by the \code{copareModels} function.  If \code{showCoefs=TRUE}, then those columns will be excluded. }
#'
#' @author  Michael D. Ward <\email{michael.d.ward@@duke.edu}> and Jacob M. Montgomery <\email{jacob.montgomery@@wustl.edu}>
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
#' summary(this.ensemble, period="calibration") 
#'
#' summary(this.ensemble, period="test",showCoefs=FALSE)
#' @aliases summary,FDatFitLogit-method print,SummaryForecastData-method show,SummaryForecastData-method SummaryForecastData-class
#' @export
setMethod(
          f="summary",
          signature="FDatFitLogit",
          definition=function(object,
            period="calibration",
            fitStatistics=c("brier", "auc", "perCorrect", "pre"),
            threshold=.5,
            baseModel=0,
            showCoefs=TRUE,
            ...){
            out <- compareModels(object, .period=period, .fitStatistics=fitStatistics, .threshold=threshold, .baseModel=baseModel)
            if(showCoefs){
              coefs <- object@modelParams
              coefs <- rbind(c(NA,NA), coefs)
              out <- cbind(coefs, out)
            }
            # Adding weights column
            W <- object@modelWeights
            W <- c(NA, W)
            out <- cbind(W, out)

            rownames(out) <- c("EBMA", object@modelNames)
            new("SummaryForecastData", summaryData=out)
              }
            )


        
#' @export
setMethod(
          f="print",
          signature="SummaryForecastData",
          definition=function(x, digits=3, ...){
            print(x@summaryData, na.print="", digits=digits)
            }
          )


       
#' @export
setMethod(
          f="show",
          signature="SummaryForecastData",
          definition=function(object){
            print(object@summaryData, na.print="", digits=3)
            }
          )



#' Calibrate an ensemble Bayesian Model Averaging model
#' 
#' This function calibrates an EBMA model based on out-of-sample performance in the calibration time period. Given a dependent variable and calibration-sample predictions from multiple component forecast models in the \code{ForecastData} the \code{calibrateEnsemble} function fits an ensemble BMA mixture model. The weights assigned to each model are derived from the individual model's performance in the calibration period. Missing observations are allowed in the calibration period, however models with missing observations are penalized. When missing observations are prevalent in the calibration set, the EM algorithm is adjusted and model parameters are estimated by maximizing a renormalized partial expected complete-data log-likelihood (Fraley et al. 2010).
#'
#'
#' @param .forecastData An object of class \code{ForecastData} that will be used to calibrate the model.
#' @param exp The exponential shrinkage term.  When \code{model="logit"}, forecasts are raised to the (1/exp) power on the logit scale for the purposes of bias reduction.  The default value is \code{exp=3}. When \code{model="logit"}, forecasts are raised to the (1/exp) and the default value is \code{exp=1}.
#' @param tol Tolerance for improvements in the log-likelihood before the EM algorithm will stop optimization.  The default is \code{sqrt(.Machine$double.eps)}. 
#' @param maxIter The maximum number of iterations the EM algorithm will run before stopping automatically. The default is \code{maxIter=1e6}.
#' @param model The model type that should be used given the type of data that is being predicted (i.e., normal, binary, etc.).
#' @param method The estimation method used.  Currently only implements "EM".
#' @param predType The prediction type used for the EBMA model under the normal model, user can choose either \code{posteriorMedian} or \code{posteriorMean}. Posterior median is the default.
#' @param useModelParams A logical that indicates whether the regression parameters should be used to post-process the data.  The default is TRUE.  We recommend setting this to FALSE for sparse datasets or datasets modeling rare events.  
#' @param ... Not implemented
#'
#' @return Returns a data of class \code{FDatFitLogit} or \code{FDatFitNormal}, a subclass of \code{ForecastData}, with the following slots
#' \item{predCalibration}{An array containing the predictions of all component models and the EBMA model for all observations in the calibration period.} 
#' \item{predTest}{An array containing the predictions of all component models and the EBMA model for all observations in the test period.}
#' \item{outcomeCalibration}{A vector containing the true values of the dependent variable for all observations in the calibration period.} 
#' \item{outcomeTest}{A vector containing the true values of the dependent variable for all observations in the test period.}
#' \item{modelNames}{A character vector containing the names of all component models.  If no model names are specified, names will be assigned automatically.}
#' \item{modelWeights}{A vector containing the model weights assigned to each model.}
#' \item{modelParams}{An array containing the parameters for the individual logit models that transform the component models.}
#' \item{logLik}{The final log-likelihood for the calibrated EBMA model.}
#' \item{exp}{The exponential shrinkage term.}
#' \item{tol}{Tolerance for improvements in the log-likelihood before the EM algorithm will stop optimization.}
#' \item{maxIter}{The maximum number of iterations the EM algorithm will run before stopping automatically.}
#' \item{method}{The estimation method used. }
#' \item{call}{The actual call used to create the object.}
#'
#'
#' @author Michael D. Ward <\email{michael.d.ward@@duke.edu}> and Jacob M. Montgomery <\email{jacob.montgomery@@wustl.edu}> and Florian M. Hollenbach <\email{florian.hollenbach@@duke.edu}>
#'
#' @references Montgomery, Jacob M., Florian M. Hollenbach and Michael D. Ward. (2012). Improving Predictions Using Ensemble Bayesian Model Averaging. \emph{Political Analysis}. \bold{20}: 271-291.
#'
#' @references Raftery, A. E., T. Gneiting, F. Balabdaoui and M. Polakowski. (2005). Using Bayesian Model Averaging to calibrate forecast ensembles. \emph{Monthly Weather Review}. \bold{133}:1155--1174.
#' @references Sloughter, J. M., A. E. Raftery, T. Gneiting and C. Fraley. (2007). Probabilistic quantitative precipitation forecasting using Bayesian model averaging. \emph{Monthly Weather Review}. \bold{135}:3209--3220.
#' @references Fraley, C., A. E. Raftery, T. Gneiting. (2010). Calibrating Multi-Model Forecast Ensembles with Exchangeable and Missing Members using Bayesian Model Averaging. \emph{Monthly Weather Review}. \bold{138}:190--202.
#' @references Sloughter, J. M., T. Gneiting and A. E. Raftery. (2010). Probabilistic wind speed forecasting using ensembles and Bayesian model averaging. \emph{Journal of the American Statistical Association}. \bold{105}:25--35.
#'
#' @examples 
#'
#' \dontrun{
#' data(calibrationSample)
#'
#' data(testSample) 
#'
#' this.ForecastData <- makeForecastData(.predCalibration=calibrationSample[,c("LMER", "SAE", "GLM")],
#' .outcomeCalibration=calibrationSample[,"Insurgency"],.predTest=testSample[,c("LMER", "SAE", "GLM")],
#' .outcomeTest=testSample[,"Insurgency"], .modelNames=c("LMER", "SAE", "GLM"))
#'
#' this.ensemble <- calibrateEnsemble(this.ForecastData, model="logit", exp=3)
#' }
#'
#' @keywords calibrate EBMA 
#'
#' @rdname calibrateEnsemble
#' @aliases fitEnsemble,ForecastDataLogit-method fitEnsemble,ForecastDataNormal-method FDatFitLogit-class ForecastDataLogit-class  ForecastDataNormal-class FDatFitNormal-class calibrateEnsemble,ForecastData-method 
#' @export
setGeneric(name="calibrateEnsemble",
           def=function(.forecastData=new("ForecastData"),
             exp=1,
             tol=sqrt(.Machine$double.eps),
             maxIter=1e6,
             model="logit",
             method="EM",
             useModelParams=TRUE,
             ...)
           {standardGeneric("calibrateEnsemble")}
           )



#' @export
setMethod(f="calibrateEnsemble",
          signature="ForecastData",
          definition=function(
            .forecastData,
            exp=1,
            tol=sqrt(.Machine$double.eps),
            maxIter=1e6,
            model="logit",
            method="EM",
            useModelParams = TRUE,
            predType="posteriorMedian",
            ...)
          {
            switch(model,
                   logit ={.forecastData <- as(.forecastData, "ForecastDataLogit")},
                   logistic ={.forecastData <- as(.forecastData, "ForecastDataLogit")},
                   normal={.forecastData <- as(.forecastData, "ForecastDataNormal")}
                   )
            eval(fitEnsemble(.forecastData,
                             exp=exp,
                             tol=tol,
                             maxIter=maxIter,
                             method="EM",
                             useModelParams=useModelParams,
                             predType=predType,
                             ...), parent.frame())
          }
          )




           


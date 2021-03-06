% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/forecastData.R
\docType{class}
\name{ForecastData-class}
\alias{ForecastData-class}
\title{An ensemble forecasting data object}
\description{
Objects of class \code{ForecastData} are used in the \code{calibrateEnsemble} function. Datasets should be converted into an object of class \code{ForecastData} using the \code{makeForecastData} function. Individual slots of the \code{ForecastData} object can be accessed and changed using the \code{get} and \code{set} functions respectively. Missing observations in the prediction calibration set are allowed.
}
\details{
@slot predCalibration An array containing the predictions of all component models for the observations in the calibration period.
 @slot predTest An array containing the predictions of all component models for all the observations in the test period.
 @slot outcomeCalibration A vector containing the true values of the dependent variable for all observations in the calibration period.
 @slot outcomeTest A vector containing the true values of the dependent variable for all observations in the test period.
 @slot modelNames A character vector containing the names of all component models.
}
\examples{
\dontrun{ 
data(calibrationSample)
data(testSample)

this.ForecastData <- makeForecastData(.predCalibration=calibrationSample[,c("LMER", "SAE", "GLM")],
.outcomeCalibration=calibrationSample[,"Insurgency"],.predTest=testSample[,c("LMER", "SAE", "GLM")],
.outcomeTest=testSample[,"Insurgency"], .modelNames=c("LMER", "SAE", "GLM"))

### to acces individual slots in the ForecastData object
getPredCalibration(this.ForecastData)
getOutcomeCalibration(this.ForecastData)
getPredTest(this.ForecastData)
getOutcomeTest(this.ForecastData)
getModelNames(this.ForecastData)

### to assign individual slots, use set functions

setPredCalibration(this.ForecastData)<-calibrationSample[,c("LMER", "SAE", "GLM")]
setOutcomeCalibration(this.ForecastData)<-calibrationSample[,"Insurgency"]
setPredTest(this.ForecastData)<-testSample[,c("LMER", "SAE", "GLM")]
setOutcomeTest(this.ForecastData)<-testSample[,"Insurgency"]
setModelNames(this.ForecastData)<-c("LMER", "SAE", "GLM")
}


}

\name{makeForecastData}
\alias{ForeCastData-class}
\alias{getModelNames}
\alias{getModelNames,ForecastData-method}
\alias{getOutcomeCalibration}
\alias{getOutcomeCalibration,ForecastData-method}
\alias{getOutcomeTest}
\alias{getOutcomeTest,ForecastData-method}
\alias{getPredCalibration}
\alias{getPredCalibration,ForecastData-method}
\alias{getPredTest}
\alias{getPredTest,ForecastData-method}
\alias{initialize,ForecastData-method}
\alias{makeForecastData}
\alias{makeForecastData,ANY-method}
\alias{print,ForecastData-method}
\alias{setModelNames<-}
\alias{setModelNames<-,ForecastData-method}
\alias{setOutcomeCalibration<-}
\alias{setOutcomeCalibration<-,ForecastData-method}
\alias{setOutcomeTest<-}
\alias{setOutcomeTest<-,ForecastData-method}
\alias{setPredCalibration<-}
\alias{setPredCalibration<-,ForecastData-method}
\alias{setPredTest<-}
\alias{setPredTest<-,ForecastData-method}
\alias{show,ForecastData-method}
\title{Build a ensemble forecasting data object}
\arguments{
  \item{.predAll}{An n by p matrix of data.frame containing
  predictions, for both the calibration and test
  observations.}

  \item{.outcomeAll}{A vector of length n, containing the
  true values of the dependent variable, for both the
  calibration and test observations.}

  \item{.inOut}{A dichotomous vector of length n, where 1
  indicates that the observation is in the test sample and
  0 that it is in the calibration sample.}

  \item{.predCalibration}{A matrix with the number of rows
  being the number of observations in the calibration
  period and a column with calibration period predictions
  for each model.}

  \item{.predTest}{A matrix with the number of rows being
  the number of observations in the test period and a
  column with test period predictions for each model.}

  \item{.outcomeCalibration}{A vector with the true values
  of the dependent variable for each observation in the
  calibration period.}

  \item{.outcomeTest}{A vector with the true values of the
  dependent variable for each observation in the test
  period.}

  \item{.modelNames}{A vector of length p with the names of
  the component models.}

  \item{...}{Additional arguments not implemented}
}
\value{
  A data object of the class 'ForecastData' with the
  following slots: \item{predCalibration}{A matrix
  containing the predictions of all component models for
  all observations in the calibration period.}
  \item{predTest}{A matrix containing the predictions of
  all component models for all observations in the test
  period.} \item{outcomeCalibration}{A vector containing
  the true values of the dependent variable for all
  observations in the calibration period.}
  \item{outcomeTest}{A vector containing the true values of
  the dependent variable for all observations in the test
  period.} \item{modelNames}{A character vector containing
  the names of all component models.  If no model names are
  specified, names will be assigned automatically.}
}
\description{
  This function uses the user provided component model
  forecasts and dependent variable observations to create
  an object of class \code{ForecastData}, which can then be
  used to calibrate and fit the Ensemble.
}
\examples{
data(calibrationSample)

data(testSample)
this.ForecastData <- makeForecastData(.predCalibration=calibrationSample[,c("LMER", "SAE", "GLM")],
.outcomeCalibration=calibrationSample[,"Insurgency"],.predTest=testSample[,c("LMER", "SAE", "GLM")],
.outcomeTest=testSample[,"Insurgency"], .modelNames=c("LMER", "SAE", "GLM"))
}
\author{
  Michael D. Ward <\email{michael.d.ward@duke.edu}> and
  Jacob M. Montgomery <\email{jacob.montgomery@wustl.edu}>
}
\references{
  Montgomery, Jacob M., Florian M. Hollenbach and Michael
  D. Ward. (2012). Improving Predictions Using Ensemble
  Bayesian Model Averaging. \emph{Political Analysis}.
  Forthcoming.
}
\seealso{
  ensembleBMA, other functions
}


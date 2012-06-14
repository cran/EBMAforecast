\name{fitEnsemble}
\alias{FDatFitLogit-class}
\alias{ForecastData-class}
\alias{ForecastDataLogit-class}
\alias{ForecastDataNormal-class}
\alias{calibrateEnsemble}
\alias{calibrateEnsemble,ForecastData-method}
\alias{fitEnsemble}
\alias{fitEnsemble,ForecastDataLogit-method}
\alias{fitEnsemble,ForecastDataNormal-method}
\title{Calibrate an ensemble Bayesian Model Averaging model}
\arguments{
  \item{.forecastData}{An object of class 'ForecastData'
  that will be used to calibrate the model.}

  \item{exp}{The exponential shrinkage term.  Forecasts are
  raised to the (1/exp) power on the logit scale for the
  purposes of bias reduction.  The default value is
  \code{exp=3}.}

  \item{tol}{Tolerance for improvements in the
  log-likelihood before the EM algorithm will stop
  optimization.  The default is \code{tol= 0.01}, which is
  somewhat high.  Researchers may wish to reduce this by an
  order of magnitude for final model estimation.}

  \item{maxIter}{The maximum number of iterations the EM
  algorithm will run before stopping automatically. The
  default is \code{maxIter=10000}.}

  \item{model}{The model type that should be used to the
  type of data that is being predicted (i.e., normal,
  binary, etc.)}

  \item{method}{The estimation method used.  Currently only
  implements "EM"}

  \item{...}{Not implemented}
}
\value{
  Returns a data of class 'FDatFitLogit', a subclass of
  'ForecastData', with the following slots
  \item{predCalibration}{A matrix containing the
  predictions of all component models and the EBMA model
  for all observations in the calibration period.}
  \item{predTest}{A matrix containing the predictions of
  all component models and the EBMA model for all
  observations in the test period.}
  \item{outcomeCalibration}{A vector containing the true
  values of the dependent variable for all observations in
  the calibration period.} \item{outcomeTest}{An optional
  vector containing the true values of the dependent
  variable for all observations in the test period.}
  \item{modelNames}{A character vector containing the names
  of all component models.  If no model names are
  specified, names will be assigned automatically.}
  \item{modelWeights}{A vector containing the model weights
  assigned to each moel} \item{modelParams}{The parameters
  for the individual logit models that transform the
  component models} \item{logLik}{The final log-likelihood
  for the calibrated EBMA model} \item{exp}{The exponential
  shrinkage term} \item{tol}{Tolerance for improvements in
  the log-likelihood before the EM algorithm will stop
  optimization.} \item{maxIter}{The maximum number of
  iterations the EM algorithm will run before stopping
  automatically} \item{method}{The estimation method used.
  } \item{call}{The actual call used to create the object}
}
\description{
  This function calibrates an EBMA model based on
  out-of-sample performance in the calibration time period.
  Given a dependent variable and in-sample predictions from
  multiple component forecast models in the
  \code{ForecastData} the \code{calibrateEnsemble} function
  fits an ensemble BMA mixture model. The weights assigned
  to each model are derived from the individual model's
  performance in the calibration period.
}
\examples{
data(calibrationSample)

data(testSample)

this.ForecastData <- makeForecastData(.predCalibration=calibrationSample[,c("LMER", "SAE", "GLM")],
.outcomeCalibration=calibrationSample[,"Insurgency"],.predTest=testSample[,c("LMER", "SAE", "GLM")],
.outcomeTest=testSample[,"Insurgency"], .modelNames=c("LMER", "SAE", "GLM"))

this.ensemble <- calibrateEnsemble(this.ForecastData, model="logit", tol=0.0001, maxIter=25000, exp=3)
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

  Raftery, A. E., T. Gneiting, F. Balabdaoui and M.
  Polakowski. (2005). Using Bayesian Model Averaging to
  calibrate forecast ensembles. \emph{Monthly Weather
  Review}. \bold{133}:1155--1174.

  Sloughter, J. M., A. E. Raftery, T. Gneiting and C.
  Fraley. (2007). Probabilistic quantitative precipitation
  forecasting using Bayesian model averaging. \emph{Monthly
  Weather Review}. \bold{135}:3209--3220.

  Fraley, C., A. E. Raftery, T. Gneiting. (2010).
  Calibrating Multi-Model Forecast Ensembles with
  Exchangeable and Missing Members using Bayesian Model
  Averaging. \emph{Monthly Weather Review}.
  \bold{138}:190--202.

  Sloughter, J. M., T. Gneiting and A. E. Raftery. (2010).
  Probabilistic wind speed forecasting using ensembles and
  Bayesian model averaging. \emph{Journal of the American
  Statistical Association}. \bold{105}:25--35.
}
\keyword{EBMA}
\keyword{calibrate}


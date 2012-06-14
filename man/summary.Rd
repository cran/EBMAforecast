\name{summary}
\alias{SummaryForecastData-class}
\alias{print,SummaryForecastData-method}
\alias{show,SummaryForecastData-method}
\alias{summary}
\alias{summary,FDatFitLogit-method}
\title{Summary Function}
\arguments{
  \item{object}{An object of class "FDatFitLogit"}

  \item{period}{The period for which the summary should be
  provided, either "calibration" or "test".}

  \item{fitStatistics}{A vector naming statistics that
  should be calculated.  Possible values include "auc",
  "brier", "percCorrect", "pre".}

  \item{threshold}{The threshold used to calculate when a
  "positive" prediction is made by the model.}

  \item{baseModel}{Vector containing predictions used to
  calculate proportional reduction of error ("pre").}

  \item{showCoefs}{A logical indicating whether model
  ceofficients from the ensemble should be shown.}

  \item{...}{Not implemented}
}
\value{
  A data object of the class 'SummaryForecastData' with the
  following slots: \item{summaryData}{Under the default,
  the function produces a matrix containing one row for
  each model plus one row for the EBMA forecast.  The first
  column is always the model weights assigned to the
  component models.  The second and third columns are for
  the model parameters for the transformation of the
  component models.  The remaining columns are the
  requested fit statistics for all models, as calculated by
  the \code{copareModels} function.  If
  \code{showCoefs=TRUE}, then those columns will be
  excluded. }
}
\description{
  This function summarizes the Ensemble models that have
  been fit previously by the user.
}
\examples{
data(calibrationSample)

data(testSample)

this.ForecastData <- makeForecastData(.predCalibration=calibrationSample[,c("LMER", "SAE", "GLM")],
.outcomeCalibration=calibrationSample[,"Insurgency"],.predTest=testSample[,c("LMER", "SAE", "GLM")],
.outcomeTest=testSample[,"Insurgency"], .modelNames=c("LMER", "SAE", "GLM"))

this.ensemble <- calibrateEnsemble(this.ForecastData, model="logit", tol=0.0001, maxIter=25000, exp=3)

summary(this.ensemble, period="calibration")

summary(this.ensemble, period="test",showCoefs=FALSE)
}
\author{
  Michael D. Ward <\email{michael.d.ward@duke.edu}> and
  Jacob M. Montgomery <\email{jacob.montgomery@wustl.edu}>
}


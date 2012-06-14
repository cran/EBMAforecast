\name{plot}
\alias{plot}
\alias{plot,FDatFitLogit-method}
\title{Plotting function for logistic ensemble models that have already been fit}
\arguments{
  \item{x}{An object of class "FDatFitLogit".}

  \item{period}{Can take value of "calibration" or "test"
  and indicates the period for which the plots should be
  produced.}
}
\value{
  NULL
}
\description{
  This function creates separation plots for each of the
  fitted models, including the EBMA model. Observations are
  ordered from left to right with increasing predicted
  probabilities, which is depicted by the black line.
  Actual occurrences are displayed by red vertical lines.
  Plots can be dislayed for the test or calibration period.
}
\examples{
data(calibrationSample)

data(testSample)

this.ForecastData <- makeForecastData(.predCalibration=calibrationSample[,c("LMER", "SAE", "GLM")],
.outcomeCalibration=calibrationSample[,"Insurgency"],.predTest=testSample[,c("LMER", "SAE", "GLM")],
.outcomeTest=testSample[,"Insurgency"], .modelNames=c("LMER", "SAE", "GLM"))

this.ensemble <- calibrateEnsemble(this.ForecastData, model="logit", tol=0.0001, maxIter=25000, exp=3)

plot(this.ensemble, period="calibration")

plot(this.ensemble, period="test")
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

  Greenhill, B., M.D. Ward, A. Sacks. (2011). The
  Separation Plot: A New Visual Method For Evaluating the
  Fit of Binary Data. \emph{American Journal of Political
  Science}.\bold{55}: 991--1002.
}
\seealso{
  \code{separationplot}
}


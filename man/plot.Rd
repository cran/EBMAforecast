\name{plot}
\alias{plot}
\alias{plot,FDatFitLogit-method}
\alias{plot,FDatFitNormal-method}
\title{Plotting function for ensemble models of the class "FDatFitLogit" or "FDatFitNormal", which are the objects created by the \code{calibrateEnsemble()} function.}
\arguments{
  \item{x}{An object of class "FDatFitLogit" or
  "FDatFitNormal"}

  \item{period}{Can take value of "calibration" or "test"
  and indicates the period for which the plots should be
  produced.}

  \item{subset}{The row names or numbers for the
  observations the user wishes to plot.  Only implemented
  for the subclass "FDatFitNormal"}

  \item{mainLabel}{A vector strings to appear at the top of
  each predictive posterior plot.  Only implemented for the
  subclass "FDatFitNormal"}

  \item{xLab}{The label for the x-axis. Only implemented
  for the subclass "FDatFitNormal"}

  \item{yLab}{The label for the y-axis.  Only implemented
  for the subclass "FDatFitNormal"}

  \item{cols}{A vector containing the color for plotting
  the predictive pdf of each component model forecast. Only
  implemented for the subclass "FDatFitNormal"}
}
\value{
  NULL
}
\description{
  Default plotting for objectes created by the
  "calibrateEnsemble" function.  See details below.
}
\details{
  For objects of the class "FDatFitLogit", this function
  creates separation plots for each of the fitted models,
  including the EBMA model. Observations are ordered from
  left to right with increasing predicted probabilities,
  which is depicted by the black line. Actual occurrences
  are displayed by red vertical lines. Plots can be
  displayed for the test or calibration period. For objects
  of the class "FDatFitNormal", this function creates a
  plot of the predictive density distribution containing
  the EBMA PDF and the PDFs for all component models
  (scaled by their model weights).  It also plots the
  prediction for the ensemble and the components for the
  specified observations.
}
\examples{
data(calibrationSample)

\dontrun{
data(testSample)

this.ForecastData <- makeForecastData(.predCalibration=calibrationSample[,c("LMER", "SAE", "GLM")],
.outcomeCalibration=calibrationSample[,"Insurgency"],.predTest=testSample[,c("LMER", "SAE", "GLM")],
.outcomeTest=testSample[,"Insurgency"], .modelNames=c("LMER", "SAE", "GLM"))

this.ensemble <- calibrateEnsemble(this.ForecastData, model="logit", tol=0.001, exp=3)

plot(this.ensemble, period="calibration")
plot(this.ensemble, period="test")

}
}
\author{
  Michael D. Ward <\email{michael.d.ward@duke.edu}> and
  Jacob M. Montgomery <\email{jacob.montgomery@wustl.edu}>
}
\references{
  Raftery, A. E., T. Gneiting, F. Balabdaoui and M.
  Polakowski. (2005). Using Bayesian Model Averaging to
  calibrate forecast ensembles. \emph{Monthly Weather
  Review}. \bold{133}:1155--1174.

  Greenhill, B., M.D. Ward, A. Sacks. (2011). The
  Separation Plot: A New Visual Method For Evaluating the
  Fit of Binary Data. \emph{American Journal of Political
  Science}.\bold{55}: 991--1002.

  Montgomery, Jacob M., Florian M. Hollenbach and Michael
  D. Ward. (2012). Improving Predictions Using Ensemble
  Bayesian Model Averaging. \emph{Political Analysis}.
  \bold{20}: 271-291.
}
\seealso{
  \code{separationplot}
}


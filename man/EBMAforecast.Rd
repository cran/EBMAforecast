\docType{package}
\name{EBMAforecast}
\alias{EBMAforecast}
\alias{EBMAforecast-package}
\title{EBMAforecast}
\description{
  The EBMAforecast package (currently under development)
  allows users to increase the accuracy of forecasting
  models by pooling multiple component forecasts to
  generate ensemble forecasts. It includes functions to fit
  an ensemble Bayesian model averaging (EBMA) model using
  in-sample predictions, generate ensemble out-of-sample
  predictions, and create useful data visualizations.
  Currently, the package can only handle dichotomous
  outcomes, although additional models will be added to the
  package in the coming months.
}
\examples{
demo(EBMAforecast)
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


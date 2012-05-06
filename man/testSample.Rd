\docType{data}
\name{testSample}
\alias{testSample}
\title{Test sample data}
\description{
  This includes the test sample data for the insurgency
  forecasting example in Montgomery, Hollenbach and Ward
  (2012). It provides the predictions for the three models
  included in the Ensemble models, as well as the true
  values of the dependent variable for insurgency in 29
  Asian countries. The test sample ranges ranges from
  January 2010 to December 2010.
}
\details{
  The variables included in the dataset are: \itemize{
  \item\code{LMER} The test sample predictions of the LMER
  model from the insurgency prediction example in
  Montgomery et. al. (2012). The LMER model is a
  generalized linear mixed effects model using the logistic
  link function. It includes two random effects terms and
  several other covariates. \item\code{SAE} The test sample
  prediction of the SAE model from the insurgency
  prediction example in Montgomery et. al. (2012). This is
  a model developed as part of the ICEWS project and was
  designed by \emph{Strategic Analysis Enterprises}. It is
  a simple generalized linear model with 27 independent
  variables. \item\code{GLM} The test sample prediction of
  the GLM model from the insurgency prediction example in
  Montgomery et. al. (2012). This is a crude logistic model
  with only four independent variables.
  \item\code{Insurgency} The true values of the dependent
  variable in the test sample from the insurgency
  prediction example in Montgomery et. al. (2012). This is
  a binary variable indicating the actual ocurrence of
  insurgency for each observation in the test sample. }
}
\references{
  Montgomery, Jacob M., Florian M. Hollenbach and Michael
  D. Ward. (2012). Improving Predictions Using Ensemble
  Bayesian Model Averaging. \emph{Political Analysis}.
  Forthcoming.
}
\keyword{datasets}


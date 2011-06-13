
sampling_freq <- function(null, test, cpu=16, nboot=200, 
                          sample_effort = c(10, 50, 100),
                          length.original=NULL){
# Evaluate the effect of changing the sampling effort
# Args:
#   m: a set of model fits from fit_models()
#   sample_effort: either a percentage of data or as number of points
#   length.original: if NULL, use abolute lengths, otherwise, sample_effort
#                    is interpreted as a fraction of this original data length
  if(!is.null(length.original))
    sample_effort <- sample_effort * length.original
  lapply(sample_effort, 
         function(effort){
          montecarlotest(null, test, cpu=cpu, nboot=nboot, times=effort)
         })
}


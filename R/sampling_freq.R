
sampling_freq <- function(m, sample_effort = c(.1, .5, 1, 2, 5, 10, 20),
                          mode=c("percent", "total")){
# Evaluate the effect of changing the sampling effort
# Args:
#   m: a set of model fits from fit_models()
#   sample_effort: either a percentage of data or as number of points
#   mode: Whether sample_effort is a "percent" of observed sampling, or 
#         "total" number of pts
  mode <- match.arg(mode)
  if(mode=="percent")
    sample_effort <- sample_effort * length(m$X)
  lapply(sample_effort, 
         function(effort){
          montecarlotest(m$const, m$timedep, cpu, nboot, times=effort)
         })
}



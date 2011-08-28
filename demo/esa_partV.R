require(warningsignals)
load("false_negative.Rdat") # Failed Detection
fn <- fit_models(ibm_critical, "LSN")
fn_taus <- tau_dist_montecarlo(ibm_critical, fn$const, fn$timedep, nboot=500, cpu=16, signal="CV")
load("true_positive.Rdat") # Successful Detection
tp <- fit_models(ibm_critical, "LSN")
tp_taus <- tau_dist_montecarlo(ibm_critical, tp$const, tp$timedep, nboot=500, cpu=16, signal="CV")

load("small_tau.Rdat") # 
tn <- fit_models(ibm_critical, "LSN")
tn_taus <- tau_dist_montecarlo(ibm_critical, tn$const, tn$timedep, nboot=500, cpu=16, signal="CV")

load("stable_pos.Rdat") # false alarm
fp <- fit_models(ibm_critical, "LSN")
fp_taus <- tau_dist_montecarlo(ibm_critical, fp$const, fp$timedep, nboot=500, cpu=16, signal="CV")

save(list=ls(), file="partV.Rdat")

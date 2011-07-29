require(warningsignals)
load("false_negative.Rdat") # Failed Detection
fn <- fit_models(ibm_critical, "LSN")
fn_mc <- montecarlotest(fn$const, fn$timedep, cpu=16, nboot=200)
fn_taus <- tau_dist_montecarlo(ibm_critical, fn$const, fn$timedep, nboot=500, cpu=16)
load("true_positive.Rdat") # Successful Detection
tp <- fit_models(ibm_critical, "LSN")
tp_mc <- montecarlotest(tp$const, tp$timedep, cpu=16, nboot=200)
tp_taus <- tau_dist_montecarlo(ibm_critical, tp$const, tp$timedep, nboot=500, cpu=16)

load("small_tau.Rdat") # 
tn <- fit_models(ibm_critical, "LSN")
tn_mc <- montecarlotest(tn$const, tn$timedep, cpu=16, nboot=200)
tn_taus <- tau_dist_montecarlo(ibm_critical, tn$const, tn$timedep, nboot=500, cpu=16)

load("stable_pos.Rdat") # false alarm
fp <- fit_models(ibm_critical, "LSN")
fp_mc <- montecarlotest(fp$const, fp$timedep, cpu=16, nboot=200)
fp_taus <- tau_dist_montecarlo(ibm_critical, fp$const, fp$timedep, nboot=500, cpu=16)

save(list=ls(), file="partIV.Rdat")

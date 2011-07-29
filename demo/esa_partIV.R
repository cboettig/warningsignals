int <- 1:85
load("false_negative.Rdat") # Failed Detection, FN
M <- cbind(time(ibm_critical)[int], ibm_critical[int])
Y <- matrix2ts(M)
save(list="Y", file="fn.Rdat")
fn <- fit_models(Y, "LSN")
#fn_mc <- montecarlotest(fn$const, fn$timedep, cpu=16, nboot=200)
fn_taus <- tau_dist_montecarlo(Y, fn$const, fn$timedep, nboot=500, cpu=16)


int <- 1:85
load("true_positive.Rdat") # Successful Detection, TP
M <- cbind(time(ibm_critical)[int], ibm_critical[int])
Y <- matrix2ts(M)
save(list="Y", file="tp.Rdat")
tp <- fit_models(Y, "LSN")
#tp_mc <- montecarlotest(tp$const, tp$timedep, cpu=16, nboot=200)
tp_taus <- tau_dist_montecarlo(Y, tp$const, tp$timedep, nboot=500, cpu=16)


int <- 1:90
load("small_tau.Rdat") # TN
M <- cbind(time(ibm_critical)[int], ibm_critical[int])
Y <- matrix2ts(M)
save(list="Y", file="tn.Rdat")
tn <- fit_models(Y, "LSN")
#tn_mc <- montecarlotest(tn$const, tn$timedep, cpu=16, nboot=200)
tn_taus <- tau_dist_montecarlo(Y, tn$const, tn$timedep, nboot=500, cpu=16)


int <- 1:90
load("stable_pos.Rdat") # false alarm, FP
M <- cbind(time(ibm_critical)[int], ibm_critical[int])
Y <- matrix2ts(M)
save(list="Y", file="fp.Rdat")
fp <- fit_models(Y, "LSN")
#fp_mc <- montecarlotest(fp$const, fp$timedep, cpu=16, nboot=200)
fp_taus <- tau_dist_montecarlo(Y, fp$const, fp$timedep, nboot=500, cpu=16)




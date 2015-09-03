library(dpcR)
library(pbapply)
library(reshape2)
library(dplyr)

#https://github.com/michbur/test_counts2/blob/master/abstract_simulation.R



comp_2runs <- do.call(rbind, pblapply(1L:500, function(replicate) {
  do.call(rbind, lapply(1L:10*20, function(m1) {
    adpcr1 <- binarize(sim_adpcr(m = m1, n = 765, times = 1e6, pos_sums = FALSE, n_panels = 2))
    
    compb <- test_counts(adpcr1, "binomial")
    compr <- test_counts(adpcr1, "ratio")
    
    data.frame(replicate = replicate, m = m1, k = unname(colSums(adpcr1)), test = c("GLM", "MT"), 
               p_value = c(slot(compb, "test_res")[, "p_value"], slot(compr, "test_res")[, "p_value"]))
  }))
}))


sucrate <- mutate(comp_2runs, equal = p_value > 0.05) %>%
  group_by(m, test) %>% 
  summarise(sucrate = mean(equal), mp_value = mean(p_value), sdp_value = sd(p_value))


# save("adpcr_comp", "indat", "m_coverage2", "m_simultaneous", "madpcr_comp", "sucrate",
#      file = "test_count_eval.RData")


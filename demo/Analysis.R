indep <- read_BayesTraits_log("BayesTraits/log_indep_stones.txt") %>%
  select(Iteration, Lh, Harmonic_Mean) %>%
  mutate(type = "independent")
dep <- read_BayesTraits_log("BayesTraits/log_dep_stones.txt") %>%
  select(Iteration, Lh, Harmonic_Mean) %>%
  mutate(type = "dependent")

rbind(indep, dep) %>%
  ggplot(aes(x = Iteration, y = Harmonic_Mean, color = type)) +
  geom_line()

(ll_indep <- indep$Harmonic_Mean[nrow(indep)])
(ll_dep <- dep$Harmonic_Mean[nrow(dep)])

2 * (ll_dep - ll_indep)

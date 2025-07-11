
fxn. <- bind_rows(
  fxn.coms,
  fxn.child
) %>% 
  mutate( status = tolower(status)) %>% 
  mutate( status = factor (status, c('patient', 'patient.ext', 'carrier','control','unknown'))) 
# filter( status == 'patient', is.na(gaa1))
# mutate( mutation = pm )

fxn <- fxn. %>% 
  filter(pm==0) %>% 
  select(study, origin, type, fxn, aoo, gaa1) %>% 
  filter(!is.na(aoo), !is.na(gaa1), !is.na(fxn), is.finite(fxn))

calculate_correlations <- function(data) {
  corr_fxn_aoo <- cor.test(data$fxn, data$aoo)
  corr_fxn_gaa1 <- cor.test(data$fxn, data$gaa1)
  corr_aoo_gaa1 <- cor.test(data$aoo, data$gaa1)
  
  tibble(
    Pair = c("fxn-aoo", "fxn-gaa1", "aoo-gaa1"),
    R = round(c(corr_fxn_aoo$estimate, corr_fxn_gaa1$estimate, corr_aoo_gaa1$estimate), 2),
    R2 = round(c(corr_fxn_aoo$estimate^2, corr_fxn_gaa1$estimate^2, corr_aoo_gaa1$estimate^2), 2),
    P_Value = sprintf("%.1e", c(corr_fxn_aoo$p.value, corr_fxn_gaa1$p.value, corr_aoo_gaa1$p.value)),
    Observations = c(corr_fxn_aoo$parameter + 2, corr_fxn_gaa1$parameter + 2, corr_aoo_gaa1$parameter + 2)
  )
}

results <- fxn %>%
  group_by(study, origin, type) %>%
  nest() %>%
  mutate(correlation = map(data, calculate_correlations)) %>%
  select(-data) %>%
  unnest(correlation)

flextable(results)

results %>% .ct

fxn. %>% 
  unnest(data) %>% 
  filter(!is.na(sev.o)) %>% 
  group_by(group, sev.o) %>% 
  ggplot()+geom_boxplot()+
  aes(x = sev.o, y = fxn)+
  facet_wrap(~group)

fxn.nest %>% 
  filter(group == 'FACOMS blood mature') %>% 
  unnest(data) %>% 
  filter(pm==0) %>%
  lmer(aval ~ bl + fxnpct_group*time. +  (1 + time.|sjid) , data = . , control = .lme4.ctrl ) %>%
  # lmer(aval ~ bl +       fxn:time. + time. + (1 + time.|sjid) , data = . , control = .lme4.ctrl ) %>% 
  # lmer(aval ~ bl + fxn + fxn:time. + time. + (1 + time.|sjid) , data = . , control = .lme4.ctrl ) %>% 
  broom::tidy(effects = "fixed", conf.int = T) %>% 
  .fixmod()


# table -------------------------------------------------------------------


results. <- fxn. %>%
  filter(!is.na(sev.o)) %>%
  filter(pm==0) %>% 
  filter(group %in% c('FACOMS blood mature', 'FACHILD blood mature')) %>%
  group_by(group, sev.o ) %>%
  summarise(
    n = n(),
    mean_fxn_log =    mean(fxn, na.rm = TRUE),
    mean_fxn     = 10^mean(fxn, na.rm = TRUE),
  ) %>% 
  group_by(group) %>% 
  mutate(unit = case_when(
    group == 'FACOMS blood mature'  ~ '% of ctrl',
    group == 'FACHILD blood mature' ~ 'ng/ml',
    TRUE ~ 'pmol/mg/min'
  )) %>% 
  mutate(group_diff = mean_fxn-min(mean_fxn)) %>% 
  mutate(group_mult = mean_fxn/min(mean_fxn))

results. %>% .ct

# can I model these?  -----------------------------------------------------

model. <- fxn. %>% 
  filter(!is.na(sev.o)) %>%
  filter(pm==0) %>% 
  filter(group == 'FACOMS blood mature') %>%
  group_by(group) %>% select(-data) %>%
  nest() %>% 
  # mutate  (lm.mod = map( data,   ~ lm(fxn ~ sev.o + gaa100 + aoo, data = .x))) %>% 
  mutate  (lm.mod = map( data,   ~ lm(fxn ~ sev.o, data = .x))) %>%
  # mutate  (lm.mod = map( data,   ~ lm(fxn ~ sev.o + gaa100, data = .x))) %>% 
  # pull(lm.mod)
  mutate  (coefs  = map( lm.mod, ~ broom::tidy(., conf.int = T))) %>% 
  unnest(coefs) %>% 
  filter(term != '(Intercept)') %>%
  select(-lm.mod, -data) %>%
  .fixmod

results. %>% 
  left_join(
    model. %>% mutate(sev.o = gsub('sev.o','', term)) %>% select(group, sev.o, estimate, conf.low, conf.high,  p.value) %>%
      mutate_at(vars(estimate, conf.low, conf.high), ~10^.)
    ) %>% 
  select(group, sev.o, n, mean_fxn_log, mean_fxn, estimate, conf.low, conf.high, p.value, unit, group_diff, group_mult)

fxn.lm <- fxn. %>% 
  filter(!is.na(sev.o)) %>%
  filter(pm==0) %>% 
  filter(group == 'FACOMS blood mature') %>%
  lm( fxn ~ sev.o, data = .)

fxn.lm %>% 
  broom::tidy(effects = "fixed", conf.int = T) %>% 
  mutate_at(vars(estimate, conf.low, conf.high), ~10^.) %>%
  mutate( term = gsub('sev.o','', term )) %>% 
  # mutate( estimate = estimate/max(estimate)) %>% 
  ggplot()+geom_pointrange()+
  aes(x = term, y = estimate, ymin = conf.low, ymax = conf.high)+
  geom_hline(yintercept = 10, linetype = 'dashed')


norm.coef <- 10^coef(fxn.lm)

sjPlot::plot_model(fxn.lm, type = "est", coef = norm.coef)
sjPlot::plot_model(fxn.lm, type = 'est')

# graph means -------------------------------------------------------------


means <- fxn. %>%
  filter(!is.na(sev.o)) %>%
  filter(group %in% c('FACOMS blood mature', 'FACHILD blood mature')) %>%
  group_by(sev.o, group) %>%
  summarise(mean_fxn = 10^mean(fxn, na.rm = TRUE))

fxn. %>% 
  filter(!is.na(sev.o)) %>% 
  filter(group %in% c('FACOMS blood mature', 'FACHILD blood mature')) %>%
  # unnest(data) %>%
  ggplot()+
  geom_histogram() +
  # geom_density(alpha = .5) +
  aes(x = 10^fxn)+
  aes(fill = sev.o)+scale_fill_brewer(palette = 'Set1')+
  geom_vline(data = means, aes(xintercept = mean_fxn), 
             color = "blue", linetype = "dashed") +
  facet_grid(sev.o~group, scales ='free_x')+
  .leg_none
  
  

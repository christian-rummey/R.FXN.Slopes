
rm(list = ls() )
source('DM.FXN.combine.R')

# fxn by onset group, all pts -------------------------------------------------------------------

results. <- fxn. %>%
  filter(type != 'fxn.t') %>% 
  group_by( study, tissue, status, type, sev.o, unit) %>% 
  filter(!is.na(sev.o)) %>%
  filter(pm==0) %>% 
  # filter(group %in% c('FACOMS blood mature', 'FACHILD blood mature')) %>%
  # group_by(group, sev.o ) %>%
  summarise(
    n = n(),
    mean_fxn       =    mean(      value , na.rm = TRUE),
    mean_fxn_log   =    mean(log10(value), na.rm = TRUE),
  ) %>% 
  group_by( study, tissue, status, type, unit) %>% 
  # mutate(unit = case_when(
  #   group == 'FACOMS blood mature'  ~ '% of ctrl',
  #   group == 'FACHILD blood mature' ~ 'ng/ml',
  #   TRUE ~ 'pmol/mg/min'
  # )) %>% 
  arrange(type, study, tissue, sev.o) %>% 
  mutate(group_diff = mean_fxn-min(mean_fxn)) %>% 
  mutate(group_mult = mean_fxn/min(mean_fxn))

results. %>% .ct


# boxplot -----------------------------------------------------------------

with(fxn., table(study, type))

fxn.tmp  <- fxn. %>%
  mutate( sev.ox = as.character(sev.o) ) %>% 
  mutate(sev.ox = case_when(
    status == 'control' ~ 'control',
    status == 'carrier' ~ 'carrier',
    TRUE ~ as.character(sev.o)
  )) %>%
  filter(study != 'FACOMS') %>% 
  filter(type != 'fxn.t')

fxn.counts <- fxn.tmp %>% 
  filter(study != 'FACOMS', type != 'fxn.t') %>%
  mutate(sev.ox = factor(sev.ox, c('0-7y','8-14y','15-24y','>24y','carrier','control'))) %>%
  group_by(sev.ox, study, tissue, status, type, unit) %>%
  summarise(n = n(), .groups = 'drop')

# Plot with n's above boxes
fxn.tmp %>%
  mutate(sev.ox = case_when(
    status == 'control' ~ 'control',
    status == 'carrier' ~ 'carrier',
    TRUE ~ as.character(sev.o)
  )) %>%
  filter(study != 'FACOMS', type != 'fxn.t') %>%
  mutate(sev.ox = factor(sev.ox, c('0-7y','8-14y','15-24y','>24y','carrier','control'))) %>%
  ggplot(aes(x = sev.ox, y = value, fill = sev.ox)) +
  geom_boxplot() +
  scale_y_log10() +
  geom_text(
    data = fxn.counts,
    aes(x = sev.ox, y = 35, label = paste0("n=", n)),
    vjust = -0.5,
    inherit.aes = FALSE
  ) +
  facet_wrap(~ type + study) +
  .theme()+
  .leg('none')

# fxn by onset group, all pts -------------------------------------------------------------------

# results. <- fxn. %>%
#   filter(!is.na(sev.o)) %>%
#   left_join(dt.) %>% filter(!is.na(paramcd)) %>% 
#   filter(pm==0) %>% 
#   filter(group %in% c('FACOMS blood mature', 'FACHILD blood mature')) %>%
#   group_by(group, sev.o ) %>%
#   summarise(
#     n = n(),
#     mean_fxn_log =    mean(fxn, na.rm = TRUE),
#     mean_fxn     = 10^mean(fxn, na.rm = TRUE),
#   ) %>% 
#   group_by(group) %>% 
#   mutate(unit = case_when(
#     group == 'FACOMS blood mature'  ~ '% of ctrl',
#     group == 'FACHILD blood mature' ~ 'ng/ml',
#     TRUE ~ 'pmol/mg/min'
#   )) %>% 
#   mutate(group_diff = mean_fxn-min(mean_fxn)) %>% 
#   mutate(group_mult = mean_fxn/min(mean_fxn))
# 
# results. %>% .ct

fxn.dt. <- fxn. %>% 
  unnest(data) %>% group_by(sjid)

# fxn by percentile group, all pts -------------------------------------------------------------------

results. <- fxn. %>%
  filter(!is.na(sev.o)) %>%
  # left_join(dt.) %>% filter(!is.na(paramcd)) %>% 
  filter(pm==0) %>% 
  # filter(group %in% c('FACOMS blood mature', 'FACHILD blood mature')) %>%
  group_by(group, fxnpct_group ) %>%
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

# fxn by percentile group, pts with slope -------------------------------------------------------------------

results. <- fxn. %>%
  filter(!is.na(sev.o)) %>%
  left_join(dt.) %>% filter(!is.na(paramcd)) %>%
  unnest ( data) %>% 
  filter ( pm==0) %>% 
  # filter(sjid == 5) %>% 
  filter  ( group %in% c('FACOMS blood mature', 'FACHILD blood mature')) %>%
  # select  ( site, sjid, status, time., bl, aval ) %>% print(n=20)%>%
  group_by( sjid, status, group, fxnpct_group ) %>% 
  summarize(
    slope1 = cov(time., aval) / var(time.), 
    slope = coef(lm(aval ~ time.))[2]) %>% 
  group_by(group, fxnpct_group ) %>%
  summarise(
    n = n(),
    mean_slope =    mean(slope, na.rm = TRUE)
    )

%>% 
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
  
  

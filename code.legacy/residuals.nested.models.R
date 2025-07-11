fxn.nest <- fxn.dt. %>% 
  filter( group == 'FACOMS blood mature' ) %>%
  filter(pm==0) %>%
  group_by( group, paramcd ) %>% nest()

models <- bind_rows(
  fxn.nest %>% mutate(m='FF')%>% mutate( mod.fxn = map( data, ~ lmer(aval ~ bl + fxn + time. + (1 + time.|sjid) , data = . , control = .lme4.ctrl ))),
  fxn.nest %>% mutate(m='FF')%>% mutate( mod.fxn = map( data, ~ lmer(aval ~ bl + aoo + time. + (1 + time.|sjid) , data = . , control = .lme4.ctrl ))),
  fxn.nest %>% mutate(m='FF')%>% mutate( mod.fxn = map( data, ~ lmer(aval ~ bl + gaa100 + time. + (1 + time.|sjid) , data = . , control = .lme4.ctrl ))),
  fxn.nest %>% mutate(m='3FF')%>% mutate( mod.fxn = map( data, ~ lmer(aval ~ bl + fxn + aoo + gaa100 + time. + (1 + time.|sjid) , data = . , control = .lme4.ctrl ))),
  fxn.nest %>% mutate(m='T+I')%>% mutate( mod.fxn = map( data, ~ lmer(aval ~ bl + fxn*time.+ (1 + time.|sjid) , data = . , control = .lme4.ctrl ))),
  fxn.nest %>% mutate(m='Io') %>% mutate( mod.fxn = map( data, ~ lmer(aval ~ bl + fxn:time.+ (1 + time.|sjid) , data = . , control = .lme4.ctrl ))),
  fxn.nest %>% mutate(m='T+I')%>% mutate( mod.fxn = map( data, ~ lmer(aval ~ bl + aoo:time.    + time.   + (1 + time.|sjid) , data = . , control = .lme4.ctrl ))),
  fxn.nest %>% mutate(m='Io') %>% mutate( mod.fxn = map( data, ~ lmer(aval ~ bl + aoo:time.    + (1 + time.|sjid) , data = . , control = .lme4.ctrl ))),
  fxn.nest %>% mutate(m='T+I')%>% mutate( mod.fxn = map( data, ~ lmer(aval ~ bl + gaa100:time. + time.   + (1 + time.|sjid) , data = . , control = .lme4.ctrl ))),
  fxn.nest %>% mutate(m='Io') %>% mutate( mod.fxn = map( data, ~ lmer(aval ~ bl + gaa100:time. + (1 + time.|sjid) , data = . , control = .lme4.ctrl ))),
  fxn.nest %>% mutate(model = 'fxn+I') %>% mutate( mod.fxn = map( data, ~ lmer(aval ~ bl + fxn + fxn:time.    + (1 + time.|sjid) , data = . , control = .lme4.ctrl ))),
  fxn.nest %>% mutate(model = 'aoo+I') %>% mutate( mod.fxn = map( data, ~ lmer(aval ~ bl + aoo + aoo:time.    + (1 + time.|sjid) , data = . , control = .lme4.ctrl ))),
  fxn.nest %>% mutate(model = 'gaa+I') %>% mutate( mod.fxn = map( data, ~ lmer(aval ~ bl + gaa100 + gaa100:time. + (1 + time.|sjid) , data = . , control = .lme4.ctrl )))
)

models %>% 
  mutate( fixed.effects = map( mod.fxn, ~ tidy   ( .x, effects = "fixed", conf.int = T ))) %>%
  unnest( fixed.effects ) %>% 
  select(-data, -mod.fxn, -effect, -statistic, -df ) %>% 
  filter( term != '(Intercept)')%>% 
  # filter( term != 'bl') %>% 
  # mutate_at(vars(estimate, conf.low, conf.high), ~ 10^.) %>%
  # arrange(group, m) %>% 
  # filter( model == 'interaction only') %>% 
  select( group, paramcd, m, term, estimate, std.error, conf.low, conf.high, p.value) %>%
  .fixmod() %>% 
  # filter(term != 'time.') %>% arrange(group) %>% .ct
  mutate( term = factor(term, c('bl','time.','aoo','aoo:time.','gaa100','gaa100:time.','fxn','fxn:time.'))) %>% arrange(term) %>% 
  print(n=Inf) %>% #filter(term=='fxn')
  ggplot()+geom_point()+
  aes(x = term, y = estimate)+
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high))+
  facet_wrap(~paste(group, m))+
  geom_hline(yintercept = 0, linetype = 2)+
  geom_text(aes(label=p.value), size = 3, y = 0.5)



rm(list = ls())

fars.slopes.nodup <- readRDS('../R.RCR.Modeling/DATA derived/fars.slopes.descr.nodup.rds')
fars.slopes       <- readRDS('../R.RCR.Modeling/DATA derived/fars.slopes.descr.dup.rds')

fars.slopes <- fars.slopes %>% 
  left_join(fars.slopes.nodup) %>% 
  left_join(
    .dd('demo') %>% select(study, sjid, sev.o)
  ) %>% 
  filter(!is.na(sev.o))

fars.slopes %>% 
  group_by(study, sev.o, paramcd, phase.n) %>% 
  summarise(
    n = n(),
    slope       = mean(slope),
    slope.nodup = mean(slope.nodup, na.rm=T)
  ) %>% 
  pivot_wider(
    names_from  = c('paramcd'),
    values_from = c('slope', 'slope.nodup')
  ) %>% 
  filter(phase.n == 1 ) %>% # it is always identical in phase 2
  select(study, sev.o, contains('FARS.E'), contains('FARS.B'), contains('mFARS'))

# correlation plots -------------------------------------------------------

fars.slopes %>% 
  filter(phase.n == 1 ) %>% # it is always identical in phase 2
  filter(paramcd != 'mFARS') %>% 
  filter(!is.na(slope.nodup)) %>% 
  ggplot()+geom_point()+
  aes( x = slope, y = slope.nodup )+
  facet_wrap(phase.n~paramcd)

# NH graph ----------------------------------------------------------------

fars.slopes %>% filter(paramcd == 'FARS.B') %>% 
  filter(phase.n == 1) %>%
  select(-contains('obs')) %>% 
  gather( type, aval, slope.nodup, slope ) %>% 
  group_by( sev.o, paramcd, phase.n, type ) %>% 
  summarise(aval = mean(aval, na.rm=T)) %>% 
  # .ug %>% select(grp) %>% .tab
  ggplot()+geom_col(position = position_dodge(width=.75))+
  aes(x = sev.o)+
  aes(y = aval)+
  aes(alpha = type)+scale_alpha_manual(values = c(0.5, 1))+
  aes(fill = sev.o)+.sfbs1+
  facet_wrap(~phase.n+paramcd, ncol = 2)+
  .theme()

.sp(ti= 'Slopes, dup vs nodup')

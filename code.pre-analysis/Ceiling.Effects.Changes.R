
dt. <- .dd('fars.slope.chg.rds') %>%
  filter(sjid != 4988) %>% 
  group_by( study, sjid, paramcd, phase) %>% 
  filter(study == 'UNIFAI') %>%
  # filter(paramcd %in% c('mFARS','FARS.E')) %>% 
  # filter( !is.na(aval)) %>% 
  # filter( !is.na(adt) ) %>% 
  select(-dupline)

# 1y and 2y changes for all pars ------------------------------------------

dt.means <- dt. %>% 
  filter(!is.na(chg)) %>% 
  mutate(aval.grp = cut(aval, seq(0,100,5) , include.lowest = T )) %>% 
  group_by( paramcd, phase, aval.grp, int ) %>% 
  mutate( phase = factor(phase, c(1,2), c('ambulatory','non-amb.'))) %>% 
  filter( !is.na(phase) ) %>% 
  filter( !(paramcd == 'FARS.E' & phase == 2) ) %>% 
  summarise( n = n(), mchg = mean(chg), sd = sd(chg, na.rm=T) ) %>% 
  filter(n>1)

# individual figures ------------------------------------------------------
# mFARS, FARS.E, ambulatory -----------------------------------------------

dt.means %>% 
  filter(paramcd %in% c('FARS.E','mFARS')) %>% 
  filter(phase == 'ambulatory') %>% 
  ggplot()+geom_point()+geom_line()+
  geom_text(nudge_y= -0.5, aes(label = n), size = 2)+
  geom_errorbar(width=.1)+
  aes(ymin = mchg-sd,ymax = mchg+sd )+
  aes(group = paste(paramcd, int))+
  aes(x = aval.grp)+
  aes(color = int)+.scbs1+
  aes(y = mchg)+
  facet_wrap(~ paramcd, ncol = 1)+
  geom_hline( yintercept = 0)+
  .theme(base_size = 12)+
  coord_cartesian(ylim = c(-5, 7.5))

# .sp(ti = 'ambulatory phase')

# mFARS, FARS.B, non-ambulatory -----------------------------------------------

dt.means %>% 
  filter(paramcd %in% c('FARS.B','mFARS')) %>% 
  filter(phase != 'ambulatory') %>% 
  ggplot()+geom_point()+geom_line()+
  geom_text(nudge_y= -0.5, aes(label = n), size = 2)+
  geom_errorbar(width=.1)+
  aes(ymin = mchg-sd,ymax = mchg+sd )+
  aes(group = paste(paramcd, int))+
  aes(x = aval.grp)+
  aes(color = int)+.scbs1+
  aes(y = mchg)+
  facet_wrap(~ paramcd, ncol = 1)+
  geom_hline( yintercept = 0)+
  .theme(base_size = 12)+
  coord_cartesian(ylim = c(-5, 7.5))

# .sp(ti = 'non-ambulatory phase')


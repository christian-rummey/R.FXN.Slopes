

dt. <- .dd('fars.descriptive.slopes') %>%

  group_by( study, sjid, paramcd, phase) %>% 
  filter(study == 'UNIFAI') %>%
  # filter(paramcd %in% c('mFARS','FARS.E')) %>% 
  # filter( !is.na(aval)) %>% 
  # filter( !is.na(adt) ) %>% 
  droplevels()

# 1y and 2y changes for all pars ------------------------------------------

dt.means <- dt. %>% 
  # filter(is.na(slope)) %>% 
  mutate( bl.grp = cut(bl, seq(0,100,5) , include.lowest = T )) %>% 
  # filter(is.na(bl.grp)) %>%
  group_by( paramcd, phase, bl.grp ) %>% 
  mutate( phase = factor(phase, c(1,2), c('ambulatory','non-amb.'))) %>% 
  filter( !is.na(phase) ) %>% 
  filter( !(paramcd == 'FARS.E' & phase == 2) ) %>% 
  summarise( n = n(), mchg = mean(slope), sd = sd(slope) ) %>% 
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
  aes(group = paste(paramcd))+
  aes(x = bl.grp)+
  # aes(color = int)+.scbs1+
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
  aes(group = paste(paramcd))+
  aes(x = bl.grp)+
  # aes(color = int)+.scbs1+
  aes(y = mchg)+
  facet_wrap(~ paramcd, ncol = 1)+
  geom_hline( yintercept = 0)+
  .theme(base_size = 12)+
  coord_cartesian(ylim = c(-5, 7.5))

# .sp(ti = 'non-ambulatory phase')


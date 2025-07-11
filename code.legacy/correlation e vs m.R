
rm(list = ls() )
source('DM.FXN.combine.R')

fxn. %>% 
  group_by( sjid, type ) %>% 
  slice(2) %>% 
  .ug %>% 
  group_by( study, status ) %>% tally


fxn. %>% 
  mutate(sev.ox = case_when(
    status == 'control' ~ 'control',
    status == 'carrier' ~ 'carrier',
    TRUE ~ as.character(sev.o)
  )) %>%
  filter(study != 'FACOMS', type != 'fxn.t') %>%
  mutate(sev.ox = factor(sev.ox, c('0-7y','8-14y','15-24y','>24y','carrier','control'))) %>% 
  spread( type, value ) %>% 
  # filter( is.na(sev.ox ))
  ggplot()+geom_point()+
  aes( x = fxn.e, y = fxn.m )+
  aes( color = sev.ox)+
  scale_x_log10()+scale_y_log10()+
  facet_wrap(~ study, ncol = 2) +
  geom_smooth(method = lm, color = 'black')+
  .theme()+
  .leg('tr')

# controls correlation only -----------------------------------------------

fxn. %>% 
  filter ( study == 'Ians Lab') %>% 
  filter ( status %in% c('carrier','control') ) %>% 
  mutate ( sitex = substr(sjid, 1, 2) ) %>% 
  spread( type, value ) %>% 
  # filter( is.na(sev.ox ))
  ggplot()+geom_point()+
  aes( x = fxn.e, y = fxn.m )+
  aes( color = sitex)+
  aes( shape = status)+.ssmB+
  scale_x_log10()+scale_y_log10()+
  facet_wrap(~ sitex, ncol = 2) +
  # geom_smooth(method = lm, color = 'black')+
  .theme()+
  .leg('tr')

# gaa1 vs fxn.e -----------------------------------------------------------

fxn. %>% 
  filter(sjid != 'CHP035') %>% 
  mutate(sev.ox = case_when(
    status == 'control' ~ 'control',
    status == 'carrier' ~ 'carrier',
    TRUE ~ as.character(sev.o)
  )) %>%
  filter(study != 'FACOMS', type != 'fxn.t') %>%
  mutate(sev.ox = factor(sev.ox, c('0-7y','8-14y','15-24y','>24y','carrier','control'))) %>% 
  spread( type, value ) %>% 
  ggplot()+geom_point()+
  aes( x = gaa1, y = fxn.e )+
  aes( color = sev.ox)+
  # scale_x_log10()+
  scale_y_log10()+
  facet_wrap(~ study, ncol = 2) +
  geom_smooth(method = lm, color = 'black')+
  # geom_smooth( formula = y ~ poly(x, 2), method = 'lm', color = 'blue')
  # geom_smooth( method = lm, color = 'red')+
  # stat_regline_equation()+
  stat_cor               (aes(label = paste(..rr.label..), group= 1), r.digits = 2)+
  # ggpmisc::stat_correlation(aes(label = paste(..rr.label..)))+
  # geom_abline(slope = y.max/x.max, linetype = 'dotted') +
  .theme()+
  .leg('lr')

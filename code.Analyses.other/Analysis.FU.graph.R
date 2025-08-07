
# source('code.DM/DM.FXN Slope models DM.R')
# source('code.DM/DM.FXN.3.add.clinical.data.R')
fxn.dt. <- read_rds('DATA derived/fxn.dt.rds')


# summaries ---------------------------------------------------------------

# pts with only one mFARS
fxn.dt. %>%
  filter ( paramcd == 'mFARS' ) %>% 
  filter(map_int(data, nrow) == 1) %>% 
  group_by( analysis.group, sev.o, paramcd, phase ) %>%
  summarise(n = n()) %>% 
  spread( sev.o, n )

# pts with FU
fxn.dt. %>%
  filter ( paramcd == 'mFARS' ) %>% 
  filter ( type    == 'FXN-M' ) %>% 
  filter(map_int(data, nrow) > 1) %>% 
  group_by( analysis.group, paramcd, phase ) %>%
  mutate  ( N = n() ) %>% 
  group_by( analysis.group, sev.o, paramcd, phase, N ) %>%
  summarise( n = n() ) %>% 
  mutate   ( pct  = round(100*n/N,1) ) %>% 
  select(-n, -N) %>% 
  spread   ( sev.o, pct ) %>% 
  arrange ( phase )

fxn.dt.sum <- fxn.dt. %>%
  filter ( pm==0) %>% 
  filter ( paramcd == 'mFARS' ) %>% 
  filter(map_int(data, nrow) > 1) %>% 
  group_by( analysis.group, sjid, sev.o, paramcd, phase ) %>%
  unnest  ( data ) %>% 
  summarise(
    visits = n(),
    # period = as.numeric(max(adt)-min(adt))/365.25
    period = max(time.)
  ) %>% 
  group_by( analysis.group, sev.o, paramcd, phase ) %>%
  summarise(
    visits = mean(visits),
    period = mean(period)
  ) %>%
  filter(!is.na(sev.o))

fxn.dt.sum %>%
  filter(analysis.group != 'lcms fxn.m') %>% 
  ggplot()+geom_col(position = position_dodge(width = 0.75))+
  aes(x = sev.o, y = period )+
  aes(fill = sev.o )+.sfbs1+
  aes(alpha = factor(phase))+scale_alpha_manual(values = c(1,0.5))+
  facet_grid(phase~analysis.group)+
  geom_text(aes(label = round(visits,1), y = period))+
  .theme()

# .sp('Amount of FU by Analysis group')


# source('code.DM/DM.FXN Slope models DM.R')
source('code.DM/DM.FXN.3.add.clinical.data.R')

# summaries ---------------------------------------------------------------

# pts with only one mFARS
fxn.dt. %>%
  filter ( paramcd == 'mFARS' ) %>% 
  filter(map_int(data, nrow) == 1) %>% 
  group_by( analysis.group, sev.o, paramcd, amb ) %>%
  summarise(n = n()) %>% 
  spread( sev.o, n )

# pts with FU
fxn.dt. %>%
  filter ( paramcd == 'mFARS' ) %>% 
  filter ( type    == 'fxn.m' ) %>% 
  filter(map_int(data, nrow) > 1) %>% 
  group_by( analysis.group, paramcd, amb ) %>%
  mutate  ( N = n() ) %>% 
  group_by( analysis.group, sev.o, paramcd, amb, N ) %>%
  summarise( n = n() ) %>% 
  mutate   ( pct  = round(100*n/N,1) ) %>% 
  select(-n, -N) %>% 
  spread   ( sev.o, pct ) %>% 
  arrange ( amb )

fxn.dt.sum <- fxn.dt. %>%
  filter ( pm==0) %>% 
  filter ( paramcd == 'mFARS' ) %>% 
  filter(map_int(data, nrow) > 1) %>% 
  group_by( analysis.group, sjid, sev.o, paramcd, amb ) %>%
  unnest  ( data ) %>% 
  summarise(
    visits = n(),
    period = as.numeric(max(adt)-min(adt))/365.25
  ) %>% 
  group_by( analysis.group, sev.o, paramcd, amb ) %>%
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
  aes(alpha = amb)+scale_alpha_manual(values = c(1,0.5))+
  facet_grid(amb~analysis.group)+
  geom_text(aes(label = round(visits,1), y = period))+
  .theme()

# .sp('Amount of FU by Analysis group')

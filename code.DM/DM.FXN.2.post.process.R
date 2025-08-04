
rm(list=ls())
source('code.DM/DM.FXN.1.combine.R')

fxn. %<>% 
  filter( study  != 'TRACKFA') %>% 
  filter( tissue == 'blood'  ) %>% 
  filter( type   != 'fxn.t'  ) %>% 
  droplevels()

fxn. %<>% 
  mutate( assay = ifelse( unit == '%', 'LF Cohort [%]','TQ Cohort [ng/ml]' ) )

# average multiple measures 

fxn. %<>% 
  select(-n) %>% 
  group_by( sjid, status, assay, type ) %>% 
  arrange ( sjid, status, assay, type ) %>% 
  # filter  ( n()>1 ) %>% 
  mutate  ( value = mean(value) ) %>% 
  slice   (1)

# define analysis groups 

fxn. %>% .ug %>% 
  select(status, assay, type) %>% 
  .tab

fxn. %<>% .ug %>% 
  mutate( analysis.group = paste ( assay, type ))

# -------------------------------------------------------------------------

fxn. %>% .ug %>% 
  select( analysis.group, status ) %>% .tab

fxn. %>% 
  select( analysis.group, sev.o ) %>% .tab

fxn. %<>%
  filter( status != 'unknown' ) %>% 
  mutate( sev.o = as.character(sev.o) ) %>% 
  mutate( sev.o = case_when(
    status == 'control' ~ 'control',
    status == 'carrier' ~ 'carrier',
    TRUE ~ as.character(sev.o)
  )) %>%
  mutate( sev.o = factor( sev.o, c(levels(fxn.$sev.o), 'carrier','control')))

fxn. %>%
  select( assay, sev.o ) %>% .tab

# require -----------------------------------------------------------------
# 2nd sheet from DL email Tue, Jun 24, 3:50 AM 
# there was an earlier sheet with TRACKFA values that are not included here

rm(list=ls())

fxn.track <- read_excel('DATA/first round/FXN-M and FXN-E all cases 06-19-25 - study added.xlsx') %>% 
  filter( study == 'TRACKFA' ) %>% 
  mutate( sjid = gsub("\\s+", "", FACOMS) ) %>% 
  mutate( sjid = gsub('TRACKFA_','', sjid) ) %>% 
  rename( fxn.m = `FXN-M`, fxn.e = `FXN-E`, fxn.t = Total ) %>% 
  select( study, sjid, starts_with('fxn.'))

fxn.track %<>% 
  left_join(
    .dd('demo.l') %>% 
      select(study, site, sjid, dob, sex, sev.o, aoo, gaa1, gaa2, pm)
  ) %>% 
  mutate( gaa1 = ifelse( sjid == 5483, 693, gaa1 )) %>% # email DL 2025-06-27
  mutate( gaa2 = ifelse( sjid == 5483, 849, gaa2 )) %>% 
  mutate( pm   = ifelse( sjid == 5483,   0, pm   )) %>% 
  # filter(sjid == 5483)
  droplevels()

fxn.track %>% .ug %>% 
  gather ( type, value, starts_with( "fxn." ) ) %>% 
  mutate ( status = 'patient' ) %>% 
  mutate ( tissue = 'blood' ) %>% 
  mutate ( unit   = 'ng/ml' ) %>% 
  select ( study, status, sjid, dob, sev.o, sex, aoo, gaa1, gaa2, pm, tissue, type, value, unit ) %>% 
  write_rds('DATA derived/fxn.track.rds')


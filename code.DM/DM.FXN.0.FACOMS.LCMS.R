
# require -----------------------------------------------------------------
# 2nd sheet from DL email Tue, Jun 24, 3:50 AM 
# there was an earlier sheet with TRACKFA values that are not included here

rm(list=ls())

dt. <- read_excel('DATA/4CRDLFXN-M and FXN-E all cases 06-19-25-CR.xlsx')


# correct duplicates ------------------------------------------------------

duplicates <- .rt('../DATA other/FACOMS.duplicates.txt') %>% 
  filter(!is.na(sjid.d1))  %>%
  mutate(across(c(sjid, sjid.d1), as.character))

dt. %<>% 
  # filter(sjid %in% duplicates$sjid.d1) %>%
  left_join(
    duplicates %>%
      filter(!is.na(study)) %>% 
      select(sjid = sjid.d1, duplicate.of = sjid)
  ) %>%
  mutate( sjid = ifelse( !is.na(duplicate.of), duplicate.of, sjid ) ) %>% 
  select( -duplicate.of )

rm(duplicates)

# demo --------------------------------------------------------------------

dt. %<>% 
  mutate( study = 'UNIFAI' ) %>% 
  filter( !sjid %in% c('43','5186')) %>% # as per DL email
  mutate( sjid = as.numeric (sjid )) %>% 
  mutate( status = ifelse(sjid > 6000, 'control', 'patient')) %>% 
  mutate( sjid = as.character (sjid )) %>% 

  rename( fxn.m = `FXN-M`, fxn.e = `FXN-E`, fxn.t = Total) %>% 
  
  select( study, sjid, status, starts_with('fxn.')) %>% 
  
  left_join(
    .dd('demo.l') %>% 
      select( study, site, sjid, dob, sex, sev.o, aoo, gaa1, gaa2, pm)
    ) %>% 
  mutate( gaa1 = ifelse( sjid == 5483, 693, gaa1 )) %>% # email DL 2025-06-27
  mutate( gaa2 = ifelse( sjid == 5483, 849, gaa2 )) %>% 
  mutate( pm   = ifelse( sjid == 5483,   0, pm   )) %>% 
  # filter(sjid == 5483)
  droplevels()

# dt. %>% 
#   arrange(sjid) %>% 
#   group_by( sjid ) %>% 
#   filter ( n()>2 ) %>%.p 
#   gather ( paramcd, aval, )

dt. %>% .ug %>% 
  gather ( type, value, starts_with( "fxn." ) ) %>% 
  # mutate ( status = 'patient' ) %>% 
  mutate ( tissue = 'blood' ) %>% 
  mutate ( unit   = 'ng/ml' ) %>% 
  select ( study, status, sjid, dob, sev.o, sex, aoo, gaa1, gaa2, pm, tissue, type, value, unit ) %>% 
  write_rds('DATA derived/fxn.lcms.rds')


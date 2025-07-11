
# require -----------------------------------------------------------------
# 2nd sheet from DL email Tue, Jun 24, 3:50 AM 
# there was an earlier sheet with TRACKFA values that are not included here

rm(list=ls())

fxn.controls <- read_excel('DATA/CHOP, UCLA, Florida, Oklahoma 200 Controls and carriers 100.xlsx', 1) %>%
  select(-Date) %>% 
  rename(
    sex = `M=1/F=2`,
    fxn.m = `Mean FXN-M (ng/mL)`,
    fxn.e = `Mean FXN-E (ng/mL)`,
    fxn.t = `Mean Total (ng/mL)`
    )

fxn.carriers <- read_excel('DATA/CHOP, UCLA, Florida, Oklahoma 200 Controls and carriers 100.xlsx', 2) %>%
  select(-Date) %>% 
  rename(
    sex = `M=1/F=2`,
    fxn.m = `FXN-M (ng/mL)`,
    fxn.e = `FXN-E (ng/mL)`,
    fxn.t = `Total FXN (ng/mL)`
  )

fxn.controls <- bind_rows(
  fxn.carriers %>% mutate( status = 'carrier'),
  fxn.controls %>% mutate( status = 'control')
  ) %>% 
  mutate( unit = 'ng/ml' ) %>% 
  gather( type, value, starts_with('fxn.') ) %>% 
  mutate(
    sex    = ifelse( sex == 1, 'm', 'f' ),
    tissue = 'blood',
    study  = 'external (Ian)'
    ) %>% 
  select ( study, status, sjid = ID, tissue, type, value, unit )


fxn.controls %<>% .ug %>% 
  write_rds('DATA derived/fxn.controls.rds')


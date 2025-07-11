

rm(list = ls() )
source('DM.FXN.combine.R')

fxn. %>% 
  select( study, status ) %>% .tab

# double controls ---------------------------------------------------------

# fxn. %>% .ug %>% 
#   filter(status %in% c('control','carrier') ) %>% 
#   group_by( status, sjid, type, tissue, value ) %>% 
#   filter(n()>1)

# patients ----------------------------------------------------------------
# some FACHILD samples are included (29) in UNIFAI set, some not (61). 
# the values are identical
# these are now removed in the combine step

# fxn. %>% .ug %>% 
#   filter( status %in% c('patient') ) %>% 
#   # filter(sjid == 136) %>% 
#   group_by( status, sjid, type, tissue, value ) %>% 
#   arrange ( status, sjid, type, tissue, value ) %>% 
#   mutate  ( n = n() ) %>%  
#   filter(study == 'FACHILD', type == 'fxn.e') %>% 
#   filter ( n==2 ) %>%
#   # filter ( n==1 ) %>%
#   .p

# which patients in which set ---------------------------------------------
# TRACKFA months were scrambled

.dd('demo.l') %>% 
  filter( sjid %in% c(
    '4808','CHP039',
    'CHP035','4999',
    'UFL006' , '5026' ,
    'UFL004' , '5031')) %>% 
  arrange(dob) %>% filter(study!='FACOMS')

# arrange by month --------------------------------------------------------
# & remove externals

fxn. %<>% 
  filter( !grepl('external', study)) %>% 
  mutate( dob.m = paste(year(dob), month(dob)) )

# average duplicate measures ----------------------------------------------

fxn. %<>% 
  group_by( study, sjid, status, type, tissue ) %>%
  # filter  ( n()>1 ) %>% 
  # arrange ( sjid, study, tissue, type ) %>% .p
  mutate  ( value = mean(value, na.rm = TRUE) ) %>%
  slice_head(n = 1)  # Or distinct(across(...)) if needed

# figure out UDs ----------------------------------------------------------

fxn. %<>% .ug %>% 
  select( -c(tissue, type, value, unit, n)) %>% 
  # filter( sjid == 162 ) %>%
  unique() %>% 
  # filter( aoo == 11, gaa1 == 660 ) %>% 
  group_by(dob, dob.m, sex, aoo, gaa1, gaa2, pm, study) %>%
  # filter(rowid>1)
  ungroup()

fxn. %<>% 
  bind_rows(
    .dd('demo.l') %>% 
      filter( study == 'UNIFAI'     ) %>% 
      mutate( study  = 'UNIFAI.all' ) %>% 
      mutate( dob.m = paste(year(dob), month(dob)) ) %>% 
      select( study, sjid, site, dob, sev.o, dob.m, sex, aoo, gaa1, gaa2, pm )
    )  %>% 
  select(-status)

# but now -----------------------------------------------------------------

# date.x <- as.Date('1992-10-08')
# date.l <- date.x - 30
# date.h <- date.x + 30

fxn.  %>% 
  # filter(study == 'TRACKFA')
  filter(is.na(save.id)) %>% 
  arrange(gaa1, gaa2, dob) %>%
  slice(2301:3000) %>%
  filter(study %in% c('UNIFAI.all','TRACKFA')) %>% .p
  # filter(site == 'UF') %>% 
  # filter(sjid == 'UFL009')
  # filter( study == 'TRACKFA', is.na(save.id), site == 'UF' )
  filter( dob > date.l, dob < date.h )
%>%
  filter(sjid == 'UFL017') %>% 
  filter( !(is.na(aoo) & is.na(gaa1) & is.na(gaa2) & is.na(pm)) ) %>% 
  # group_by(dob.m, sex, aoo, gaa1, gaa2, pm) %>% 
  arrange (dob.m, sex, aoo, gaa1, gaa2, pm) %>% 
  distinct(dob.m, sex, aoo, gaa1, gaa2, pm, .keep_all = TRUE) %>%
  spread ( study, sjid) %>% 
  filter(!is.na(TRACKFA))


    # slice(180:185) %>%
  filter(sjid %in% c('10','UFL021','5150')) %>%
  mutate(rowid = row_number()) %>%    # disambiguate same-group entries
  pivot_wider(
    names_from  = c(study, dob.m),
    values_from = c(sjid),
    id_cols     = c(site, sex, aoo, gaa1, gaa2, pm, rowid),
    values_fn   = identity
  ) %>% 
  arrange(gaa1) %>% 
  spread( dob.m )

  # optional
  select(-rowid) %>% 
  unique() %>% 
  arrange(dob.m, gaa1, gaa2) %>% 
  # mutate (days = abs(round(as.numeric(lag(dob)-dob)))) %>% 
  # mutate (days = ifelse(days<31, days, NA)) %>% 
  # .p
  .ct


# check by dob, sex
fxn.instudy %>% 
  group_by(dob, sex) %>% 
  arrange (dob) %>% 
  filter(n ( )>1) %>% 
  .p


fxn.instudy %>% 
  .ct




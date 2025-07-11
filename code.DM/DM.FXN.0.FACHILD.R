
fxn.child <- .rt('../DATA/FACOMS FXN/fxn.txt') %>%
  # filter(grepl('C', sjid)) %>%
  # select(status, pm) %>% table
  select    (sjid, status, mature, isoform.e, total) %>% 
  droplevels()

# averaging step - taken out 2025-07-03 -----------------------------------
# fxn.child %>% 
#   group_by(sjid) %>% 
#   filter(n()>1)
#   slice(2)

# fxn.child %>%
#   group_by(sjid) %>% arrange(sjid) %>%
#   # filter(n()>1) %>%
#   group_by(sjid, status) %>%
#   summarize_at(vars(mature, isoform.e, total), ~mean(., na.rm = TRUE))

# total is not always sum of it's parts -----------------------------------

fxn.child %<>% 
  # mutate( total.c = isoform.e + mature)
  # filter( mature  + isoform.e != total)
  select(-total) %>% 
  rename( fxn.m = mature, fxn.e = isoform.e ) %>% 
  gather( type, value, starts_with('fxn.')) %>% 
  select( status, sjid, type, value )

fxn.child %<>% 
  mutate ( tissue = 'blood' ) %>% 
  # left_join( .dd('demo') %>% filter(study == 'FACOMS') ) %>%
  mutate(study = 'FACHILD') %>% 
  # filter(status == 'point mut.') %>% 
  mutate(status =  ifelse( status == 'point mut.', 'patient', status)) %>%
  mutate(status =  ifelse( status == 'homozygous', 'patient', status)) %>%
  mutate ( fxn.unit = 'ng/ml' ) %>%
  rename ( fxn      = value ) %>% 
  select ( study, tissue, sjid, status, fxn.unit, everything())

fxn.child %>% 
  group_by ( tissue, status, type ) %>%
  summarise( n = n(), s = length(unique(sjid)))

fxn.child %<>% .ug %>% 
  left_join( .dd('demo.l') %>% filter( study == 'FACOMS' ) %>% select(-study) ) %>% 
  select ( study, status, sjid, dob, sev.o, sex, aoo, gaa1, gaa2, pm, tissue, type, value = fxn, unit = fxn.unit )

fxn.child %>% 
  # summary
  write_rds('DATA derived/fxn.child.rds')


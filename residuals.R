
# . -----------------------------------------------------------------------

dt. <- readRDS ( '../R.RCR.Modeling/DATA derived/fars.forslope.rds' ) %>% 
  group_by(sjid) %>% 
  filter(study == 'UNIFAI') %>%
  filter( phase.n == 1) %>% 
  filter(sjid %in% unique(fxn.$sjid)) %>%
  filter(paramcd %in% c('FARS.E'))

dt. %>% 
  filter( n()>1 ) %>% 
  # select( -dupline, -phase.n, -amb, -adt ) %>% 
  select( sjid, time., paramcd, bl, aval ) %>% 
  group_by(sjid, paramcd) %>%
  nest()

fxn. %>% 
  filter(!is.na(sev.o)) %>% 
  group_by(analysis.group, sev.o) %>% 
  summarise(
    length(unique(sjid))
  )

# 
# 
# # fxn. %<>% 
# #   filter(status == 'patient')
# # %>% 
# #   left_join(dt.)
# 
# # fxn. %<>%
# #   mutate( data_row_count = map_int(data, ~if(is.data.frame(.x) && nrow(.x) > 0) nrow(.x) else 0) ) %>% 
# #   mutate( ambulatory.slope = ifelse(data_row_count>0, 'Y', 'N' ) )
# 
# # summarise data ----------------------------------------------------------
# 
# # fxn. %>% 
# #   group_by( study, origin, status, type, ambulatory.slope ) %>%
# #   summarise(s = length(unique(sjid))) %>%
# #   spread( ambulatory.slope, s) %>%
# #   mutate( pct = round(100*Y/sum(Y,N),0) ) %>% 
# #   flextable %>%
# #   .st()
# 
# # FACHILD FXN vales, Patients, all their data ---------------------------------
# 
# fxn. %<>% 
#   mutate ( group = paste( study, origin, type )) %>%
#   ungroup %>% 
#   # filter ( ambulatory.slope == 'Y') %>% 
#   select ( -c(study, origin, type) )
# 
# fxn. %<>%
#   # select(group, site, sjid, status, sev.o, fxn, aoo, gaa1, pm, paramcd, data, ) %>% 
#   mutate(gaa100 = gaa1/100) %>% 
#   select(-gaa1)
# 
# fxn. %<>%
#   group_by(group) %>%  # Replace 'group' with the actual name of your first column
#   mutate(
#     fxn_percentile = ntile(fxn, 4),  # Calculate the percentile group (1-4)
#     fxnpct_group = case_when(
#       fxn_percentile == 1 ~ '0-25',
#       fxn_percentile == 2 ~ '25-50',
#       fxn_percentile == 3 ~ '50-75',
#       fxn_percentile == 4 ~ '75-100',
#       TRUE ~ 'NA'  # Just in case there are any NAs or unexpected values
#     )
#   )
# 
# 
# fxn. %>% 
#   summarise(n(), length(unique(sjid)))
# 


# intro -------------------------------------------------------------------

rm(list=ls())

require(labelled)
options(digits = 5)

source('code.DM/DM.FXN.3.add.clinical.data.R')

dt.

fxn.dt. %>% 
  filter( pm != 0) %>% .ug %>% 
  select(sjid, analysis.group) %>% group_by(analysis.group) %>% unique %>% count()

fxn.dt. %>% 
  filter( status != 'patient' )

# clarify the n's ---------------------------------------------------------

bl.tmp <- fxn.dt. %>% 
  filter( pm == 0) %>% 
  filter( paramcd == 'mFARS') %>% 
  filter( type == 'fxn.m') %>% 
  unnest( data ) %>%
  group_by( sjid, type ) %>% 
  mutate( follow.up = ifelse( n()>1, 'FU', 'No-FU' ) ) %>% 
  filter( adt == min(adt) ) %>% .ug

bl.tmp %>% 
  count(analysis.group, follow.up  ) %>%
  group_by(analysis.group) %>%
  mutate(
    total = sum(n),
    pct = round(100 * n / total, 1)
  )

# %>% 
#   .ct
bl.tmp %>% 
  # filter(follow.up == 'FU') %>% 
  count(analysis.group, amb ) %>%
  group_by(analysis.group) %>%
  mutate(
    total = sum(n),
    pct = round(100 * n / total, 1)
  ) %>% .ct

rm(bl.tmp)

# Demo Table --------------------------------------------------------------

pars. <- c('mFARS','FARS.E','FARS.B')

# FU data is the same for analysis group
# take out pms

dt.bl <- fxn.dt. %>% 
  filter( pm == 0) %>% 
  # filter(sjid == 10) %>% 
  # filter( analysis.group == 'dipstick fxn.m') %>% # this doesn't work because not all are in both
  filter( paramcd == 'mFARS') %>% 
  unnest( data ) %>%
  filter( dupline == FALSE ) %>% # anyway none in here anymore
  group_by( sjid ) %>% 
  select( study, sjid, adt, time., amb, status, sev.o, sex, aoo, gaa1, gaa2, pm ) %>% 
  unique( ) %>% 
  mutate( 
    fu_t = as.numeric( max(adt)-min(adt) )/365.25,
    fu_v = n()
  ) %>% 
  # .p
  mutate( NoFU = ifelse( fu_v == 1 , T , F ) ) %>% 
  mutate( fu_t = ifelse( NoFU , NA, fu_t ) ) %>% 
  mutate( fu_v = ifelse( NoFU , NA, fu_v ) ) %>% 
  filter( adt == min(adt) ) %>% 
  unique()

fxn.group.sjid <- fxn.dt. %>% 
  filter( pm == 0) %>% 
  filter( type == 'fxn.m' ) %>% 
  .ug %>% 
  select(
    study, sjid, analysis.group
  ) %>% unique

dt.bl <- fxn.group.sjid %>% 
  left_join(
    dt.bl
    ) %>% 
  left_join( .dd('demo') %>% select(study, sjid, dob) ) %>% 
  mutate( bl_age = as.numeric( adt-dob )/365.25 ) %>% 
  select( study, analysis.group, amb, sjid, NoFU, fu_v, fu_t, bl_age, sex, sev.o, aoo, gaa1, gaa2 )

dt.bl %<>%   
  mutate_at('analysis.group', factor, c('dipstick fxn.m','lcms fxn.m') ) %>%
  group_by(analysis.group, amb)



# re-add Baselines --------------------------------------------------------

dt.bl.fars <- fxn.dt. %>% 
  filter( pm == 0) %>% 
  filter( type == 'fxn.m') %>% 
  group_by( analysis.group, paramcd, sjid ) %>% 
  unnest  ( data ) %>% 
  filter  ( adt == min(adt) ) %>% 
  select  ( analysis.group, study, sjid, paramcd, aval ) %>% 
  spread ( paramcd, aval )

dt.bl %<>% 
  left_join( dt.bl.fars ) %>% 
  # filter(mFARS>19.9) %>% 
  # filter(study == 'UNIFAI') %>%
  droplevels

# demo.table --------------------------------------------------------------

.tab1.sub <- function (df, strata = NA )  {
  tb <- tableone::CreateTableOne(
    vars       = c('sev.o','sex',
                   'bl_age',
                   'aoo', 'gaa1', 'gaa2', 
                   'NoFU','fu_v' , 'fu_t', 
                   'amb','mFARS', 'FARS.E','FARS.B'),
    factorVars = c('sev.o','sex','amb', 'NoFU','analysis.group' ),
    strata     = strata,
    test = T,
    data       = df
  ) %>% 
    print(
      varLabels = T,
      nonnormal = c('bl_age', 'aoo','gaa1','gaa2','fu_v' , 'fu_t'),
      contDigits = 1, catDigits = 1,
      missing = T,
      explain = F, dropEqual= F,
      add.rownames = T) %>%
    data.frame() %>% rownames_to_column() %>%
    flextable() %>% 
    width(width = 7.4/3) %>% 
    fontsize(size = 18, part = "all") %>% 
    height_all(7.10/13, part = "all") 
  return (tb)
}

my.ft <- .tab1.sub( 
  dt.bl %>% 
    mutate( amb = factor(amb, c('non-amb.', 'ambulatory'))) %>% 
    # filter(pm==0) %>% 
    droplevels(), 
  strata = c('analysis.group') 
  )

# Export to Word
doc <- read_docx() %>%
  body_add_flextable(my.ft) %>%
  body_add_par("", style = "Normal")  # optional spacer

# Save
print(doc, target = "table1_summary.docx")



# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# ft %>% print() %>% as.data.frame() %>%  .ct
# 
# ft %>% print(
#   Missing = T
# )
# 
# ft <- .tab1.sub( 
#   dt.bl %>% 
#     filter(pm==0) %>% 
#     filter(
#       analysis.group != 'dipstick fxn.m',
#       amb == 'ambulatory'
#     ) %>% 
#     mutate(pm = ifelse(pm == 0, 0, 1)), 
#   strata = c('sev.o') 
# )
# 
# 
# 
# read_pptx( '../../_templates/CR.template.pptx' ) %>%
#   add_slide   ( layout = 'TTE', master = 'CR') %>%
#   ph_with     ( 
#     ft, 
#     location = ph_location_type( type = "body" , id = 1) ) %>%
#   # add_slide   ( layout = 'TTE', master = 'CR') %>%
#   # ph_with     ( .tab1.sub(dt.bl, strata = 'study'), location = ph_location_type( type = "body" , id = 1) ) %>%
#   # add_slide   ( layout = 'TTE', master = 'CR') %>%
#   # ph_with     ( .tab1.sub(dt.bl, strata = c('study','amb')), location = ph_location_type( type = "body" , id = 1) ) %>%
#   # add_slide   ( layout = 'TTE', master = 'CR') %>%
#   # ph_with     ( .tab1.sub(dt.bl, strata = c('study','med.age')), location = ph_location_type( type = "body" , id = 1) ) %>%
#   # add_slide   ( layout = 'TTE', master = 'CR') %>%
#   # ph_with     ( .tab1.sub(dt.bl, strata = c('study','med.FrE')), location = ph_location_type( type = "body" , id = 1) ) %>%
#   print ( target = paste('Demo.Tables.', gsub(":","-", Sys.time()), ".pptx", sep="") )
# 
# 
# # # flextable ---------------------------------------------------------------
# # std_border = officer::fp_border(style = 'dotted', width = 1)
# # 
# # ft %>%
# #   as.data.frame() %>%
# #   # rownames_to_column('names') %>%
# #   flextable( ) %>%
# #   theme_booktabs(fontsize = 12) %>%
# #   align(align = "left", part = "all") %>%
# #   align(j = 2:4, align = "center", part = "all") %>%
# #   # colformat_num(c('estimate')) %>%
# #   # color ('grey', j = c('term',,'p'), i = ~ p.value >= 0.01) %>%
# #   hline(i = seq(3,nrow(ft),3), border = std_border) %>%
# #   # merge_v(j = c('param')) %>%
# #   autofit()
# 



# 
# .st <- function(l = "TTE", ti = "(st, no title)", tt = ft, template = "../Templates/CR.template.pptx", m = "CR", i = 1){
# 
#   read_pptx( template ) %>%
#     add_slide   ( layout = l, master = m) %>%
#     # ph_with (tt, location = ph_location_type( type = "body" , id = i ) ) %>%
#     ph_with (tt, location = ph_location_type( type = "body" , id = i) ) %>%
#     print ( target = paste(gsub(":","-", Sys.time()), " - ", ti,".pptx", sep="") )
# 
#   rm(tt)
# 
# }
# 
# .st()



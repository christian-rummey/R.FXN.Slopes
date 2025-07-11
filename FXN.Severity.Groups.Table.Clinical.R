
# intro -------------------------------------------------------------------

rm(list=ls())

require(labelled)
options(digits = 5)

source('code.DM/DM.FXN Slope models DM.R')

pars. <- c('mFARS','FARS.E','FARS.B')

fxn.dt. <- fxn.dt %>% 
  filter( map_int(data, nrow) > 1 )

dt.bl <- fxn.dt. %>% 
  unnest(data) %>% 
  filter( type == 'fxn.m' ) %>%
  select( -aval ) %>% 
  mutate( bl = ifelse(time. == 0 , bl, NA)) %>% 
  spread( paramcd, bl ) %>% 
  group_by( sjid, amb, analysis.group ) %>% 
  mutate( 
    fu_p = as.numeric( max(adt)-min(adt) )/365.25,
    fu_v = n()
    ) %>% 
  filter(!is.na(mFARS))
  
dt.bl %<>%   
  left_join( .dd('demo') %>% select(study, sjid, dob) ) %>% 
  mutate( bl_age = as.numeric( adt-dob )/365.25 ) %>%
  select( study, analysis.group, amb, sjid, fu_v, fu_p, bl_age, sex, sev.o, aoo, gaa1, gaa2, pm, fxn = value, mFARS, FARS.E, FARS.B ) %>% 
  mutate_at('analysis.group', factor, c('dipstick fxn.m','lcms fxn.m') ) %>%
  group_by(analysis.group, amb)

# switches for output -----------------------------------------------------

dt.bl %<>% 
  # filter(vc>1) %>% 
  # filter(mFARS>19.9) %>% 
  filter(study == 'UNIFAI') %>%
  droplevels

# demo.table all --------------------------------------------------------------

.tab1.all <- function ( df )  {
  tb <- tableone::CreateTableOne(
    vars       = c('sex','aoo', 'gaa1', 'gaa2', 'pm', 'bl_age', 'fu_v' , 'fu_p', 'amb','analysis.group', 'mFARS', 'FARS.E','FARS.B'),
    factorVars = c('sex','amb', 'pm','analysis.group' ),
    test = F,
    data       = df
  ) %>% 
    print(
      varLabels = T,
      nonnormal = c('bl.age', 'aoo','gaa1','gaa2','period' , 'visits'),
      contDigits = 1, catDigits = 0,
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

.tab1.sub <- function (df, strata = NA )  {
  tb <- tableone::CreateTableOne(
    vars       = c('sex','aoo', 'gaa1', 'gaa2', 'pm', 'bl_age', 'fu_v' , 'fu_p', 'amb','analysis.group', 'mFARS', 'FARS.E','FARS.B'),
    factorVars = c('sex','amb', 'pm','analysis.group' ),
    strata     = strata,
    test = F,
    data       = df
  ) %>% 
    print(
      varLabels = T,
      nonnormal = c('bl.age', 'mx.age','dur','symp','gaa1','fu','fu_v'),
      contDigits = 1, catDigits = 1,
      # missing = T,
      explain = F, dropEqual= F,
      add.rownames = T) %>%
    data.frame() %>% rownames_to_column() %>%
    flextable() %>% 
    width(width = 7.4/3) %>% 
    fontsize(size = 18, part = "all") %>% 
    height_all(7.10/13, part = "all") 
  return (tb)
}

# dt.bl %<>% 
#   left_join(.dd.FA('demo.l') %>% select(sjid, rfstdt)) %>%
#   group_by(study) %>% 
#   mutate(enrol.med = median(rfstdt)) %>% 
#   mutate(study.3 = ifelse(study == 'FACOMS', ifelse(rfstdt < enrol.med, 'FACOMS.e','FACOMS.l'), 'FACHILD'))

.tab1.sub( dt.bl, strata = c('analysis.group', 'amb') )
.tab1.sub( dt.bl %>% filter(group == 'ok'), strata = c('study.3') )
.tab1.sub( 
  dt.bl %>% 
    filter(group == 'ok'), strata = c('study') )
.tab1.sub( 
  dt.bl %>% 
    filter(group == 'ok'), strata = 'group' )



read_pptx( '../Templates/CR.template.pptx' ) %>%
  add_slide   ( layout = 'TTE', master = 'CR') %>%
  ph_with     ( .tab1.all(dt.bl                    ), location = ph_location_type( type = "body" , id = 1) ) %>%
  add_slide   ( layout = 'TTE', master = 'CR') %>%
  ph_with     ( .tab1.sub(dt.bl, strata = 'study'), location = ph_location_type( type = "body" , id = 1) ) %>%
  add_slide   ( layout = 'TTE', master = 'CR') %>%
  ph_with     ( .tab1.sub(dt.bl, strata = c('study','amb')), location = ph_location_type( type = "body" , id = 1) ) %>%
  add_slide   ( layout = 'TTE', master = 'CR') %>%
  ph_with     ( .tab1.sub(dt.bl, strata = c('study','med.age')), location = ph_location_type( type = "body" , id = 1) ) %>%
  add_slide   ( layout = 'TTE', master = 'CR') %>%
  ph_with     ( .tab1.sub(dt.bl, strata = c('study','med.FrE')), location = ph_location_type( type = "body" , id = 1) ) %>%
  print ( target = paste('Demo.Tables.', gsub(":","-", Sys.time()), ".pptx", sep="") )


# # flextable ---------------------------------------------------------------
# std_border = officer::fp_border(style = 'dotted', width = 1)
# 
# ft %>%
#   as.data.frame() %>%
#   # rownames_to_column('names') %>%
#   flextable( ) %>%
#   theme_booktabs(fontsize = 12) %>%
#   align(align = "left", part = "all") %>%
#   align(j = 2:4, align = "center", part = "all") %>%
#   # colformat_num(c('estimate')) %>%
#   # color ('grey', j = c('term',,'p'), i = ~ p.value >= 0.01) %>%
#   hline(i = seq(3,nrow(ft),3), border = std_border) %>%
#   # merge_v(j = c('param')) %>%
#   autofit()
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



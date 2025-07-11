
# read Ian's file and translate track & UF IDs ----------------------------

# dt. <- read_excel('DATA/FXN-M and FXN-E all cases 06-19-25.xlsx') %>% 
#   rename(sjid = FACOMS) %>% 
#   mutate(sjid = gsub ('TRACKFA_','',sjid )) %>% 
#   mutate(sjid = gsub ('TRACK FA_','',sjid ))

# this is the first sheet, which included TRACK-FA samples

dt.1 <- read_excel('DATA/FXN-M and FXN-E all cases 06-19-25 - study added.xlsx') %>% 
  rename(sjid = FACOMS) %>% 
  mutate(sjid = gsub ('TRACKFA_','',sjid )) %>% 
  mutate(sjid = gsub ('TRACK FA_','',sjid ))

# UF IDs to FACOMS --------------------------------------------------------

dt. %<>% 
  left_join(
    read_excel('../DATA/FACHILD/PATNO.UFID.xlsx') %>% 
      rename(sjid = UF)
  ) %>% 
  mutate(sjid = ifelse(is.na(patno), sjid, patno)) %>% 
  select(-patno)

dt. %>% filter(!is.na(...11) | !is.na(...10))

# fix numeric and order ---------------------------------------------------

dt. %<>% 
  mutate( `GAA-1` = ifelse ( `GAA-1` == '>700', 700, `GAA-1`)) %>% 
  mutate( `GAA-1` = ifelse ( `GAA-1` == 'Unk' , NA , `GAA-1`)) %>% 
  
  mutate( `GAA-2` = ifelse ( `GAA-2` == 'G130V',  NA, `GAA-2`)) %>% 
  mutate( `GAA-2` = ifelse ( `GAA-2` == 'g130V',  NA, `GAA-2`)) %>% 
  mutate( `GAA-2` = ifelse ( `GAA-2` == '>700' , 700, `GAA-2`)) %>% 
  mutate( `GAA-2` = ifelse ( `GAA-2` == '10?'  ,  NA, `GAA-2`)) %>% 
  mutate( `GAA-2` = ifelse ( `GAA-2` == 'Unk'  ,  NA, `GAA-2`)) %>% 
  
  mutate( `AOO`   = ifelse (  AOO %in% c('Ukn','?') , NA , AOO))

dt. %<>% 
  mutate(across(c(`GAA-1`, `GAA-2`, AOO), as.numeric)) 

dt. %<>% 
  mutate(
    GAA_short = pmin(`GAA-1`, `GAA-2`, na.rm = TRUE),
    GAA_long  = pmax(`GAA-1`, `GAA-2`, na.rm = TRUE),
    GAA1 = if_else(GAA_short < 100, GAA_long, GAA_short),
    GAA2 = if_else(GAA_short < 100, GAA_short, GAA_long)
  ) %>%
  mutate(`GAA-1` = GAA1, `GAA-2` = GAA2) %>% 
  select(-GAA1, -GAA2, -GAA_short, -GAA_long)

dt. %<>% 
  mutate(sex = ifelse( `M/F` %in% c('f','F','female'), 'f', NA )) %>% 
  mutate(sex = ifelse( `M/F` %in% c('m','M','male'  ), 'm', sex )) %>% 
  mutate(`M/F` = sex) %>% 
  select(-sex)
  # select(`M/F`, sex) %>% .tab

# match -------------------------------------------------------------------

dm. <- .dd('demo.l') %>% 
  select(study, sjid, sex, aoo, gaa1, gaa2, pm, pm.mut) %>% 
  filter(study %in% c('UNIFAI', 'TRACKFA'))

# dt. %>% 
#   filter(row_number() %in% 337:369) %>% .p

dt. %<>%
  left_join(
    dm. %>% 
      select(study, sjid, sex, aoo, gaa1, gaa2, pm),
    by = c("study","sjid" )
  )

dt. %>% 
  group_by(study, sjid) %>% 
  filter(n()>1) %>% .p

# . -----------------------------------------------------------------------

library(openxlsx)

# Step 1: Mark discrepancies
dt. <- dt. %>%
  mutate( gaa1.diff = gaa1 - `GAA-1` ) %>% mutate( gaa1.diff = ifelse( gaa1.diff == 0, NA, gaa1.diff ) ) %>% 
  mutate( gaa2.diff = gaa2 - `GAA-2` ) %>% mutate( gaa2.diff = ifelse( gaa2.diff == 0, NA, gaa2.diff ) ) %>% 
  mutate( sex.diff  = ifelse(`M/F` != sex, 'm/f mismatch', NA ) ) %>% 
  # mutate(marked = ifelse( `M/F` != sex, 1, 0) )
  mutate(marked = ifelse( `GAA-1` != gaa1 | `GAA-2` != gaa2 | `M/F` != sex, 1, 0) )

# Step 2: Find rows to highlight (add 1 for header row)
rows_to_highlight <- which(dt.$marked == 1) + 1

dt. %<>% 
  select(-marked)

# Step 3: Create and populate workbook
wb <- createWorkbook()
addWorksheet(wb, "FACOMS + Non-FACOMS")
writeData(wb, "FACOMS + Non-FACOMS", dt.)

# Step 4: Define and apply highlight style
highlight_style <- createStyle(fgFill = "#FFFF00")
addStyle(
  wb, sheet = "FACOMS + Non-FACOMS",
  style = highlight_style,
  rows = rows_to_highlight,
  cols = 1:ncol(dt.),
  gridExpand = TRUE
)

# Step 5: Save workbook
saveWorkbook(wb, "DATA/FXN-M and FXN-E all cases 06-19-25 - study.xlsx", overwrite = TRUE)  
  

# dt. %>% writexl::write_xlsx('FXN-M and FXN-E all cases 06-19-25-CR.xlsx')

# plot --------------------------------------------------------------------

# dt.tmp <- dt. %>%
#   filter(!GAA2 > 3000) %>% 
#   # filter(gaa1 != GAA1)
#   # filter( sjid == 10) %>% 
#   # gather( allele, value, GAA1, GAA2, gaa1, gaa2 ) %>% 
#   # spread ( allele )
#   mutate( x = gaa1, y = `GAA1` ) %>% 
#   # mutate( x = gaa2, y = `GAA2` ) %>% 
#   droplevels()

# flagged <- c( 67, )

# dt. %>% 
#   filter(`M/F` != sex)
#   filter( gaa1 != GAA1 | GAA2 != gaa2 | `M/F` != sex ) %>%
#   .ct
# 
#     .p
# 
# 
# 
# dt.tmp %>% 
#   ggplot()+geom_point()+
#   aes(x = x, y = y)
# 
# 
# 
# 
# 
# 
# 

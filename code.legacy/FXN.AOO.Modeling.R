

fxn.tmp <- fxn. %>% 
  filter( pm == 0) %>% 
  filter( !is.na(aoo) ) %>% 
  filter( !is.na(gaa1) ) %>% 
  filter( !is.na(gaa2) ) %>% 
  mutate( identical = ifelse ( gaa1 == gaa2, '1', '0' ) ) %>% 
  # mutate(across(c(value), ~ log10(.x))) %>%
  mutate(across(c(gaa1, gaa2), ~ .x / 100)) %>% 
  # filter  ( analysis.group == 'lcms fxn.m') %>% 
  group_by( analysis.group )

full_model_output <- fxn.tmp %>% 
  nest() %>%
  mutate(
    model   = map(data, ~ lm(value ~ aoo + gaa1 + gaa2, data = .x)),
    tidied  = map(model, ~ tidy(.x, conf.int = TRUE)),
    glanced = map(model, glance)
  ) %>%
  select(analysis.group, tidied, glanced) %>%
  unnest(tidied, names_sep = ".") %>%
  unnest(glanced, names_sep = ".") %>%
  select(
    analysis.group,
    term = tidied.term,
    estimate = tidied.estimate,
    std.error = tidied.std.error,
    statistic = tidied.statistic,
    p.value = tidied.p.value,
    conf.low = tidied.conf.low,
    conf.high = tidied.conf.high,
    r.squared = glanced.r.squared,
    adj.r.squared = glanced.adj.r.squared
  ) %>% 
  .fixmod()

# 1. Identify row numbers where each group ends
row_ends <- full_model_output %>%
  group_by(analysis.group) %>%
  summarise(row_id = dplyr::n()) %>%
  mutate(row_id = cumsum(row_id)) %>%
  pull(row_id)

# 2. Create the flextable and apply `hline` on group boundaries
ft <- full_model_output %>%
  flextable() %>%
  colformat_double(
    j = c("estimate", "std.error", "statistic", "conf.low", "conf.high"),
    digits = 3
  ) %>%
  colformat_double(
    j = c("r.squared", "adj.r.squared"),
    digits = 2
  ) %>%
  colformat_double(j = "p.value", digits = 3) %>%
  merge_v(j = c("analysis.group", "r.squared", "adj.r.squared")) %>%
  hline(i = row_ends, border = fp_border(color = "black", width = 1)) %>%
  autofit()                                                         

ft

# -------------------------------------------------------------------------

doc <- read_docx() %>%
  body_add_par("Model Output", style = "heading 1") %>%
  body_add_flextable(
    ft %>%
      set_table_properties(layout = "autofit", width = 1) %>%
      flextable::fontsize(size = 8, part = "all") %>%
      flextable::font(fontname = "Tenorite", part = "all")
  ) %>%
  body_add_par("", style = "Normal")

print(doc, target = "FXN.AOO.Modeling.docx")



 
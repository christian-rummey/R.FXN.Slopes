
# intro -------------------------------------------------------------------

rm(list = ls())
source('project.settings.R')

require(lme4)
require(lmerTest)
require(optimx)
require(broom.mixed)

.lme4.ctrl      <- lmerControl( optimizer = "optimx", calc.derivs = FALSE, optCtrl = list(method = "nlminb" , starttests = FALSE, kkt = FALSE, maxiter = 10000, save.failures = T)) # def iterations 500

source('code.DM/DM.FXN.3.add.clinical.data.R')

fxn.dt. <- fxn.dt. %>%
  group_by(sjid) %>%           # 593 
  filter( pm == 0) %>%         # 562 
  filter( !is.na(aoo) ) %>%    
  filter( !is.na(gaa1) ) %>%   # 539
  filter( !is.na(gaa2) ) %>%   # 538
  mutate( identical = ifelse ( gaa1 == gaa2, '1', '0' ) )

fxn.dt. %<>% 
  mutate( log.value = log10(value) ) %>% 
  # mutate(across( c(value), ~ log10(.x)) ) %>%
  mutate(across( c(gaa1, gaa2), ~ .x / 100))

fxn.dt. %<>% 
  filter  ( !(paramcd == 'FARS.E' & phase == 2) ) %>% 
  filter  ( !(paramcd == 'FARS.B' ) )

fxn.dt. %>% 
  group_by( analysis.group )

table(fxn.dt.$analysis.group)

# exclude low/high baselins ("Decline phase") -----------------------------

names. <- setdiff(names(fxn.dt.), "data")

fxn.dt. %<>%
  unnest(data) %>%
  filter(
    case_when(
      paramcd == "mFARS"  ~ between(bl, 10, 60),
      paramcd == "FARS.E" ~ between(bl, 5, 30),
      TRUE                ~ TRUE
    )
  ) %>%
  nest(data = -all_of(names.))

# finalize nested functional data --------------------------------------------


fxn.dt. %<>% 
  unnest  ( data ) %>% 
  # filter  (!dupline) %>% 
  arrange ( phase, paramcd, analysis.group ) %>% 
  # filter(paramcd == 'FARS.E', phase == 1) %>%
  # filter(analysis.group == 'dipstick fxn.m') %>% 
  group_by( sjid, phase ) %>% 
  mutate  ( time. = age - min(age) ) %>% 
  mutate  ( sev.o = cut( aoo, c( 0, 7, 14, 24, 75, 100), include.lowest = T, labels = c('0-7y', '8-14y', '15-24y', '>24y','control'))) %>%
  select  ( analysis.group, log.value, value, study, sjid, bl.age, sev.o, aoo, gaa1, gaa2, phase, avisitn, time., paramcd, bl, aval  ) %>% 
  
  group_by( analysis.group, paramcd, phase ) %>% 
  # arrange (phase) %>% 
  nest() 


# model -------------------------------------------------------------------

fxn.mod <- fxn.dt. %>% 

  mutate(
    # model = map(data, ~ lmer(aval ~ bl + value*time. + log.value*time. + (1 + time. | sjid), data = ., control = .lme4.ctrl))
    # model = map(data, ~ lmer(aval ~ bl + value:time. + log.value:time. + (1 + time. | sjid), data = ., control = .lme4.ctrl))
    # model = map(data, ~ lmer(aval ~ bl:time. + time. + log.value:time. + (1 + time. | sjid), data = ., control = .lme4.ctrl))
    model = map(data, ~ lmer(aval ~ bl +  time.          + log.value:time. + (1 + time. | sjid), data = ., control = .lme4.ctrl))
    # model = map(data, ~ lmer(aval ~ bl +  time. + sev.o + log.value:time. + (1 + time. | sjid), data = ., control = .lme4.ctrl))
  )

fxn.mod %>% 
  # only best model 
  mutate(
    tidied  = map(model, ~ tidy(.x, conf.int = TRUE))) %>% 
  unnest(tidied) %>%
  filter(effect == 'fixed' & !term %in% c('(Intercept)')) %>% 
  select(analysis.group, paramcd, phase, effect, term, estimate, p.value) %>% 
  .fixmod(d=4) %>% 
  .ct

# comparison with GAA/AOO -------------------------------------------------

fxn.mod <- fxn.dt. %>%
  unnest( data ) 
  
  mutate(
    model.fxn = map(data, ~ lmer(aval ~ bl +  time. + log.value:time. + (1 + time. | sjid), data = ., control = .lme4.ctrl)),
    model.gaa = map(data, ~ lmer(aval ~ bl +  time. + log.value:time. + (1 + time. | sjid), data = ., control = .lme4.ctrl)),
    model.aoo = map(data, ~ lmer(aval ~ bl +  time. + log.value:time. + (1 + time. | sjid), data = ., control = .lme4.ctrl)),
  )




# sev.o -------------------------------------------------------------------

fxn.mod <- fxn.dt. %>% 
  
  mutate(
    # model = map(data, ~ lmer(aval ~ bl + value*time. + log.value*time. + (1 + time. | sjid), data = ., control = .lme4.ctrl))
    # model = map(data, ~ lmer(aval ~ bl + value:time. + log.value:time. + (1 + time. | sjid), data = ., control = .lme4.ctrl))
    # model = map(data, ~ lmer(aval ~ bl + time. + log.value:time. + (1 + time. | sjid), data = ., control = .lme4.ctrl))
    model = map(data, ~ lmer(aval ~ bl + sev.o + time. + log.value:time. + (1 + time. | sjid), data = ., control = .lme4.ctrl))
  )

fxn.mod %>% 
  # only best model 
  mutate(
    tidied  = map(model, ~ tidy(.x, conf.int = TRUE))) %>% 
  unnest(tidied) %>%
  filter(effect == 'fixed' & !term %in% c('(Intercept)','bl')) %>% 
  select(analysis.group, paramcd, phase, effect, term, estimate, p.value) %>% 
  .fixmod(d=4) %>% 
  .ct



# tidy, glance, performance -----------------------------------------------


  

fxn.mod %>%
  mutate(
    tidied  = map(model, ~ tidy(.x, conf.int = TRUE)),
    glanced = map(model, glance),
    rsq     = map(model, ~ performance::r2(.x))
  ) %>%
  mutate(
    rsq_tidy = map(rsq, ~ as_tibble(.x))
  ) %>%
  select(analysis.group, tidied, glanced, rsq_tidy) %>%
  unnest(tidied, names_sep = ".") %>%
  unnest(glanced, names_sep = ".") %>%
  unnest(rsq_tidy, names_sep = ".")

fxn.mod %>%
  filter( !grepl("(Intercept)", tidied.term) ) %>% 
  select(
    paramcd,
    analysis.group,
    term = tidied.term,
    estimate = tidied.estimate,
    p.value = tidied.p.value,
    R2_marginal = rsq_tidy.R2_marginal,
    R2_conditional = rsq_tidy.R2_conditional
  ) %>% 
  .fixmod() %>% .ct
  .p

# predicted progression plot ----------------------------------------------
library(ggeffects)

# Choose the model you want to visualize (e.g., fxn.m from LCMS)
mod <- fxn.dt. %>% 
  filter(analysis.group == "lcms fxn.m") %>%
  nest() %>% 
  mutate(
    model = map(data, ~ lmer(aval ~ bl + value*time. + (1 + time. | sjid), data = ., control = .lme4.ctrl))
  ) %>% 
  pull(model) %>%
  .[[1]]

# Generate predicted values for varying fxn levels across time
preds <- ggpredict(mod, terms = c("time.", "value [quart]"), type = "fixed")

# Plot
ggplot(preds, aes(x = x, y = predicted, color = group)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2, color = NA) +
  labs(
    title = "Predicted FARS.E Progression by fxn Level (LCMS fxn.m)",
    x = "Time",
    y = "Predicted FARS.E",
    color = "fxn Quartile",
    fill = "fxn Quartile"
  ) +
  theme_minimal(base_size = 13)


# threshold analysis ------------------------------------------------------
# Step 1: Define fxn quartile groups (use your actual fxn variable)
fxn.dt.tmp <- fxn.dt. %>%
  filter(analysis.group == "lcms fxn.m") %>%
  mutate(fxn_group = cut(value, breaks = quantile(value, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE),
                         include.lowest = TRUE, labels = c("Q1", "Q2", "Q3", "Q4")))

# Step 2: Fit model using fxn_group as factor
model_thresh <- lmer(aval ~ bl + fxn_group*time. + (1 + time. | sjid),
                     data = fxn.dt.tmp %>% filter(analysis.group == "lcms fxn.m"),
                     control = .lme4.ctrl)

# Step 3: Tidy output
library(broom.mixed)
tidy(model_thresh, conf.int = TRUE) %>% 
  filter(str_detect(term, "fxn_group")) %>%
  select(term, estimate, conf.low, conf.high, p.value)

# 🧠 Interpretation
# There is no significant difference at baseline across fxn quartiles — all groups start roughly the same.
# BUT: Progression slopes differ, especially for higher fxn groups:
#   
# Q4 (highest fxn) has dramatically slower progression than Q1.
# Q3 also shows a meaningful slowdown.
# Q1 (lowest fxn) shows the steepest worsening over time.
# 
# ✅ This supports a threshold effect:
#   Rather than a linear association, the effect of fxn on progression seems nonlinear, with a protective plateau in upper quartile.

library(ggeffects)

preds <- ggpredict(model_thresh, terms = c("time.", "fxn_group"))

ggplot(preds, aes(x = x, y = predicted, color = group)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2, color = NA) +
  labs(
    title = "FARS.E Progression by fxn Quartile (LCMS fxn.m)",
    x = "Time",
    y = "Predicted FARS.E",
    color = "fxn Group",
    fill = "fxn Group"
  ) +
  theme_minimal(base_size = 13)


# option  -----------------------------------------------------------------

# Extract slopes per group
slopes <- preds %>%
  group_by(group) %>%
  summarise(
    change_24mo = predicted[x == max(x)] - predicted[x == min(x)],
    .groups = "drop"
  )

# Bar plot of change per group
ggplot(slopes, aes(x = group, y = change_24mo, fill = group)) +
  geom_col() +
  labs(
    title = "Estimated 24-Month FARS.E Change by fxn Quartile",
    x = "fxn Group (Quartile)",
    y = "Δ FARS.E over 24 months"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")

# optin 2 -----------------------------------------------------------------

ggplot(preds, aes(x = x, y = predicted, color = group)) +
  geom_line(size = 1.4) +
  geom_segment(data = preds %>% filter(x == 0 | x == 24),
               aes(x = min(x), xend = max(x),
                   y = min(predicted), yend = max(predicted),
                   color = group),
               arrow = arrow(length = unit(0.2, "cm")),
               linetype = "dashed", size = 1.1) +
  labs(
    title = "FARS.E Progression Rates by fxn Quartile",
    subtitle = "Dashed lines show Δ over 24 months",
    x = "Time (Months)",
    y = "Predicted FARS.E"
  ) +
  theme_minimal(base_size = 13)


# table output ------------------------------------------------------------



table. %>%
  names()
  filter(tidied.effect == 'fixed') %>% select(-tidied.effect) %>% 
  select(
    analysis.group,
    term      = tidied.term,
    estimate  = tidied.estimate,
    std.error = tidied.std.error,
    statistic = tidied.statistic,
    p.value   = tidied.p.value,
    conf.low  = tidied.conf.low,
    conf.high = tidied.conf.high,
    r.squared     = glanced.r.squared,
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



 
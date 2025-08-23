# intro -------------------------------------------------------------------

rm(list = ls())

library(splines)

source(here("code.DM/DM.FXN.3.add.clinical.data.R"))
rm(fxn.dt., dt.)

fxn. %<>%
  left_join(.dd("demo.l") %>% select(study, sjid, age_bl), by = c("study","sjid")) %>%
  
  mutate(
    identical = gaa1 == gaa2,
    pm. = pm != 0,
	fxn.log   = log10( value )
  ) %>%
  
  mutate(across(c(gaa1, gaa2), ~ . / 100)) %>%
  # mutate(across(c(gaa1, gaa2), ~ ifelse( . > 7, 7, .) ) ) %>%
  # pull(gaa1) %>% log10 %>% hist
  
  filter( !is.na(agroup)) %>% 
  filter( pm == 0) %>% 
  filter( !is.na(sev.o)) %>%
  filter( !is.na(gaa1) ) %>% 
  filter( !is.na(identical)) %>%
  
  mutate(
    agroup = factor(
      agroup,
      c('TQ.E', 'TQ.M', 'LF.M')
      )
    ) %>% 
  filter( agroup != 'TQ.T' ) %>%
  
  select( study, agroup, sjid, sev.o, sex, aoo, age_bl, gaa1, gaa2, type, fxn.log, value )



# --- CONFIG: swap ONLY this line to change the model -------------------------
MODEL_FORMULA <- fxn.log ~ sex + gaa1 + gaa2 + aoo
MODEL_FORMULA <- fxn.log ~ sex + gaa1 + gaa2 + aoo
# Examples:
# MODEL_FORMULA <- fxn.log ~ gaa1 + gaa2 + aoo + identical
# MODEL_FORMULA <- fxn.log ~ sex + gaa1 + gaa2 + aoo + age_bl + identical

# --- helpers -----------------------------------------------------------------
pretty_p <- function(p) ifelse(p < 1e-4, "<0.0001", sprintf("%.4f", p))
label_cohort <- function(x) dplyr::recode(
  as.character(x),
  "LF.M"       = "LF, FXN-M",
  "TQ.E"       = "TQ, FXN-E",
  "TQ.M"       = "TQ, FXN-M",
  .default = as.character(x)
)

map_term <- function(x) dplyr::case_when(
  grepl("^sex", x)      ~ "Sex",
  x == "fxn.log"        ~ "FXN",
  x == "age_bl"         ~ "Age",
  x == "aoo"            ~ "AOO",
  x == "gaa1"           ~ "GAA1",
  x == "gaa2"           ~ "GAA2",
  x == "identicalTRUE"  ~ "Identical",
  TRUE                  ~ NA_character_
)

# --- models per cohort -------------------------------------------------------

fxn.models <- fxn. %>%
  group_by(agroup) %>%
  nest() %>%
  mutate(
    model  = map(data, ~ lm(MODEL_FORMULA, data = .x)),
    tidy   = map(model, tidy),
    glance = map(model, glance)
  )

# cohort order (robust if agroup isn't an ordered factor)
ag_levels <- levels(fxn.models$agroup)
if (is.null(ag_levels)) ag_levels <- unique(as.character(fxn.models$agroup))

# --- coefficients (auto-detect what's present) -------------------------------
coefs <- fxn.models %>%
  select(agroup, tidy) %>%
  unnest(tidy) %>%
  filter(term != "(Intercept)") %>%
  mutate(term = map_term(term)) %>%
  filter(!is.na(term)) %>%                    # keep only mapped terms
  transmute(
    agroup, term,
    est  = sprintf("%.3f ± %.3f", estimate, std.error),
    pval = pretty_p(p.value)
  )

# --- model fit row (R² and model p) ------------------------------------------
mods <- fxn.models %>%
  select(agroup, glance) %>%
  unnest(glance) %>%
  transmute(
    agroup, term = "Model",
    est  = sprintf("%.2f (R²)", r.squared),
    pval = pretty_p(p.value)
  )

# --- combine, reshape, order rows & columns ----------------------------------
row_order <- c("Model","Sex","Age","AOO","GAA1","GAA2","Identical")
tab <- bind_rows(mods, coefs) %>%
  mutate(term = factor(term, levels = row_order)) %>%
  arrange(term) %>%
  pivot_wider(
    names_from  = agroup,
    values_from = c(est, pval),
    names_sep   = "___"
  )

# reorder columns: for each cohort -> est then pval (and drop missing safely)
desired_cols <- as.vector(rbind(paste0("est___", ag_levels),
                                paste0("pval___", ag_levels)))
tab <- tab %>%
  select(term, any_of(desired_cols))

# --- header mapping (Cohort on top; Estimate/p-value below) ------------------
cohort_names <- label_cohort(ag_levels)
present <- intersect(desired_cols, names(tab))
top    <- c("Cohort", rep(label_cohort(sub("^(est|pval)___", "", present))[seq(1, length(present), 2)], each = 2))
bottom <- c("Variable", rep(c("Estimate ± SE","p-value"), times = length(top) %/% 2))

hdr <- data.frame(
  col_keys = names(tab),
  top      = top,
  bottom   = bottom,
  check.names = FALSE
)

# --- flextable ---------------------------------------------------------------
ft <- flextable(tab) %>%
  set_header_df(mapping = hdr, key = "col_keys") %>%
  merge_h(part = "header") %>%
  set_header_labels(term = "Variable") %>%
  theme_booktabs() %>%   # keep the clean default borders
  autofit() %>%
  set_caption("Frataxin levels: Regression with GAA, Age, Sex, Identical")

# --- export ---------------------------------------------------------------
doc <- officer::read_docx() %>%
  body_add_flextable(ft)

print(
  doc,
  target = sprintf("FXN Model 1 (FXN)_%s.docx",
                   format(Sys.time(), "%Y-%m-%d_%H-%M"))
)
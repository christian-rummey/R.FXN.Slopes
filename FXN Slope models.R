
# intro -------------------------------------------------------------------

rm(list=ls())

source('code.DM/DM.FXN Slope models DM.R')

fxn.dt. <- fxn.dt %>% 
  filter( pm == 0 ) %>% 
  filter( map_int(data, nrow) > 1 )

# 2025-07-11 - I don't get this anymore. pm's will be excluded. 
# since average avals are similar over the groups, I can argue that pm is not necessary as 
# fixed factor. If used as such, it drains effects from interaction term.  
# including time. or not changes the way results are presented (p.value for change or p-value for interaction)
# adjusting by gaa makes the estimates slightly more realistic (although p.value is not significant)

# Influence of BL - should get smaller with lower BL? ---------------------

models. <- fxn.dt. %>% 
  unnest( data ) %>% 
  # filter( dupline == T)
  filter( paramcd == 'FARS.E' ) %>% 
  filter( amb == 'ambulatory') %>% 
  group_by( analysis.group, type, paramcd, amb ) %>% 
  nest() %>%
  mutate( mod  = map( data, ~ lmer(aval ~ bl + value + time. + (1 + time.|sjid) , data = . , control = .lme4.ctrl ) ) )

models. <- fxn.dt. %>% 
  unnest( data ) %>% 
  mutate( bl.grp = cut(bl, c(0,10,20,30,40))) %>% 
  # filter( dupline == T)
  filter( paramcd == 'FARS.E' ) %>% 
  filter( amb == 'ambulatory') %>% 
  group_by( analysis.group, type, paramcd, amb, bl.grp ) %>% 
  nest() %>%
  mutate( mod  = map( data, ~ lmer(aval ~ bl + value + time. + (1 + time.|sjid) , data = . , control = .lme4.ctrl ) ) )


models. %>% 
  mutate( coef = map( mod, ~ broom::tidy(.x, effects = "fixed", conf.int = T) ) ) %>% 
  unnest( coef ) %>% 
  select( -data, -mod ) %>% 
  filter( !term %in% c('(Intercept)')) %>% 
  arrange(analysis.group, bl.grp) %>% 
  filter(term == 'time.') %>% 
  .fixmod() %>% .p






# preview model ------------------------------------------------------------

table(fxn.dt$analysis.group)

fxn.dt %>% 
  grou
  lmer(aval ~ bl +  time. + (1 + time.|sjid) , data = . , control = .lme4.ctrl ) %>%
  # lmer(aval ~ bl +       fxn:time. + time. + (1 + time.|sjid) , data = . , control = .lme4.ctrl ) %>% 
  # lmer(aval ~ bl + fxn + fxn:time. + time. + (1 + time.|sjid) , data = . , control = .lme4.ctrl ) %>% 
  broom::tidy(effects = "fixed", conf.int = T) %>% 
  .fixmod()

# nested models -----------------------------------------------------------

# Define the model formulas with their corresponding names

model_formulas <- list(
  "fxn+time"            = aval ~ bl + fxn    + time. + (1 + time. | sjid),
  "aoo+time"            = aval ~ bl + aoo    + time. + (1 + time. | sjid),
  "gaa100+time"         = aval ~ bl + gaa100 + time. + (1 + time. | sjid),
  "fxn+aoo+gaa100+time" = aval ~ bl + fxn + aoo + gaa100 + time. + (1 + time. | sjid),
  "fxn:time"            = aval ~ bl +          fxn:time.    +         (1 + time. | sjid),
  "fxn+fxn:time"        = aval ~ bl + fxn +    fxn:time.    +         (1 + time. | sjid),
  "fxn:time+time."      = aval ~ bl +          fxn:time.    + time. + (1 + time. | sjid),
  "fxn*time"            = aval ~ bl +          fxn*time.    +         (1 + time. | sjid),
  "aoo:time"            = aval ~ bl +          aoo:time.    +         (1 + time. | sjid),
  "aoo+aoo:time"        = aval ~ bl + aoo +    aoo:time.    +         (1 + time. | sjid),
  "aoo:time+time"       = aval ~ bl +          aoo:time.    + time. + (1 + time. | sjid),
  "aoo*time"            = aval ~ bl +          aoo*time.    +         (1 + time. | sjid),
  "gaa100:time"         = aval ~ bl +          gaa100:time. +         (1 + time. | sjid),
  "gaa100:time+time"    = aval ~ bl +          gaa100:time. + time. + (1 + time. | sjid),
  "gaa100+gaa100:time"  = aval ~ bl + gaa100 + gaa100:time. +         (1 + time. | sjid),
  "gaa100*time"         = aval ~ bl +          gaa100*time. +         (1 + time. | sjid)
)

# Filter and nest the data once, then apply all models within the same pipeline
# Process the data
models <- fxn.dt. %>%
  # filter(group == "FACOMS blood mature", pm == 0) %>%
  # filter(group == "FACHILD blood mature", pm == 0) %>%
  group_by(group, paramcd) %>%
  nest() %>%
  # Apply each model formula and name to the nested data
  mutate(
    results = map(data, ~ {
      .data <- .x  # Assign data to .data to avoid conflicts
      map2_dfr(model_formulas, names(model_formulas), ~ {
        form <- .x  # Assign formula to form
        model <- lmer(form, data = .data, control = .lme4.ctrl)
        tidy(model, effects = "fixed", conf.int = TRUE) %>%
          mutate(model_name = .y)  # Add model name to each result
      })
    })
  )

model_fits <- models %>%
  mutate(
    # Calculate AIC and BIC for each formula and dataset
    fit_stats = map(data, ~ {
      .data <- .x  # Assign dataset to .data
      map2_dfr(model_formulas, names(model_formulas), ~ {
        form <- .x
        model <- lmer(form, data = .data, control = .lme4.ctrl)
        tibble(
          model_name = .y,
          AIC = AIC(model),
          BIC = BIC(model)
        )
      })
    })
  ) %>%
  # Unnest the fit statistics
  unnest(fit_stats) %>%
  select(group, paramcd, model_name, AIC, BIC) %>% arrange(AIC)


coefs <- models %>%
  # Unnest results to get a flat table of models
  unnest(results) %>%
  # Select relevant columns
  mutate ( model_name = factor(model_name, levels = names(model_formulas))) %>%
  arrange( model_name) %>% 
  select (group, paramcd, model_name, term, estimate, std.error, conf.low, conf.high, p.value)

coefs  %>%
  # Filter out the intercept for clarity and factor terms for consistent ordering
  filter(term != "(Intercept)") %>%
  filter(term != "bl") %>%
  mutate(
    term = factor(term, levels = c('bl', 'time.', 'aoo', 'aoo:time.', 'gaa100', 'gaa100:time.', 'fxn', 'fxn:time.'))
  ) %>%
  # Create the plot
  print(n=Inf) %>% #filter(term=='fxn')
  
  ggplot( aes(x = term, y = estimate )) +
  # aes(color = model_name)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, position = position_dodge(width = 0.5)) +
  
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  geom_text(aes(label = sprintf("%.2f", estimate)), size = 3, vjust = -1, hjust = 0.5, position = position_dodge(width = 0.5)) +
  geom_text(aes(label = sprintf("%.4f", p.value )), size = 3, vjust =  1, hjust = 0.5, position = position_dodge(width = 0.5)) +
  
  facet_wrap(~ paste(group, paramcd, sep = " - "), scales = "free_x") +
  labs(x = "Term", y = "Estimate", title = "Model Coefficients with Confidence Intervals") +
  facet_wrap(~model_name)+
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )+.box


# table for poster --------------------------------------------------------

coefs  %>%
  filter(grepl('blood mature', group)) %>% 
  # Filter out the intercept for clarity and factor terms for consistent ordering
  filter(term != "(Intercept)") %>%
  filter(term != "bl") %>%
  mutate(
    term = factor(term, levels = c('bl', 'time.', 'aoo', 'aoo:time.', 'gaa100', 'gaa100:time.', 'fxn', 'fxn:time.'))
  ) %>% 
  filter(model_name == 'fxn*time') %>% 
  .fixmod() %>% 
  select(group, term, estimate, conf.low, conf.high, p.value) %>% 
  .ct

# fit statistics ----------------------------------------------------------

coefs %>%
  filter(term == "time.") %>%
  select(model_name, estimate, conf.low, conf.high, p.value) %>%
  arrange(desc(abs(estimate)))  # Sort by size of `time.` estimate

models %>%
  unnest(results) %>%
  mutate(
    AIC = map_dbl(fit, AIC),
    BIC = map_dbl(fit, BIC)
  ) %>%
  arrange(AIC)

unnest(results) %>%
  mutate(
    AIC = map_dbl(data, ~ AIC(lmer(.x$form, data = .x, control = .lme4.ctrl))),
    BIC = map_dbl(data, ~ BIC(lmer(.x$form, data = .x, control = .lme4.ctrl)))
  ) %>%
  arrange(AIC)


# plot that ---------------------------------------------------------------

plot_data <- model_fits %>%
  mutate(
    factor = case_when(
      str_detect(model_name, "gaa100") ~ "gaa100",
      str_detect(model_name, "fxn") ~ "fxn",
      str_detect(model_name, "aoo") ~ "aoo",
      TRUE ~ "Other"
    )
  )

# Create the plot
ggplot(plot_data, aes(x = reorder(model_name, AIC), y = AIC, fill = factor)) +
  geom_bar(stat = "identity", width = 0.7) +
  coord_flip() +
  scale_fill_manual(values = c("gaa100" = "dodgerblue", "fxn" = "forestgreen", "aoo" = "darkorange", "Other" = "grey70")) +
  labs(
    title = "Model Comparison by AIC for Predicting Progression",
    x = "Model",
    y = "AIC",
    fill = "Factor Involved"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.y = element_text(size = 10),
    plot.title = element_text(hjust = 0.5)
  )


# check by severity group -------------------------------------------------

require(emmeans)

# Fit the model including `sev.o` interaction terms

best_model_sev <- fxn.dt. %>%
  filter(group == "FACOMS blood mature", pm == 0) %>%
  lmer(aval ~ bl + fxn * time. * sev.o + (1 + time. | sjid), data = .)

tidy(best_model_sev, effects = "fixed", conf.int = TRUE) %>%
  filter(str_detect(term, "time."))  # Select only terms involving time

emmeans::emtrends( best_model_sev,
  ~ sev.o,
  var = "time."
)


# Define the variables to analyze
factors <- c("aoo", "gaa100", "fxn")

# Nested procedure to calculate trends with emtrends
trend_results <- factors %>%
  map_df(~ {
    factor_name <- .x
    
    # Fit the model for each factor
    model <- fxn.dt. %>%
      filter(group == "FACOMS blood mature", pm == 0) %>%
      lmer(as.formula(paste("aval ~ bl +", factor_name, "* time. * sev.o + (1 + time. | sjid)")), data = .)
    
    # Extract trends by `sev.o` using emtrends
    emtrends_results <- emtrends(model, ~ sev.o, var = "time.") %>%
      as.data.frame() %>%
      mutate(factor = factor_name)  # Add the factor name for reference
    
    # Return the results for each factor
    emtrends_results
  })

# Display the full results in a single data frame
print(trend_results)

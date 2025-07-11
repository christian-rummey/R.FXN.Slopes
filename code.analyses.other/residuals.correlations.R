
dt. <- fxn. %>%
  # group_by(sjid,group) %>% filter(n()>1) %>% arrange(sjid)
  filter(status == "patient") %>% ungroup

dt. %>% 
  filter(group == 'FACOMS blood mature') %>% 
  select(group, fxn, aoo, gaa100)

# Compute the Spearman correlation for the full dataset
full_correlation <- dt. %>%
  filter(group == 'FACOMS blood mature') %>%
  select(aoo, gaa100) %>%
  cor(method = "spearman", use = "pairwise.complete.obs")

# Filter out cases where gaa100 > 7 and compute Spearman correlation
filtered_correlation <- dt. %>%
  filter(group == 'FACOMS blood mature', gaa100 <= 7) %>%
  select(aoo, gaa100) %>%
  cor(method = "spearman", use = "pairwise.complete.obs")

# Print the results
full_correlation
filtered_correlation


# Create a modified gaa100 where values greater than 7 are set to 7
dt_modified <- dt. %>%
  filter(group == 'FACOMS blood mature') %>%
  mutate(gaa100_modified = ifelse(gaa100 > 7, 7, gaa100))

# Compute the Spearman correlation with the modified gaa100
correlation_modified <- dt_modified %>%
  select(aoo, gaa100_modified) %>%
  cor(method = "spearman", use = "pairwise.complete.obs")

# Print the result
correlation_modified



# Create a new variable that captures values of gaa100 greater than 7
dt_broken_stick <- dt. %>%
  filter(group == 'FACOMS blood mature') %>%
  mutate(gaa100_above_7 = pmax(gaa100 - 7, 0))  # Values above 7, else 0

# Fit a linear model (broken stick model)
model_broken_stick <- lm(aoo ~ gaa100 + gaa100_above_7, data = dt_broken_stick)

# Summary of the model
summary(model_broken_stick)


# Plot the data
plot(dt_broken_stick$gaa100, dt_broken_stick$aoo, main = "Broken Stick Model",
     xlab = "gaa100", ylab = "aoo", pch = 19)

# Add the fitted line from the broken stick model
abline(model_broken_stick, col = "red")

# scan for optimal --------------------------------------------------------

# Function to fit a broken stick model for a given breakpoint
fit_broken_stick <- function(data, breakpoint) {
  data <- data %>%
    mutate(gaa100_above_bp = pmax(gaa100 - breakpoint, 0))  # Create new variable for values above breakpoint
  
  # Fit the linear model
  model <- lm(aoo ~ gaa100 + gaa100_above_bp, data = data)
  
  # Return model's RSS (Residual Sum of Squares)
  return(sum(resid(model)^2))  # Or use AIC(model) if you prefer
  # return(AIC(model))
}

# Filter the relevant group
dt_filtered <- dt. %>%
  filter(group == 'FACOMS blood mature') %>% 
  filter(!is.na(gaa100))

# Range of possible breakpoints (can adjust as needed)
breakpoints <- seq(min(dt_filtered$gaa100), max(dt_filtered$gaa100), by = 0.1)

# Scan over breakpoints and compute RSS for each
rss_values <- sapply(breakpoints, function(bp) fit_broken_stick(dt_filtered, bp))

# Find the breakpoint that minimizes RSS
optimal_breakpoint <- breakpoints[which.min(rss_values)]

# Print the optimal breakpoint
optimal_breakpoint

# Fit the final broken stick model with the optimal breakpoint
dt_filtered <- dt_filtered %>%
  mutate(gaa100_above_optimal = pmax(gaa100 - optimal_breakpoint, 0))

model_optimal <- lm(aoo ~ gaa100 + gaa100_above_optimal, data = dt_filtered)

# Summary of the optimal model
summary(model_optimal)


# Plot RSS values against breakpoints
plot(breakpoints, rss_values, type = "l", xlab = "Breakpoint", ylab = "RSS", main = "RSS vs Breakpoint")
abline(v = optimal_breakpoint, col = "red", lwd = 2, lty = 2)  # Mark the optimal breakpoint


# plot --------------------------------------------------------------------

library(ggplot2)

# Assume 'optimal_breakpoint' has already been calculated using the previous method

# Filter and create variables for below and above the optimal breakpoint
dt_filtered <- dt. %>%
  filter(group == 'FACOMS blood mature') %>%
  mutate(below_threshold = gaa100 <= optimal_breakpoint,
         gaa100_above_threshold = pmax(gaa100 - optimal_breakpoint, 0))

# Plot the data with different colors for below and above threshold
ggplot(dt_filtered, aes(x = gaa100, y = aoo)) +
  geom_point(aes(color = below_threshold), size = 2) +  # Points colored by threshold
  geom_smooth(data = subset(dt_filtered, below_threshold == TRUE), method = "lm", se = FALSE, color = "blue") +  # Regression line below threshold
  geom_smooth(data = subset(dt_filtered, below_threshold == FALSE), method = "lm", se = FALSE, color = "red") +  # Regression line above threshold
  labs(title = "Correlation between GAA100 and AOO",
       x = "GAA100",
       y = "AOO",
       color = "Below Threshold") +
  theme_minimal() +
  geom_vline(xintercept = optimal_breakpoint, linetype = "dashed", color = "black", size = 1)  # Add vertical line for threshold
















dt. <- fxn. %>%
  # group_by(sjid,group) %>% filter(n()>1) %>% arrange(sjid)
  filter(status == "patient") %>% ungroup

# . -----------------------------------------------------------------------

# Function to calculate the optimal breakpoint for a given group
find_optimal_breakpoint <- function(data) {
  breakpoints <- seq(min(data$gaa100, na.rm = TRUE), max(data$gaa100, na.rm = TRUE), by = 0.1)
  
  # Function to fit broken stick model and return RSS for a given breakpoint
  fit_broken_stick <- function(data, breakpoint) {
    data <- data %>%
      mutate(gaa100_above_bp = pmax(gaa100 - breakpoint, 0))
    model <- lm(aoo ~ gaa100 + gaa100_above_bp, data = data)
    return(sum(resid(model)^2))  # Returning Residual Sum of Squares (RSS)
  }
  
  # Scan over breakpoints and find the one with the lowest RSS
  rss_values <- sapply(breakpoints, function(bp) fit_broken_stick(data, bp))
  optimal_breakpoint <- breakpoints[which.min(rss_values)]
  
  return(optimal_breakpoint)
}

# Loop through each group, calculate optimal breakpoint, and store in a table
groups <- levels(factor(dt.$group))

output_table <- data.frame(Group = character(), Optimal_Breakpoint = numeric())

for (group in groups) {
  data_group <- dt. %>%
    filter(group == !!group, !is.na(gaa100))  # Filter the group and remove empty gaa100 values
  
  optimal_breakpoint <- find_optimal_breakpoint(data_group)
  
  # Store the result in the output table
  output_table <- rbind(output_table, data.frame(Group = group, Optimal_Breakpoint = optimal_breakpoint))
}

# Print the output table with optimal breakpoints
print(output_table)

# Modify the dataset to include the information about below/above threshold

dt_modified <- dt. %>%
  filter(!is.na(gaa100)) %>%
  left_join(output_table, by = c("group" = "Group")) %>%
  # mutate(Optimal_Breakpoint = 7) %>% 
  mutate(gaa100_below_threshold = gaa100 <= Optimal_Breakpoint)

# Calculate correlation coefficients for below, above, and overall for each group
correlation_data <- dt_modified %>%
  group_by(group) %>%
  summarise(
    below_corr = cor(gaa100[gaa100_below_threshold], aoo[gaa100_below_threshold], method = "spearman", use = "pairwise.complete.obs"),
    above_corr = cor(gaa100[!gaa100_below_threshold], aoo[!gaa100_below_threshold], method = "spearman", use = "pairwise.complete.obs"),
    overall_corr = cor(gaa100, aoo, method = "spearman", use = "pairwise.complete.obs")  # Overall correlation
  )

# Calculate rho^2 for all correlation columns using mutate_at
correlation_data <- correlation_data %>%
  mutate_at(vars(below_corr, above_corr, overall_corr), ~ .^2)  

# Merge correlation data back into the dataset for plotting
dt_modified <- dt_modified %>%
  left_join(correlation_data, by = "group")

# Faceted plot with correlation coefficients added
ggplot(dt_modified, aes(x = gaa100, y = aoo)) +
  geom_point() +
  facet_wrap(~ group, scales = "free") +  # Facet by group
  geom_smooth(data = subset(dt_modified, gaa100_below_threshold == TRUE), method = "lm", se = FALSE, color = "blue") +  # Regression line below threshold
  geom_smooth(data = subset(dt_modified, gaa100_below_threshold == FALSE), method = "lm", se = FALSE, color = "red") +  # Regression line above threshold
  geom_vline(data = output_table, aes(xintercept = Optimal_Breakpoint), linetype = "dashed", color = "black") +  # Add optimal breakpoints
  labs(title = "Broken Stick Models with Correlations", x = "GAA100", y = "AOO") +
  theme_minimal() +
  # Annotate correlation coefficients below and above the threshold
  geom_text(data = correlation_data, aes(x = Inf, y = Inf, label = paste("rho² below = ", round(below_corr, 2))), hjust = 1.1, vjust = 3, color = "blue", size = 3) +
  geom_text(data = correlation_data, aes(x = Inf, y = Inf, label = paste("rho² above = ", round(above_corr, 2))), hjust = 1.1, vjust = 5, color = "red", size = 3) +
  geom_text(data = correlation_data, aes(x = Inf, y = Inf, label = paste("rho² overall = ", round(overall_corr, 2))), hjust = 1.1, vjust = 7, color = "black", size = 3)

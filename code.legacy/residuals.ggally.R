# Load necessary libraries
library(GGally)

# Load the dataset
dataset <- fxn. %>% ungroup

# Filter to include only patients
dataset_patients <- dataset %>% 
  filter(status == "patient") %>% ungroup

# Function to calculate R2 values
calculate_r2_matrix <- function(data) {
  cor_matrix <- data %>%
    select(fxn, aoo, gaa100) %>%
    as.data.frame() %>%
    cor(use = "pairwise.complete.obs")#, method = "spearman")
  cor_matrix^2
}

# Function to plot R2 heatmap
plot_r2_matrix <- function(r2_matrix, title) {
  r2_df <- as.data.frame(as.table(r2_matrix))
  ggplot(data = r2_df, aes(x = Var1, y = Var2, fill = Freq)) +
    geom_tile() +
    geom_text(aes(label = round(Freq, 2)), color = "white") +
    theme_minimal() +
    labs(title = title, fill = "R2") +
    theme(plot.title = element_text(hjust = 0.5))
}

# Function to plot pair plot
plot_pair_plot <- function(data, title) {
  ggpairs(data %>% select(fxn, aoo, gaa100)) + ggtitle(title)
}

# List of groups to analyze
groups <- c("FACHILD blood isoform.e", "FACHILD blood mature", "FACOMS blood mature", "FACOMS buccal mature")

# Loop over each group and perform analysis
for (group in groups) {
  group_data <- dataset_patients %>% filter(group == !!group)
  
  # Calculate R2 matrix
  r2_matrix <- calculate_r2_matrix(group_data)
  
  # Plot R2 heatmap
  print(plot_r2_matrix(r2_matrix, paste("R2 Matrix -", group)))
  
  # Plot pair plot
  print(plot_pair_plot(group_data, paste("Pair Plot -", group)))
}

# Function to summarize R2 values
summarize_r2 <- function(r2_matrix) {
  tibble(
    AOO_GAA100_R2 = r2_matrix["aoo", "gaa100"],
    AOO_Fxn_R2 = r2_matrix["aoo", "fxn"],
    GAA100_Fxn_R2 = r2_matrix["gaa100", "fxn"]
  )
}

# Create summary table
summary_table <- bind_rows(
  lapply(groups, function(group) {
    group_data <- dataset_patients %>% filter(group == !!group)
    r2_matrix <- calculate_r2_matrix(group_data)
    summarize_r2(r2_matrix) %>% mutate(Dataset = group)
  })
)

# Print summary table
print(summary_table)


# normality ---------------------------------------------------------------
hist(dataset_patients$gaa100)
# hist(log(dataset_patients$gaa100))
shapiro.test(dataset_patients$gaa100)

hist            (dataset_patients$aoo)
shapiro.test(    dataset_patients$aoo)
hist        (log(dataset_patients$aoo))
shapiro.test(log(dataset_patients$aoo))

hist        (dataset_patients$fxn)
shapiro.test(dataset_patients$fxn)

ks.test(dataset_patients$gaa100, "pnorm", mean = mean(dataset_patients$fxn, na.rm = TRUE), sd = sd(dataset_patients$fxn, na.rm = TRUE))

library(tseries)
jarque.bera.test(dataset_patients$fxn)

# Q-Q Plot
ggplot(dataset_patients, aes(sample = fxn)) + 
  stat_qq() + 
  stat_qq_line() + 
  theme_minimal() + 
  labs(title = "Q-Q Plot of fxn", x = "Theoretical Quantiles", y = "Sample Quantiles")


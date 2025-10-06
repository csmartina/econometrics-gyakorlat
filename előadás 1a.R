
# Load necessary library
library(ggplot2)

# Set seed for reproducibility
set.seed(123)

# Generate a non-normal population (exponential distribution)
population <- rexp(100000, rate = 0.1)  # mean = 10, skewed distribution

# Sample sizes to test
sample_sizes <- c(5, 30, 100, 500)

# Number of samples to draw for each sample size
num_samples <- 1000

# Initialize data frame to store sample means
sampling_data <- data.frame()

# Loop through each sample size
for (n in sample_sizes) {
  sample_means <- replicate(num_samples, mean(sample(population, n)))
  temp_df <- data.frame(sample_mean = sample_means, sample_size = as.factor(n))
  sampling_data <- rbind(sampling_data, temp_df)
}

# Plot sampling distributions
ggplot(sampling_data, aes(x = sample_mean)) +
  geom_histogram(bins = 50, fill = "skyblue", color = "black") +
  geom_vline(xintercept = mean(population), color = "red", linetype = "dashed") +
  facet_wrap(~sample_size, scales = "free") +
  labs(title = "CLT & LLN with Non-Normal Population (Exponential)",
       subtitle = "Sampling distributions of the mean for different sample sizes",
       x = "Sample Mean",
       y = "Frequency") +
  theme_minimal()

# Setting up the dataframe
student_no <- c(1:17)
no_visual_aids_scores <- c(50,60,58,72,36,51,49,49,25,52,41,32,58,39,25,40,61)
with_visual_aids_scores <- c(58,70,60,73,40,63,54,60,29,57,66,37,50,48,80,65,70)

# Creating the data frame
Lecture_Quality  <- data.frame(
  Student_id =  student_no,
  No_Visual_Aids = no_visual_aids_scores,
  With_Visual_Aids = with_visual_aids_scores
)
View(Lecture_Quality)
str(Lecture_Quality)
summary(Lecture_Quality)


library(reshape2)
library(ggplot2)

# Melting the data for use in ggplot
Lecture_Quality_long <- reshape2::melt(Lecture_Quality, id.vars = "Student_id")
View(Lecture_Quality_long)


# Creating the boxplot
windows(20,10 )
ggplot(Lecture_Quality_long, aes(x = variable, y = value, fill = variable)) +
  geom_boxplot() +
  labs(title = "Comparison of lecture quality With and Without Visual Aids",
       x = "Condition",
       y = "Scores",
       fill = "Test Condition") +
  theme_minimal()


windows(20,10)
# Plot histogram for no visual aids
ggplot(Lecture_Quality, aes(x = No_Visual_Aids)) + 
  geom_histogram(binwidth = 1, fill = "blue", color = "black") + 
  ggtitle("Histogram of No visual aids") +
  xlab("no_visual_aids") + 
  ylab("Frequency")

windows(20,10)
# Plot histogram for with visual aids
ggplot(Lecture_Quality, aes(x = With_Visual_Aids)) + 
  geom_histogram(binwidth = 0.25, fill = "red", color = "black") + 
  ggtitle("Histogram of with_visual_aids") +
  xlab("visual aids") + 
  ylab("Frequency")

# Q-Q plot for the 'no visual aids' variable
attach(Lecture_Quality)

windows(20,10)
qqnorm(Lecture_Quality$No_Visual_Aids, main = "Q-Q Plot for no visual aids)")
qqline(Lecture_Quality$No_Visual_Aids, col = "red")  # Adding a reference line

# Q-Q plot for the 'with visual aids' variable
windows(20,10)
qqnorm(Lecture_Quality$With_Visual_Aids, main = "Q-Q Plot for with visual aids")
qqline(Lecture_Quality$With_Visual_Aids, col = "red")  # Adding a reference line


# Sample Sizes
n_no_aids <- length(Students_scores$No_Visual_Aids)
n_with_aids <- length(Students_scores$With_Visual_Aids)

# Calculate Confidence Intervals
t_critical_no_aids <- qt(0.975, df=n_no_aids-1)
ci_no_aids <- c(mean_no_aids - t_critical_no_aids * sd_no_aids/sqrt(n_no_aids), mean_no_aids + t_critical_no_aids * sd_no_aids/sqrt(n_no_aids))

t_critical_with_aids <- qt(0.975, df=n_with_aids-1)
ci_with_aids <- c(mean_with_aids - t_critical_with_aids * sd_with_aids/sqrt(n_with_aids), mean_with_aids + t_critical_with_aids * sd_with_aids/sqrt(n_with_aids))


# Load necessary library

library(moments)

# Sample Sizes
n_no_aids <- length(Students_scores$No_Visual_Aids)
n_with_aids <- length(Students_scores$With_Visual_Aids)

# Calculate Confidence Intervals
t_critical_no_aids <- qt(0.975, df=n_no_aids-1)
ci_no_aids <- c(mean_no_aids - t_critical_no_aids * sd_no_aids/sqrt(n_no_aids), mean_no_aids + t_critical_no_aids * sd_no_aids/sqrt(n_no_aids))

t_critical_with_aids <- qt(0.975, df=n_with_aids-1)
ci_with_aids <- c(mean_with_aids - t_critical_with_aids * sd_with_aids/sqrt(n_with_aids), mean_with_aids + t_critical_with_aids * sd_with_aids/sqrt(n_with_aids))


# Print Results
cat("No Visual Aids - 95% CI:", ci_no_aids, "\n")
cat("With Visual Aids - 95% CI:", ci_with_aids, "\n")

# Calculating Descriptive Statistics
mean_no_aids <- mean(Lecture_Quality$No_Visual_Aids)
median_no_aids <- median(Lecture_Quality$No_Visual_Aids)
skewness_no_aids <- skewness(Lecture_Quality$No_Visual_Aids)
sd_no_aids <- sd(Lecture_Quality$No_Visual_Aids)

mean_with_aids <- mean(Lecture_Quality$With_Visual_Aids)
median_with_aids <- median(Lecture_Quality$With_Visual_Aids)
skewness_with_aids <- skewness(Lecture_Quality$With_Visual_Aids)
sd_with_aids <- sd(Lecture_Quality$With_Visual_Aids)

# Performing Shapiro-Wilk Normality Test
shapiro_test_no_aids <- shapiro.test(Lecture_Quality$No_Visual_Aids)
shapiro_test_with_aids <- shapiro.test(Lecture_Quality$With_Visual_Aids)


# Printing results
print(paste("No Visual Aids - Mean:", mean_no_aids, "Median:", median_no_aids, "Skewness:", skewness_no_aids))
print(paste("With Visual Aids - Mean:", mean_with_aids, "Median:", median_with_aids, "Skewness:", skewness_with_aids))
print(shapiro_test_no_aids)
print(shapiro_test_with_aids)

# t-test
t_test_results <- t.test(Lecture_Quality$With_Visual_Aids, Lecture_Quality$No_Visual_Aids, paired = TRUE)
print(t_test_results)

library(psych)
windows(20,10)
pairs.panels(Lecture_Quality,
             smooth = FALSE,
             scale = FALSE,
             density = TRUE,
             ellipses = FALSE,
             method = "spearman",
             pch = 21,
             lm =FALSE,
             cor = TRUE,
             jiggle = FALSE,
             factor = 2,
             hist.col = 4,
             stars = TRUE,
             ci = TRUE)



# Read Data
data <- read_csv("GitHub/R-Portfolio/data.csv", 
  col_types = cols(age = col_integer(), 
  recall = col_integer(), score = col_number(), 
  year = col_integer()))

# Convert both independent variables to factors with appropriate levels
data$recall <- factor(data$recall, 
                       levels = c(0, 2),
                       labels = c("Immediate", "Delay"))

data$candy <- factor(data$candy, 
                      levels = c("Sugar", "None", "Sugarfree"),
                      labels = c("Jolly Ranchers", "None", "Sugarfree Mints"))

head(data)

# Create a basic frequency table for the data
table(data$recall, data$candy)

install.packages(
  "ggplot2",
  repos = c("http://rstudio.org/_packages",
            "http://cran.rstudio.com")
)
#> You may also find it useful to restart R,
#> In RStudio, that's the menu Session >> Restart R

library(ggplot2)

# Box plot with multiple groups
# +++++++++++++++++++++
# Plot score by groups
# Color box plot by second group (candy type)
library("ggpubr")
ggboxplot(data, x = "recall", y = "score", color = "candy", palette = c("#00AFBB", "#E7B800", "#333333"))

# Two-Way ANOVA
res.aov2 <- aov(score ~ recall + candy, data = data)
summary(res.aov2)

# Neither are significant alone, but what about the interaction?
res.aov3 <- aov(score ~ recall + candy + recall:candy, data = data)
summary(res.aov3)

# Convert both independent variables to factors with appropriate levels
data$Recall <- factor(data$Recall, 
                       levels = c(0, 2),
                       labels = c("Immediate", "Delay"))

data$Candy <- factor(data$Candy, 
                      levels = c("Sugar", "None", "Sugarfree"),
                      labels = c("Jolly Ranchers", "None", "Sugarfree Mints"))

head(data)

# Create a basic frequency table for the data
table(data$Recall, data$Candy)

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
# Plot tooth length ("len") by groups ("dose")
# Color box plot by a second group: "supp"
library("ggpubr")
ggboxplot(data, x = "Recall", y = "Total score", color = "Candy",
          palette = c("#00AFBB", "#E7B800"))

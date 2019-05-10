# Dependencies!
library(car)
library(readr)
library(knitr)
library(ggplot2)
library(plyr)

if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/ggpubr")

get.se <- function(y) {
  se <- sd(y)/sqrt(length(y))
  mu <- mean(y)
  c(ymin=mu-se, ymax=mu+se)
}

# Read Data
library(readr)
anova.data <- read_csv("GitHub/R-Portfolio/Research Methods Final Project/data.csv", 
                 col_types = cols(Age = col_integer(), 
                                  Candy = col_factor(levels = c("Sugar", 
                                                                "None", "Sugarfree")), Gender = col_character(), 
                                  Major = col_character(), Recall = col_factor(levels = c("0", 
                                                                                          "2")), Score = col_number(), 
                                  Year = col_integer()))
View(anova.data)
head(anova.data)

# Create a basic frequency table for the data
table(anova.data$Recall, anova.data$Candy)

# Groups are uneven... let's visualize them
ggplot(anova.data, aes(y=Score, x=Candy, fill = Recall)) + 
  geom_boxplot() + 
  theme_classic()

# Line plots with multiple groups
# +++++++++++++++++++++++
# Plot tooth length ("len") by groups ("dose")
# Color box plot by a second group: "supp"
# Add error bars: mean_se
# (other values include: mean_sd, mean_ci, median_iqr, ....)
library("ggpubr")
ggline(anova.data, x = "Candy", y = "Score", color = "Recall",
       add = c("mean_se"),
       palette = c("#00AFBB", "#E7B800"))

# Use proper effects encoding
model <- lm(Score ~ Recall * Candy, 
            data = anova.data, 
            contrasts = list(Recall = "contr.sum", Candy = "contr.poly"))

# Compute two-way ANOVA test in R for unbalanced design
sstable <- Anova(model, type = 3)
sstable$pes <- c(sstable$'Sum Sq'[-nrow(sstable)], NA)/(sstable$'Sum Sq' + sstable$'Sum Sq'[nrow(sstable)]) # SS for each effect divided by the last SS (SS_residual)

# Summarize it
summary.aov(model)

# Make a pretty table for Word
options(knitr.kable.NA = '')
kable(sstable, digits = 3)

# Store a bar plot in variable bp
bp <- ggplot(anova.data, aes(x=Candy, y=Score, fill=Recall)) +
  stat_summary(fun.y=mean, geom="bar", position="dodge", color="black")+
  stat_summary(fun.data=get.se, geom="errorbar", width=0.1, position=position_dodge(width=0.9))

# Show bp without gridlines
bp + theme(panel.grid.minor=element_blank(),
           panel.grid.major=element_blank())

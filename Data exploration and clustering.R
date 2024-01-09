---
  title: "Some type of analysis"
output: html_document
date: "2024-01-01"
---
  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r}
library(ggplot2)
library(dplyr)
library(tidyverse)
library(broom)
library(AICcmodavg)
library(ggpubr)
library(TSA)
library(tseries)
library(kableExtra)
```

```{r}
read_file <- read.csv('Processed Data/df_touch_td_demo.csv', header=TRUE, sep=',', stringsAsFactors = T) 
df_touch_td_demo  <- read_file
```

### Random forest

Part of the code was taken from user Zach on <https://www.statology.org/bagging-machine-learning/>.

```{r}
library(rpart)
library(rpart.plot)
library(randomForest)
tree <- rpart(what ~ withw + touches + delta_td + hh_not + w1_A01 + MExtraversion + MAgreeableness + MConscientiousness + MNeuroticism, data=df_touch_td_demo, control=rpart.control(cp=.005))
printcp(tree)
best <- tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"]
pruned_tree <- prune(tree, cp=best)
prp(pruned_tree, type = 4, fallen.leaves=TRUE, leaf.round = 1,
    faclen=15, #use full names for factor labels
    extra=3, #display number of obs. for each terminal node
    roundint=F, #don't round to integers in output
    digits=1) #display 5 decimal pl
```

```{r}
set.seed(25)
model <- randomForest(
  formula = what ~  withw + touches + delta_td + hh_not + w1_A01 + department + cohort + expectday + uniproblem  + mood + MExtraversion + MAgreeableness + MConscientiousness + MNeuroticism,
  data = df_touch_td_demo
)
varImpPlot(model) 
```

### ANOVA and Kruskal-Wallis test

```{r}
#Only inserting variables I am interested in.
anova.tddemo <- aov(scale(touches) ~ hh_not + MExtraversion + what + delta_td +  w1_A01 + cohort + withw +   weekday + department + mood + where + MNeuroticism + travel_fromto + travel_medium + sport + sleep  + howwasday + expectday + howwasday + uniproblem + w1_A01 + tm + MAgreeableness + MConscientiousness,
                    data = df_touch_td_demo)
summary(anova.tddemo) 
```

By changing the order of these variables it seems like the F value keeps changing. This must mean that there are some violations to the assumption for an ANOVA model, as the variables may have dependent variances - if they did, their F-value wouldn't really change based on the other predictors order and presence. I have noted some variables increase their F value up to 200 when moved or when other predictors were inserted. I will use an alternative method to ANOVA.

From <https://statsandr.com/blog/kruskal-wallis-test-nonparametric-version-anova/>: "First, the Kruskal-Wallis test compares several groups in terms of a quantitative variable. So there must be one quantitative dependent variable (which corresponds to the measurements to which the question relates) and one qualitative independent variable (with at least 2 levels which will determine the groups to compare).2

Second, remember that the Kruskal-Wallis test is a nonparametric test, so the normality assumption is not required. However, the independence assumption still holds."

```{r}
library(rstatix)
subset_df <- df_touch_td_demo %>% 
  filter(!(withw %in% c('Alone', 'No information', 'Not answer')))  %>%
  filter(!(what %in% c(' I will go to sleep', 'Sleeping', 'Expired'))) %>%
  filter(!(touches > 2000))

ggplot(subset_df) +
  aes(x = withw, y = touches) +
  geom_boxplot() +
  theme(legend.position = "none")


ggplot(subset_df) +
  aes(y = what, x = touches) +
  geom_boxplot() +
  theme(legend.position = "none")   #+ theme(axis.text.x = element_text(angle = 90))

# Function to perform the kruskal test for each variable
perform_kruskal_test <- function(variable_name) {
  output <- kruskal.test(formula = as.formula(paste("touches ~", variable_name)),
                         data = subset_df)
  return(output)
}
variable_names <- c('withw', 'hh_not', 'what',  "w1_A01", "cohort", "withw", "department", "mood", "where", 'weekday')

output_list <- lapply(variable_names, perform_kruskal_test)

# Creating a data frame with the results
table1 <- data.frame(
  "Variable" = variable_names,
  "p value" = sapply(output_list, function(output) output$p.value),
  "Kruskal-Wallis chi-squared" = sapply(output_list, function(output) as.numeric(output$statistic))
)

# Displaying the results
print(table1)

library(FSA)

# Dunn test
perform_dunn_test <- function(variable_name) {
  output <- dunn_test(formula = as.formula(paste("touches ~", variable_name)),
                         data = subset_df)
  sub_output <- output %>%
    arrange(statistic) %>%
    head(1)
  return(sub_output)
}

output_list <- lapply(variable_names, perform_dunn_test)
table2 <- data.frame(
  "Variable" = variable_names,
  "Comparison" = sapply(output_list, function(output) paste(output$group1, " vs. ", output$group2)),
  "P value" = sapply(output_list, function(output) output$p.adj),
  "Dunn test statistic" = sapply(output_list, function(output) output$statistic)
)

print(table2)
write.csv(table1, "Processed Data/table1.csv", row.names=FALSE)
write.csv(table2, "Processed Data/table2.csv", row.names=FALSE)

kruskal_test(subset_df, touches ~ weekday)
```

### Linear regression (numerical variables only)

```{r}
excluded_columns <- c("datein_ques", "datein_answ", "DD_not", "hh_not", "mm_not", "A3c", 
                      "tdtot", "motot","answerduration_td", "what1", "what2", "where2", "withw2",
                      'A6b',  'travel_fromto', 'tm',  'sport', 'sleep')
included_columns <- c('touches', 'timestamp', 'weekday', 'day', 'week', 'what', 'where',
                       'withw', 'mood', 'delta_td', 'expectday', 'delta_mo', 'howwasday',
                      'uniproblem', 'delta_ev', 'delta_sn', 'w1_A01', 'department', 'cohort')
```

```{r}
l_mod = lm(touches ~ hh_not * weekday , data=df_touch_td_demo)
print(summary(l_mod))  # There is an Adjusted R^2 of 0.05038 
hh_not2 <- df_touch_td_demo$hh_not^2 
q_mod = lm(touches ~ hh_not* weekday + hh_not2 , data=df_touch_td_demo )  
print(summary(q_mod))  # There is an Adjusted R^2 of 0.05077 
#Day never has significance when "weekday" is not in the model. 
#The model's adjusted R^2 does not improve when adding quadratic terms.
```

### Clustering with K-MODES

Part of this code is from <https://search.r-project.org/CRAN/refmans/klaR/html/kmodes.html>.

```{r}
set.seed(25)
library(klaR)
library(factoextra)
library(FactoMineR)
library(clustMixType)
subset_df <- df_touch_td_demo %>% 
  filter(!(withw %in% c('Alone', 'No information', 'Not answer')))  

kmodes(data=subset_df[, c('withw', 'hh_not', 'what',  "w1_A01", "cohort", "withw", "department", "mood", "where", 'MExtraversion', 'MAgreeableness', 'MConscientiousness', 'MNeuroticism')], 
       modes = 40, 
       iter.max = 10, weighted = FALSE, fast = TRUE)




subset_df <- subset_df[, c('w1_A01', 'cohort', 'department', 'where', 'what', 'withw')] 
#"mood" and "nationality" were previously inserted but removed because they don't vary much, while 
#"where", "what" and "weekday" or other time-sensitive variable do not indicate very stable clusters but 
# rather some most typical occurrences

k_modes_result <- kmodes(subset_df, modes=9, iter.max = 100)

View(k_modes_result$modes)
cluster_table = k_modes_result$modes
write.csv(cluster_table, "Processed Data/cluster_table.csv", row.names=FALSE)

subset_df <- subset_df[, c('what', 'withw')] 
k_modes_result <- kmodes(subset_df, modes=20, iter.max = 100)
View(k_modes_result$modes)
cluster_table = k_modes_result$modes
write.csv(cluster_table, "Processed Data/cluster_table.csv", row.names=FALSE)
```

### Akaike Information Criterion

```{r}
library(AICcmodavg)
model.set <- list(l_mod, q_mod)
model.names <- c("linear model", 'quadratic model')
aictab(model.set, modnames = model.names)
```

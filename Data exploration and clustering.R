---
  title: "Some type of analysis"
output: html_document
date: "2024-01-01"
---
  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r warning=FALSE}
library(ggplot2)
library(dplyr, quietly=TRUE)
library(tidyverse , quietly=TRUE)
library(broom)
library(AICcmodavg)
library(ggpubr)
library(TSA)
library(tseries)
library(kableExtra)
library(rpart)
library(rpart.plot)
library(randomForest)
library(Metrics)
library(FSA)
set.seed(25)
```

```{r}
read_file <- read.csv('Processed Data/df_touch_td_demo.csv', header=TRUE, sep=',', stringsAsFactors = T) 
df_touch_td_demo  <- read_file
```

### Random forest

Part of the code was taken from user Zach on https://www.statology.org/random-forests/ . 

```{r}
# Preparing data
colnames(df_touch_td_demo)[106] <- "sex" 
df_touch_td_demo$touches <- as.numeric(df_touch_td_demo$touches)
n <- nrow(df_touch_td_demo)-nrow(df_touch_td_demo)/2  # only half of the data because it is very computationally expensive.
train_indices <- sample(1:n, 0.8 * n)
train_data <- df_touch_td_demo[train_indices, ]
test_data <- df_touch_td_demo[-train_indices, ]
```


```{r}
tree <- rpart(touches ~ what + withw + sex + department + cohort, data=train_data,
              control=rpart.control(cp=.0009))
printcp(tree)
best <- tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"]
pruned_tree <- prune(tree, cp=best)
printcp(pruned_tree)
predictions <- predict(pruned_tree, newdata = test_data)

mse <- mean((test_data$touches - predictions)^2)
cat("Mean Squared Error on Testing Data:", mse, "\n")

#resfactor = 20
#png(filename='decision_tree.png', res = 72*resfactor, height=500*resfactor, width=800*resfactor)
prp(pruned_tree, type = 4, fallen.leaves=TRUE, leaf.round = 1,
    faclen=15, #use full names for factor labels
    extra=0, #display number of obs. for each terminal node
    roundint=F, #don't round to integers in output
    digits=1) #display 5 decimal pl)
#dev.off()
```


```{r eval=FALSE}

#random forest
model <- randomForest(touches ~ what + withw + sex + department + cohort, data=train_data,
)


resfactor = 10
#png(filename='variable_importance_randomforest.png', res = 72*resfactor, height=500*resfactor, width=800*resfactor)
varImpPlot(model) 
#dev.off()
#predicting with the first model:
predictions <- predict(model, newdata = test_data)

print(rmse(test_data$touches, predictions))

cat("Mean Squared Error on Testing Data:", mse, "\n")

```

```{r eval=FALSE}
#Tuning the model

model_tuning <- tuneRF(
  x= train_data[,c("sex", 'cohort', 'withw', 'department', 'what')], 
  y= train_data[,c('touches')],
  type = "regression",
  ntreeTry=50,
  mtryStart=4,   # p/3 where p is the number of variables
  stepFactor=0.9,  
  improve=0.01,
  trace=FALSE,
  plot = FALSE,
  doBest = FALSE
)
```
```{r}
# Selecting the best parameter  (3)
best_mtry <- model_tuning[model_tuning[,2] == min(model_tuning[,2]), 1]

# After tuning
final_model <- randomForest(touches ~ what + withw + sex + department + cohort, 
                            data=train_data, mtry=3, ntree=250
)

# Predicting with the final, tuned model :
predValues <- predict(final_model ,newdata=test_data)

varImpPlot(final_model)

```





### ANOVA and Kruskal-Wallis test

```{r}
#Only inserting variables I am interested in.
anova.tddemo <- aov(touches ~ what + withw + sex + department + cohort,
                    data = df_touch_td_demo)

summary(anova.tddemo) 
png(filename='variable_importance_randomforest.png', res = 72*resfactor, height=500*resfactor, width=800*resfactor)
plot(anova.tddemo)
dev.off()

```

By changing the order of these variables it seems like the F value keeps changing. This must mean that there are some violations to the assumption for an ANOVA model, as the variables may have dependent variances - if they did, their F-value wouldn't really change based on the other predictors order and presence. I have noted some variables increase their F value up to 200 when moved or when other predictors were inserted. I will use an alternative method to ANOVA.

From <https://statsandr.com/blog/kruskal-wallis-test-nonparametric-version-anova/>: "First, the Kruskal-Wallis test compares several groups in terms of a quantitative variable. So there must be one quantitative dependent variable (which corresponds to the measurements to which the question relates) and one qualitative independent variable (with at least 2 levels which will determine the groups to compare).

Second, remember that the Kruskal-Wallis test is a nonparametric test, so the normality assumption is not required. However, the independence assumption still holds."

```{r}
library(rstatix)
subset_df <- df_touch_td_demo %>% 
  filter(!(withw %in% c('Alone', 'No information', 'Not answer', 'Expired')))  %>%
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
variable_names <- c('withw', 'what',  "sex", "cohort", 'department')

output_list <- lapply(variable_names, perform_kruskal_test)

# Creating a data frame with the results
table1 <- data.frame(
  "Variable" = variable_names,
  "p value" = sapply(output_list, function(output) output$p.value),
  "Kruskal-Wallis chi-squared" = sapply(output_list, function(output) as.numeric(output$statistic))
)

# Displaying the results
print(table1)



# Dunn test
perform_dunn_test <- function(variable_name) {
  output <- dunn_test(formula = as.formula(paste("touches ~", variable_name)),
                         data = subset_df)
  sub_output <- output %>%
    arrange(desc(abs(statistic))) %>%  #this selects the strongest statistic
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

kmodes(data=subset_df[, c('withw', 'hh_not', 'what',  "sex", "cohort", "withw", "department")], 
       modes = 40, 
       iter.max = 10, weighted = FALSE, fast = TRUE)

subset_df <- subset_df[, c('w1_A01', 'cohort', 'department', 'where', 'what', 'withw')] 

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

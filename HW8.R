library(tidytext)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(textdata)
library(data.table)


# Set the working directory
setwd("/Users/singhh/Downloads/CSCI48900")


######### Question 1(a) Random Forest #########

crime_data <-read.csv(file="compas-scores-two-years-violent.csv")

# Split the dataset into training and test
require(caTools)

set.seed(123)
sample = sample.split(crime_data,SplitRatio = 0.75) 
train =subset(crime_data,sample ==TRUE) 
test=subset(crime_data, sample==FALSE)

# Convert the variables
train$two_year_recid <- as.factor(as.character(train$two_year_recid))
train$sex <- as.factor(as.character(train$sex))
train$race <- as.factor(as.character(train$race))
train$is_recid <- as.factor(as.character(train$is_recid))
train$is_violent_recid <- as.factor(as.character(train$is_violent_recid))
train$score_text <- as.factor(as.character(train$score_text))
train$v_score_text <- as.factor(as.character(train$v_score_text))

# Convert the variables
test$sex <- as.factor(as.character(test$sex))
test$race <- as.factor(as.character(test$race))
test$is_recid <- as.factor(as.character(test$is_recid))
test$is_violent_recid <- as.factor(as.character(test$is_violent_recid))
test$score_text <- as.factor(as.character(test$score_text))
test$v_score_text <- as.factor(as.character(test$v_score_text))

# Get summary
summary(train)

library(mice)
imputed <- complete(mice(train, maxit=0))
# Build the logistic regression model
imputed <- subset(imputed, select = -c(id))
# str(imputed$decile_score.1)

library('randomForest')
# Build the model
model4 <- randomForest(two_year_recid ~ sex + race + is_recid + is_violent_recid + score_text + v_score_text, data = imputed)


# Show model error
plot(model4, ylim=c(0, 0.36))
legend('topright', colnames(model4$err.rate), col=1:3, fill=1:3)

# Get importance
importance    <- importance(model4)
varImportance <- data.frame(Variables = row.names(importance),
                            Importance = round(importance[ ,'MeanDecreaseGini'],2))

# Create a rank variable based on importance
rankImportance <- varImportance %>%
  mutate(Rank = paste0('#',dense_rank(desc(Importance))))

library('ggthemes')
# Use ggplot2 to visualize the relative importance of variables
ggplot(rankImportance, aes(x = reorder(Variables, Importance),
                           y = Importance, fill = Importance)) +
  geom_bar(stat='identity') +
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 4, colour = 'red') +
  labs(x = 'Variables') +
  coord_flip() +
  theme_few()

# Predict using the test set
prediction <- predict(model4, test)

# Save the solution
solution4<- data.frame(id = test$id, test_two_year_recid = test$two_year_recid, prediction_two_year_recid = prediction)




######### Question 1(c) LIME #########

library(MASS)
library(lime)

# First we'll clean up the data a bit
imputed$id <- NULL

# Now we'll fit a linear discriminant model
set.seed(4)
test_set <- sample(seq_len(nrow(imputed)), 4)

prediction <- imputed$compas_screening_date
imputed$compas_screening_date <- NULL

prediction <- imputed$dob
imputed$dob <- NULL

prediction <- imputed$age_cat
imputed$age_cat <- NULL

prediction <- imputed$race
imputed$race <- NULL

prediction <- imputed$c_jail_in
imputed$c_jail_in <- NULL

prediction <- imputed$c_jail_out
imputed$c_jail_out <- NULL

prediction <- imputed$c_case_number
imputed$c_case_number <- NULL

prediction <- imputed$c_offense_date
imputed$c_offense_date <- NULL

prediction <- imputed$c_arrest_date
imputed$c_arrest_date <- NULL

prediction <- imputed$c_charge_degree
imputed$c_charge_degree <- NULL

prediction <- imputed$c_charge_desc
imputed$c_charge_desc <- NULL

prediction <- imputed$r_case_number
imputed$r_case_number <- NULL

prediction <- imputed$vr_case_number
imputed$vr_case_number <- NULL

prediction <- imputed$vr_charge_degree
imputed$vr_charge_degree <- NULL

prediction <- imputed$vr_offense_date
imputed$vr_offense_date <- NULL

prediction <- imputed$vr_charge_desc
imputed$vr_charge_desc <- NULL

prediction <- imputed$type_of_assessment
imputed$type_of_assessment <- NULL

prediction <- imputed$violent_recid
imputed$violent_recid <- NULL

prediction <- imputed$score_text
imputed$score_text <- NULL

prediction <- imputed$screening_date
imputed$screening_date <- NULL

prediction <- imputed$v_type_of_assessment
imputed$v_type_of_assessment <- NULL

prediction <- imputed$v_score_text
imputed$v_score_text <- NULL

prediction <- imputed$v_screening_date
imputed$v_screening_date <- NULL

prediction <- imputed$in_custody
imputed$in_custody <- NULL

prediction <- imputed$out_custody
imputed$out_custody <- NULL

prediction <- imputed$sex
imputed$sex <- NULL

prediction <- imputed$r_charge_degree
imputed$r_charge_degree <- NULL

prediction <- imputed$r_charge_desc
imputed$r_charge_desc <- NULL

prediction <- imputed$r_offense_date
imputed$r_offense_date <- NULL

prediction <- imputed$r_jail_in
imputed$r_jail_in <- NULL

prediction <- imputed$r_jail_out
imputed$r_jail_out <- NULL

prediction <- imputed$days_b_screening_arrest
imputed$days_b_screening_arrest <- NULL

# model <- lda(imputed[-test_set, ], prediction[-test_set])
model <- lda(is_recid~.,data = imputed)

predict(model, imputed[test_set, ])

explainer <- lime(imputed[-test_set,], model, bin_continuous = TRUE, quantile_bins = FALSE)
explanation <- explain(imputed[test_set, ], explainer, n_labels = 1, n_features = 4)

# Only showing part of output for better printing
explanation[, 2:9]


plot_features(explanation, ncol = 1)



































# Load necessary libraries
library(psych)
library(tidyverse)
library(data.table)
library(recommenderlab)
library(jtools)
library(PASWR2)
library(knitr)
library(Hmisc)
library(PMCMRplus)
library(conflicted)
conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("as.matrix", "proxy")

##########################################################################
############################# PART 1 #####################################
##########################################################################

# Clear the workspace
rm(list = ls())

# Set working directory
setwd("C:\\Users\\YourPath\\YourFolder\\")  # Adjust the path

# Load dataset
data_part1 <- read_excel("Data_and_Recommandation_Results.xlsx", 
                                     sheet = "Data for Assignment 4- Part 1")

# Summary of the count of ratings
summary(data_part1$Count)

# Average number of movies rated by each user
average_ratings <- rowMeans(data_part1[,5:ncol(data_part1)], na.rm = TRUE)

# Plot histogram of average ratings
hist(average_ratings, main = "Average Ratings Per User",
     col = "lightblue", border = "black")

# Convert data to matrix
ratings_matrix <- as.matrix(data_part1[,5:ncol(data_part1)])
ratings_matrix <- as(ratings_matrix, "realRatingMatrix")

# Create evaluation scheme
eval_scheme <- evaluationScheme(ratings_matrix, method = "split", train = 0.75, given = 4, goodRating = 3)

# Build user-based collaborative filtering models with different normalizations
set.seed(10)
User_User_Cosine <- Recommender(getData(eval_scheme, "train"), "UBCF", 
                                         param = list(normalize = NULL, method = "Cosine"))

User_User_Cosine_Centered <- Recommender(getData(eval_scheme, "train"), "UBCF", 
                                             param = list(normalize = "center", method = "Cosine"))

User_User_Cosine_Zscore <- Recommender(getData(eval_scheme, "train"), "UBCF", 
                                        param = list(normalize = "Z-score", method = "Cosine"))

# Predict ratings
pred_cosine_1 <- predict(User_User_Cosine, getData(eval_scheme, "known"), type = "ratings")
pred_cosine_2 <- predict(User_User_Cosine_Centered, getData(eval_scheme, "known"), type = "ratings")
pred_cosine_3 <- predict(User_User_Cosine_Zscore, getData(eval_scheme, "known"), type = "ratings")

# Calculate prediction accuracy for each model
accuracy_cosine <- rbind(
  Cosine_NoNorm = calcPredictionAccuracy(pred_cosine_1, getData(eval_scheme, "unknown")),
  Cosine_Centered = calcPredictionAccuracy(pred_cosine_2, getData(eval_scheme, "unknown")),
  Cosine_Zscore = calcPredictionAccuracy(pred_cosine_3, getData(eval_scheme, "unknown"))
)

# Display accuracy results
kable(accuracy_cosine)

# Euclidean Distance - User-User
set.seed(10)
User_User_Euclidean <- Recommender(getData(eval_scheme, "train"), "UBCF", 
                                            param = list(normalize = NULL, method = "Euclidean"))

User_User_Euclidean_Centered <- Recommender(getData(eval_scheme, "train"), "UBCF", 
                                                param = list(normalize = "center", method = "Euclidean"))

User_User_Euclidean_Zscore <- Recommender(getData(eval_scheme, "train"), "UBCF", 
                                           param = list(normalize = "Z-score", method = "Euclidean"))

# Predict ratings
pred_euclidean_1 <- predict(User_User_Euclidean, getData(eval_scheme, "known"), type = "ratings")
pred_euclidean_2 <- predict(User_User_Euclidean_Centered, getData(eval_scheme, "known"), type = "ratings")
pred_euclidean_3 <- predict(User_User_Euclidean_Zscore, getData(eval_scheme, "known"), type = "ratings")

# Calculate prediction accuracy for each model
accuracy_euclidean <- rbind(
  Euclidean_NoNorm = calcPredictionAccuracy(pred_euclidean_1, getData(eval_scheme, "unknown")),
  Euclidean_Centered = calcPredictionAccuracy(pred_euclidean_2, getData(eval_scheme, "unknown")),
  Euclidean_Zscore = calcPredictionAccuracy(pred_euclidean_3, getData(eval_scheme, "unknown"))
)

# Display accuracy results
kable(accuracy_euclidean)

##########################################################################
############################# PART 2 #####################################
##########################################################################

# Clear the workspace
rm(list = ls())

# Load dataset
data_part2 <- read_excel("Data_and_Recommandation_Results.xlsx", 
                                     sheet = "Data for Assignment 4- Part 2")

# Summary of the count of ratings
summary(data_part2$Count)

# Average number of movies rated by each user
average_ratings_part2 <- rowMeans(data_part2[,5:ncol(data_part2)], na.rm = TRUE)

# Plot histogram of average ratings
hist(average_ratings_part2, main = "Average Ratings Per User - Part 2",
     col = "lightgreen", border = "black")

# Convert data to matrix
ratings_matrix_part2 <- as.matrix(data_part2[,5:ncol(data_part2)])
ratings_matrix_part2 <- as(ratings_matrix_part2, "realRatingMatrix")

# Create evaluation scheme
eval_scheme_part2 <- evaluationScheme(ratings_matrix_part2, method = "split", train = 0.80, given = 3, goodRating = 4)

# Build Item-Item collaborative filtering models with different normalizations
set.seed(15)
Item_Item_Cosine <- Recommender(getData(eval_scheme_part2, "train"), "IBCF", 
                                         param = list(normalize = NULL, method = "Cosine"))

Item_Item_Cosine_Centered <- Recommender(getData(eval_scheme_part2, "train"), "IBCF", 
                                             param = list(normalize = "center", method = "Cosine"))

Item_Item_Cosine_Zscore <- Recommender(getData(eval_scheme_part2, "train"), "IBCF", 
                                        param = list(normalize = "Z-score", method = "Cosine"))

# Predict ratings
pred_item_cosine_1 <- predict(Item_Item_Cosine, getData(eval_scheme_part2, "known"), type = "ratings")
pred_item_cosine_2 <- predict(Item_Item_Cosine_Centered, getData(eval_scheme_part2, "known"), type = "ratings")
pred_item_cosine_3 <- predict(Item_Item_Cosine_Zscore, getData(eval_scheme_part2, "known"), type = "ratings")

# Calculate prediction accuracy for each model
accuracy_item_cosine <- rbind(
  Item_Cosine_NoNorm = calcPredictionAccuracy(pred_item_cosine_1, getData(eval_scheme_part2, "unknown")),
  Item_Cosine_Centered = calcPredictionAccuracy(pred_item_cosine_2, getData(eval_scheme_part2, "unknown")),
  Item_Cosine_Zscore = calcPredictionAccuracy(pred_item_cosine_3, getData(eval_scheme_part2, "unknown"))
)

# Display accuracy results
kable(accuracy_item_cosine)

##########################################################################
############################# PART 3 #####################################
##########################################################################

# Build user-user collaborative filtering models with Euclidean distance
set.seed(20)
User_User_Euclidean_Part3 <- Recommender(getData(eval_scheme_part2, "train"), "UBCF", 
                                            param = list(normalize = NULL, method = "Euclidean"))

User_User_Euclidean_Centered_Part3 <- Recommender(getData(eval_scheme_part2, "train"), "UBCF", 
                                                param = list(normalize = "center", method = "Euclidean"))

User_User_Euclidean_Zscore_Part3 <- Recommender(getData(eval_scheme_part2, "train"), "UBCF", 
                                           param = list(normalize = "Z-score", method = "Euclidean"))

# Predict ratings
pred_euclidean_part3_1 <- predict(User_User_Euclidean_Part3, getData(eval_scheme_part2, "known"), type = "ratings")
pred_euclidean_part3_2 <- predict(User_User_Euclidean_Centered_Part3, getData(eval_scheme_part2, "known"), type = "ratings")
pred_euclidean_part3_3 <- predict(User_User_Euclidean_Zscore_Part3, getData(eval_scheme_part2, "known"), type = "ratings")

# Calculate prediction accuracy for each model
accuracy_euclidean_part3 <- rbind(
  Euclidean_NoNorm_Part3 = calcPredictionAccuracy(pred_euclidean_part3_1, getData(eval_scheme_part2, "unknown")),
  Euclidean_Centered_Part3 = calcPredictionAccuracy(pred_euclidean_part3_2, getData(eval_scheme_part2, "unknown")),
  Euclidean_Zscore_Part3 = calcPredictionAccuracy(pred_euclidean_part3_3, getData(eval_scheme_part2, "unknown"))
)

# Display accuracy results
kable(accuracy_euclidean_part3)

# Comparison across models
final_results_part3 <- data.frame(rbind(accuracy_item_cosine, accuracy_euclidean_part3))
final_results_part3 <- final_results_part3[order(final_results_part3$RMSE), ]
final_results_part3 <- final_results_part3[1:(nrow(final_results_part3)-1), ]

kable(final_results_part3)

# Plot RMSE for comparison
barplot(final_results_part3$RMSE, col = "lightcoral", main = "Model Comparison RMSE - Part 3", las = 2, ylab = "RMSE", horiz = FALSE, names.arg = rownames(final_results_part3), cex.names = 0.7)

# Predict using the best model
best_model_predictions_part3 <- predict(Item_Item_Cosine, ratings_matrix_part2, type = "ratings")
predictions_output_part3 <- as(best_model_predictions_part3, "matrix")
predictions_output_part3 <- as.data.frame(predictions_output_part3)

# Save outputs
write.csv(final_results_part3, "final_rmse_results_part3.csv")
write.csv(predictions_output_part3, "final_predictions_output_part3.csv")

##########################################################################
############################# PART 4 #####################################
##########################################################################

# Clear the workspace
rm(list = ls())

# Load dataset
data_part4 <- read_excel("Data_and_Recommandation_Results.xlsx", 
                                     sheet = "Data for Assignment 4- Part 4")

# Summary of the count of ratings
summary(data_part4$Count)

# Average number of movies rated by each user
average_ratings_part4 <- rowMeans(data_part4[,5:ncol(data_part4)], na.rm = TRUE)

# Plot histogram of average ratings
hist(average_ratings_part4, main = "Average Ratings Per User - Part 4",
     col = "lightcoral", border = "black")

# Convert data to matrix
ratings_matrix_part4 <- as.matrix(data_part4[,5:ncol(data_part4)])
ratings_matrix_part4 <- as(ratings_matrix_part4, "realRatingMatrix")

# Create evaluation scheme
eval_scheme_part4 <- evaluationScheme(ratings_matrix_part4, method = "split", train = 0.80, given = 3, goodRating = 4)

# Build Item-Item collaborative filtering models with different normalizations
set.seed(25)
Item_Item_Cosine_Part4 <- Recommender(getData(eval_scheme_part4, "train"), "IBCF", 
                                         param = list(normalize = NULL, method = "Cosine"))

Item_Item_Cosine_Centered_Part4 <- Recommender(getData(eval_scheme_part4, "train"), "IBCF", 
                                             param = list(normalize = "center", method = "Cosine"))

Item_Item_Cosine_Zscore_Part4 <- Recommender(getData(eval_scheme_part4, "train"), "IBCF", 
                                        param = list(normalize = "Z-score", method = "Cosine"))

# Predict ratings
pred_item_cosine_part4_1 <- predict(Item_Item_Cosine_Part4, getData(eval_scheme_part4, "known"), type = "ratings")
pred_item_cosine_part4_2 <- predict(Item_Item_Cosine_Centered_Part4, getData(eval_scheme_part4, "known"), type = "ratings")
pred_item_cosine_part4_3 <- predict(Item_Item_Cosine_Zscore_Part4, getData(eval_scheme_part4, "known"), type = "ratings")

# Calculate prediction accuracy for each model
accuracy_item_cosine_part4 <- rbind(
  Item_Cosine_NoNorm_Part4 = calcPredictionAccuracy(pred_item_cosine_part4_1, getData(eval_scheme_part4, "unknown")),
  Item_Cosine_Centered_Part4 = calcPredictionAccuracy(pred_item_cosine_part4_2, getData(eval_scheme_part4, "unknown")),
  Item_Cosine_Zscore_Part4 = calcPredictionAccuracy(pred_item_cosine_part4_3, getData(eval_scheme_part4, "unknown"))
)

# Display accuracy results
kable(accuracy_item_cosine_part4)

# Comparison across models
final_results_part4 <- data.frame(rbind(accuracy_item_cosine_part4, accuracy_euclidean_part3))
final_results_part4 <- final_results_part4[order(final_results_part4$RMSE), ]
final_results_part4 <- final_results_part4[1:(nrow(final_results_part4)-1), ]

kable(final_results_part4)

# Plot RMSE for comparison
barplot(final_results_part4$RMSE, col = "lightblue", main = "Model Comparison RMSE - Part 4", las = 2, ylab = "RMSE", horiz = FALSE, names.arg = rownames(final_results_part4), cex.names = 0.7)

# Predict using the best model
best_model_predictions_part4 <- predict(Item_Item_Cosine_Part4, ratings_matrix_part4, type = "ratings")
predictions_output_part4 <- as(best_model_predictions_part4, "matrix")
predictions_output_part4 <- as.data.frame(predictions_output_part4)

# Save outputs
write.csv(final_results_part4, "final_rmse_results_part4.csv")
write.csv(predictions_output_part4, "final_predictions_output_part4.csv")

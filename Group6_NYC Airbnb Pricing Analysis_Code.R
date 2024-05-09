library(tidyverse)
# Load CSV file
df <- read.csv("listings.csv")

df$host_response_time

summary(df$host_response_time)

summary(df$neighbourhood_cleansed)

subset_df <- subset(df, select = c(host_response_time, neighbourhood_group_cleansed, review_scores_rating, price, room_type))

summary(subset_df)  

# cleaning  price data 

cleaned_data <- subset_df[!is.na(subset_df$price) & subset_df$price != "", ]


# Assuming you have the 'cleaned_data' data frame

# Calculate the mean of the 'review_scores_rating' column excluding NA values
mean_rating <- mean(cleaned_data$review_scores_rating, na.rm = TRUE)

# Replace NA values in 'review_scores_rating' column with the mean
cleaned_data$review_scores_rating[is.na(cleaned_data$review_scores_rating)] <- mean_rating

summary(cleaned_data)

print(unique(cleaned_data$host_response_time))

library(dplyr)
df_clean <- cleaned_data %>%
  filter(host_response_time != "N/A" & host_response_time != "")
df_clean
# Remove "$" sign and convert to numeric
df_clean$price <- as.numeric(gsub("\\$", "", df_clean$price))
df_clean <- na.omit(df_clean[df_clean$price, ])
# Create dummy variables for both columns
dummies <- model.matrix(~ host_response_time + room_type + neighbourhood_group_cleansed - 1, data = df_clean)


# Bind the dummy variables to the original dataframe
df_dummies <- cbind(df_clean, dummies)

# Print the result
print(df_dummies)

df_dummies <- subset(df_dummies, select = -c(1,2,5))
df_dummies
names(df_dummies)

lm <- lm(price ~ review_scores_rating + `host_response_timewithin a day` +
           `host_response_timewithin an hour` + neighbourhood_group_cleansedManhattan +
           `neighbourhood_group_cleansedStaten Island` + `host_response_timewithin a few hours` +
           neighbourhood_group_cleansedBrooklyn + neighbourhood_group_cleansedQueens + `room_typeHotel room` + 
           `room_typePrivate room` + `room_typeShared room`, data = df_dummies)

summary(lm)

#log transformation of price
lm2 <- lm(log(price) ~ review_scores_rating + `host_response_timewithin a day` +
           `host_response_timewithin an hour` + neighbourhood_group_cleansedManhattan +
           `neighbourhood_group_cleansedStaten Island` + `host_response_timewithin a few hours` +
           neighbourhood_group_cleansedBrooklyn + neighbourhood_group_cleansedQueens +`room_typeHotel room` + 
            `room_typePrivate room` + `room_typeShared room`, data = df_dummies)
summary(lm2)

#only including the significant variables from above equation
lm3 <- lm(log(price) ~ review_scores_rating + `host_response_timewithin an hour` + neighbourhood_group_cleansedManhattan + 
           `host_response_timewithin a few hours` + neighbourhood_group_cleansedBrooklyn + 
            neighbourhood_group_cleansedQueens + `room_typeHotel room` + 
            `room_typePrivate room` + `room_typeShared room` + `host_response_timewithin a day` +
            `host_response_timewithin an hour`, data = df_dummies)
summary(lm3)

# Load the ggcorrplot package
install.packages("ggcorrplot")
library(ggcorrplot)

# Assuming 'df' is your dataframe
# Calculate the correlation matrix
correlation_matrix <- cor(df_dummies, use = "pairwise.complete.obs")
correlation_matrix
# Plot the correlation matrix using ggcorrplot
ggcorrplot(correlation_matrix, type = "upper", colors = c("#7FDBFF", "white", "#0074D9"), 
           lab = TRUE, lab_size = 3, method = "circle", title = "Correlation Matrix")


library(ivreg)
iv_model <- ivreg(log(price) ~ review_scores_rating + `host_response_timewithin an hour` + neighbourhood_group_cleansedManhattan + 
                    `host_response_timewithin a few hours` + neighbourhood_group_cleansedBrooklyn + 
                     neighbourhood_group_cleansedQueens | `neighbourhood_group_cleansedStaten Island` + 
                    `host_response_timea few days or more`, data = df_dummies)
summary(iv_model)



lm4 <- lm(log(price) ~ neighbourhood_group_cleansedManhattan +`neighbourhood_group_cleansedStaten Island` 
          + neighbourhood_group_cleansedBrooklyn + neighbourhood_group_cleansedQueens, data = df_dummies )
summary(lm4)


lm5 <- lm(log(price) ~ review_scores_rating + `host_response_timewithin an hour` + neighbourhood_group_cleansedManhattan + 
            `host_response_timewithin a few hours` + neighbourhood_group_cleansedBrooklyn + 
            neighbourhood_group_cleansedQueens + `room_typeHotel room` + 
            `room_typePrivate room` + `room_typeShared room` + `host_response_timewithin a day` +
            `host_response_timewithin an hour` + review_scores_rating*`room_typeHotel room` + 
            review_scores_rating*`room_typePrivate room` + review_scores_rating*`room_typeShared room`, data = df_dummies)
summary(lm5)

lm6 <- lm(log(price) ~ review_scores_rating + `host_response_timewithin an hour` + neighbourhood_group_cleansedManhattan + 
            `host_response_timewithin a few hours` + neighbourhood_group_cleansedBrooklyn + 
            neighbourhood_group_cleansedQueens + `room_typeHotel room` + 
            `room_typePrivate room` + `room_typeShared room` + `host_response_timewithin a day` +
            `host_response_timewithin an hour` +review_scores_rating*`room_typePrivate room`, data = df_dummies)
summary(lm6)

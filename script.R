library(data.table)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyr)


setwd("set_the_working_folder")

# Prepare Metadata >>>>>>>>>>>>>

india <- fread("india_metadata.tsv", sep = "\t")
india_subset <- india[, c(5, 6, 7, 11, 12, 13, 14)]
colnames(india_subset) <- c("accession", "collection_date", "location", "age", "gender", "clade", "lineage")

# Remove empty values
india_subset$age <- as.integer(india_subset$age)
age <- subset(india_subset, grepl("^\\d+$", age) & age != "")

metadata <- age

# Convert collection_date to Date format
metadata$collection_date <- as.Date(metadata$collection_date)
metadata$collection_date <- gsub("^(.?-.)-.*", "\\1", metadata$collection_date)

metadata <- subset(age, grepl("^\\d{4}-\\d{2}", collection_date) & collection_date != "")
metadata <- metadata[metadata$lineage != "", ]


# Split and add the state information
location_parts <- strsplit(metadata$location, "/")
metadata$new_column <- sapply(location_parts, function(x) trimws(x[3]))
metadata$dis <- sapply(location_parts, function(x) trimws(x[4]))

#Uniform state names 

metadata$new_column <- ifelse(metadata$new_column == "Andhra pradesh", "Andhra Pradesh", metadata$new_column)
metadata$new_column <- ifelse(metadata$new_column == "ARUNACHAL PRADESH", "Arunachal Pradesh", metadata$new_column)
metadata$new_column <- ifelse(metadata$new_column == "Madhya pradesh", "Madhya Pradesh", metadata$new_column)
metadata$new_column <- ifelse(metadata$new_column == "New Delhi", "Delhi", metadata$new_column)
metadata$new_column <- ifelse(metadata$new_column == "Jammu & Kashmir", "Jammu and Kashmir", metadata$new_column)
metadata$new_column <- ifelse(metadata$new_column == "Jammu and Kashmīr", "Jammu and Kashmir", metadata$new_column)
metadata$new_column <- ifelse(metadata$new_column == "Dadra and Nagar Haveli and Daman and Diu", "Daman and Diu", metadata$new_column)
metadata$new_column <- ifelse(metadata$new_column == "Dadra and Nagar Haveli", "Daman and Diu", metadata$new_column)
metadata$new_column <- ifelse(metadata$new_column == "Dadra And Nagar Haveli", "Daman and Diu", metadata$new_column)
metadata$new_column <- ifelse(metadata$new_column == "Dadra & Nagar Haveli", "Daman and Diu", metadata$new_column)
metadata$new_column <- ifelse(metadata$new_column == "Jammu & kashmir", "Jammu and Kashmir", metadata$new_column)
metadata$new_column <- ifelse(metadata$new_column == "Mamil Nadu", "Tamil Nadu", metadata$new_column)
metadata$new_column <- ifelse(metadata$new_column == "Tamilnadu", "Tamil Nadu", metadata$new_column)
metadata$new_column <- ifelse(metadata$new_column == "Gujart", "Gujarat", metadata$new_column)
metadata$new_column <- ifelse(metadata$new_column == "Gujrat", "Gujarat", metadata$new_column)
metadata$new_column <- ifelse(metadata$new_column == "Aasam", "Assam", metadata$new_column)
metadata$new_column <- ifelse(metadata$new_column == "Jammu", "Jammu and Kashmir", metadata$new_column)
metadata$new_column <- ifelse(metadata$new_column == "Harayana", "Haryana", metadata$new_column)
metadata$new_column <- ifelse(metadata$new_column == "Mumbai", "Maharashtra", metadata$new_column)
metadata$new_column <- ifelse(metadata$new_column == "Chandighar", "Chandigarh", metadata$new_column)
metadata$new_column <- ifelse(metadata$new_column == "Jarkhand", "Jharkhand", metadata$new_column)
metadata$new_column <- ifelse(metadata$new_column == "Pondicherry", "Puducherry", metadata$new_column)
metadata$new_column <- ifelse(metadata$new_column == "Maharshtra", "Maharashtra", metadata$new_column)
metadata$new_column <- ifelse(metadata$new_column == "Maharasthra", "Maharashtra", metadata$new_column)
metadata$new_column <- ifelse(metadata$new_column == "Chhatisgarh", "Chhattisgarh", metadata$new_column)
metadata <- metadata[!(metadata$new_column %in% c("Nairobi", "NA", "Bangladesh")) & !is.na(metadata$new_column), ]

metadata <- metadata %>%
  select(-location) %>%  # Remove the "location" column
  rename(location = new_column)  # Rename "new_column" to "location"


#Uniform age
metadata$age <- ifelse(metadata$age == "08", "8", metadata$age)
metadata$age <- ifelse(metadata$age == "05", "5", metadata$age)
metadata$age <- ifelse(metadata$age == "07", "7", metadata$age)
metadata$age <- ifelse(metadata$age == "04", "4", metadata$age)
metadata$age <- ifelse(metadata$age == "02", "2", metadata$age)


# Uniform gender 
metadata$gender <- gsub("\\bMaleale\\b", "Male", metadata$gender)
metadata$gender <- gsub("\\bM\\.\\b", "Male", metadata$gender)
metadata$gender <- gsub("\\bTg\\b", "Others", metadata$gender)
metadata$gender <- gsub("\\bTransgender\\b", "Others", metadata$gender)
metadata$gender <- gsub("\\bO\\b", "Others", metadata$gender)
metadata$gender <- gsub("\\bUndefined\\b", "Unknown", metadata$gender)
metadata$gender <- gsub("\\bUnknown\\b", "Unknown", metadata$gender)
metadata$gender <- gsub("\\bFamale\\b", "Female", metadata$gender)
metadata$gender <- gsub("\\bFEMLAE\\b", "Female", metadata$gender)
metadata$gender <- gsub("\\bFFemale\\b", "Female", metadata$gender)
metadata$gender <- gsub("\\bFFEMALE\\b", "Female", metadata$gender)
metadata$gender <- gsub("\\bFm\\b", "Female", metadata$gender)
metadata$gender <- gsub("\\bfamale\\b", "Female", metadata$gender)
metadata$gender <- gsub("\\bFeM\\b", "Female", metadata$gender)
metadata$gender <- gsub("\\undefined\\b", "Unknown", metadata$gender)
metadata$gender <- gsub("\\unkown\\b", "Unknown", metadata$gender)
metadata$gender <- gsub("\\unknown\\b", "Unknown", metadata$gender)
metadata$gender <- gsub("\\Unkown\\b", "Unknown", metadata$gender)
metadata <- subset(metadata, !(gender %in% c("FemaleeMaleale", "-", "26.0", "Unknown", "Others")))


# Combine Metadata and mutation information 

group1 <- read.delim("group1.tsv", header = TRUE, sep = "\t")
group2 <- read.delim("group2.tsv", header = TRUE, sep = "\t")
group3 <- read.delim("group3.tsv", header = TRUE, sep = "\t")

group_files <- c("group1.tsv", "group2.tsv", "group3.tsv")

# Initialize an empty dataframe to store the combined results
combined_results <- data.frame()

# Loop through each group file
for (group_file in group_files) {
  # Load group file
  group_data <- read.table(group_file, header = TRUE, stringsAsFactors = FALSE)
  
  # Left join group data with metadata
  result <- left_join(group_data, metadata, by = c("SAMPLE_ID" = "accession"))
  
  # Combine results
  combined_results <- bind_rows(combined_results, result)
}

combined_results <- combined_results[complete.cases(combined_results[, c(8, 9, 10, 11)]), ]

## 5 or more than 5 occurrence 

# Find counts of unique combinations of POS, REF, and ALT
counts <- table(with(combined_results, paste(POS, REF, ALT)))

# Subset the data frame based on counts
threshold <- 5
valid_combinations <- names(counts)[counts >= threshold]

combined_data <- combined_results[with(combined_results, paste(POS, REF, ALT)) %in% valid_combinations, ]


# Subset where age is 0 - 17
subset_0_17 <- combined_data[combined_data$age >= 0 & combined_data$age <= 17, ]

# Subset where age is 18 - 64
subset_18_64 <- combined_data[combined_data$age >= 18 & combined_data$age <= 64, ]

# Subset where age is 65 - 100
subset_65_100 <- combined_data[combined_data$age >= 65 & combined_data$age <= 100, ]

combined_data <- rbind(subset_0_17, subset_18_64, subset_65_100)


# >>>>>>>>>> Random sub sampling  #############################

# Set seed for reproducibility
set.seed(123)

group1_sample_ids <- unique(subset_0_17$SAMPLE_ID)
group2_sample_ids <- unique(subset_18_64$SAMPLE_ID)
group3_sample_ids <- unique(subset_65_100$SAMPLE_ID)


random_sample_ids_group1 <- sample(group1_sample_ids, 23325, replace = FALSE)
random_sample_ids_group2 <- sample(group2_sample_ids, 23325, replace = FALSE)
random_sample_ids_group3 <- sample(group3_sample_ids, 23325, replace = FALSE)

subsample_0_17 <- subset(subset_0_17, SAMPLE_ID %in% random_sample_ids_group1)
subsample_18_64 <- subset(subset_18_64, SAMPLE_ID %in% random_sample_ids_group2)
subsample_65_100 <- subset(subset_65_100, SAMPLE_ID %in% random_sample_ids_group3)
subset_combined_data <- rbind(subsample_0_17, subsample_18_64, subsample_65_100)



### chi-squre test
                       
# Create a contingency table
contingency_table <- matrix(c(group1_unique_count, group2_unique_count, group3_unique_count), 
                            nrow = 1, 
                            byrow = TRUE)

# Print the contingency table
print(contingency_table)



chi_squared_test <- chisq.test(contingency_table)

# Print the results
print(chi_squared_test)

# Interpret the p-value
alpha <- 0.05
if (chi_squared_test$p.value < alpha) {
  print("There is a significant difference in the number of unique mutations between the groups.")
} else {
  print("There is no significant difference in the number of unique mutations between the groups.")
}


##>>>>>>>> Pair-wise chi-squre test

# Create contingency tables for pairwise comparisons
contingency_table_1_2 <- matrix(c(group1_unique_count, group2_unique_count), 
                                nrow = 1, byrow = TRUE)
contingency_table_2_3 <- matrix(c(group2_unique_count, group3_unique_count), 
                                nrow = 1, byrow = TRUE)
contingency_table_1_3 <- matrix(c(group1_unique_count, group3_unique_count), 
                                nrow = 1, byrow = TRUE)

# Perform Chi-squared tests
chi_squared_test_1_2 <- chisq.test(contingency_table_1_2)
chi_squared_test_2_3 <- chisq.test(contingency_table_2_3)
chi_squared_test_1_3 <- chisq.test(contingency_table_1_3)

# Print results
print(chi_squared_test_1_2)
print(chi_squared_test_2_3)
print(chi_squared_test_1_3)

# Adjust p-values using Bonferroni correction
p_values <- c(chi_squared_test_1_2$p.value, chi_squared_test_2_3$p.value, chi_squared_test_1_3$p.value)
adjusted_p_values <- p.adjust(p_values, method = "bonferroni")

print(adjusted_p_values)

# Interpret the results
alpha <- 0.05
if (adjusted_p_values[1] < alpha) {
  print("Group 2 has a significantly higher number of unique mutations compared to Group 1.")
} else {
  print("No significant difference between Group 2 and Group 1.")
}

if (adjusted_p_values[2] < alpha) {
  print("Group 2 has a significantly higher number of unique mutations compared to Group 3.")
} else {
  print("No significant difference between Group 2 and Group 3.")
}

if (adjusted_p_values[3] < alpha) {
  print("Group 3 has a significantly higher number of unique mutations compared to Group 1.")
} else {
  print("No significant difference between Group 3 and Group 1.")
}


# Friedma - Test
                       
friedman.test(Mutation_Frequency ~ Vaccination_Status | Age_Group, data = data)





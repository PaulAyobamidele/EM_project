# importing data
data <- read.csv("final_project/project.csv")
View(data)
summary(data)

# deleting first 2 colums
new_data <- data[-c(1, 2), ]
View(new_data)


na_count <- sum(is.na(new_data$scl_dpt))
empty_string_count <- sum(new_data$scl_dpt == "")


# deleting evry row that has an empty string in its scl dpt
new_data2 <- subset(new_data, scl_dpt != "")
View(new_data2)


names(data)
library(dplyr)
# deleting unnecessery colums
new_data3 <- new_data2 %>%
    select(
        -StartDate, -EndDate, -Status, -IPAddress, -Progress, -Duration..in.seconds.,
        -Finished, -RecordedDate, -ResponseId, -RecipientLastName, -RecipientFirstName,
        -RecipientLastName, -RecipientEmail, -ExternalReference, -LocationLatitude,
        -LocationLongitude, -DistributionChannel, -UserLanguage
    )
View(new_data3)

# extracting block where there is a high enagagement frequancy
yes_block <- new_data3[new_data3$No_or_YesBlock_1 >= 4, ]
View(yes_block)

# extracting block where there is a low enagagement frequancy
no_block <- new_data3[new_data3$No_or_YesBlock_1 <= 3, ]
View(no_block)

yes_block <- yes_block %>%
    select(
        -reson_on_lack, -reson_on_lack_5_TEXT, -is_nan_positive,
        -satis_on_experience_1, -satis_on_performance_1, -aware_of_nan,
        -if_change_yes_no, -suggest_new_nan
    )
View(yes_block)

no_block <- no_block %>%
    select(
        -Satisf_NA_1, -stress_manageY_1, -time_manageY_1,
        -satis_with_communiY_1, -aware_with_nanY, -nan_grades_1,
        -nan_overall_expY_1, -suggest_new_nanY, -input_for._new_nan
    )
View(no_block)

# turning variables to numeric
yes_block$stress_manageY_1 <- as.numeric(as.character(yes_block$stress_manageY_1))
yes_block$Satisf_NA_1 <- as.numeric(as.character(yes_block$Satisf_NA_1))
yes_block$time_manageY_1 <- as.numeric(as.character(yes_block$time_manageY_1))
yes_block$satis_with_communiY_1 <- as.numeric(as.character(yes_block$satis_with_communiY_1))
yes_block$nan_grades_1 <- as.numeric(as.character(yes_block$nan_grades_1))
yes_block$nan_overall_expY_1 <- as.numeric(as.character(yes_block$nan_overall_expY_1))

no_block$satis_on_experience_1 <- as.numeric(as.character(no_block$satis_on_experience_1))
no_block$satis_on_performance_1 <- as.numeric(as.character(no_block$satis_on_performance_1))

summary(yes_block)

# turning factor variables
no_block$reson_on_lack <- as.factor(no_block$reson_on_lack)
no_block$is_nan_positive <- as.factor(no_block$is_nan_positive)
no_block$aware_of_nan <- as.factor(no_block$aware_of_nan)
no_block$aware_of_nan <- as.factor(no_block$aware_of_nan)
no_block$if_change_yes_no <- as.factor(no_block$if_change_yes_no)

no_block$local_or_inter <- as.factor(no_block$aware_of_nan)
no_block$gender <- as.factor(no_block$gender)
no_block$years_spent <- as.factor(no_block$years_spent)


yes_block$aware_with_nanY <- as.factor(yes_block$aware_with_nanY)
yes_block$aware_with_nanY <- as.factor(yes_block$aware_with_nanY)
yes_block$local_or_inter <- as.factor(yes_block$local_or_inter)
yes_block$gender <- as.factor(yes_block$gender)
yes_block$years_spent <- as.factor(yes_block$years_spent)


summary(yes_block)
summary(no_block)



View(yes_block)
View(no_block)
mean_overall_satisfaction_yes_block <- mean(yes_block$nan_overall_expY_1)
mean_overall_satisfaction_yes_block


mean_overall_satisfaction_no_block <- mean(no_block$satis_on_experience_1)
mean_overall_satisfaction_no_block



# T.TEST
# T.test for overall satisfaction


t.test_overall_experience <- t.test(yes_block$nan_overall_expY_1, no_block$satis_on_experience_1, alternative = "two.sided")
t.test_overall_experience


new_data3$No_or_YesBlock_1 <- as.numeric(new_data3$No_or_YesBlock_1)
class(new_data3$No_or_YesBlock_1)


new_data3 <- new_data3 %>%
    mutate(
        No_or_YesBlock_1 = ifelse(No_or_YesBlock_1 <= 3, "No", "Yes")
    )

View(new_data3)


yes_block$nan_overall_expY_1

new_data3 <- new_data3 %>%
    mutate(
        overall_student_satisfaction = ifelse(No_or_YesBlock_1 == "Yes", nan_overall_expY_1, satis_on_experience_1)
    )

new_data3$overall_student_satisfaction <- as.numeric(new_data3$overall_student_satisfaction)
new_data3$overall_student_satisfaction



View(new_data3)


new_data3$No_or_YesBlock_1 <- as.factor(new_data3$No_or_YesBlock_1)


linear_regression <- lm(overall_student_satisfaction ~ No_or_YesBlock_1, data = new_data3)

summary(linear_regression)

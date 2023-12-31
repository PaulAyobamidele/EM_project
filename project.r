# importing data
data <- read.csv("project.csv")
View(data)
summary(data)


head(data)
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

?t.test()

View(new_data3)


ncol(new_data3)
new_data3$No_or_YesBlock_1 <- as.numeric(new_data3$No_or_YesBlock_1)
class(new_data3$No_or_YesBlock_1)


new_data3 <- new_data3 %>%
    mutate(
        No_or_YesBlock_1 = ifelse(No_or_YesBlock_1 <= 3, "No", "Yes")
    )

View(new_data3)




new_data3 <- new_data3 %>%
    mutate(
        overall_student_satisfaction = ifelse(No_or_YesBlock_1 == "Yes", nan_overall_expY_1, satis_on_experience_1)
    )


View(new_data3)

new_data3$overall_student_satisfaction <- as.numeric(new_data3$overall_student_satisfaction)
new_data3$overall_student_satisfaction




View(new_data3)


new_data3$No_or_YesBlock_1 <- as.factor(new_data3$No_or_YesBlock_1)


# linear_regression <- lm(overall_student_satisfaction ~ No_or_YesBlock_1, data = new_data3)

linear_regression <- lm(overall_student_satisfaction ~ category_student_support, data = new_data3)


linear_regression


summary(linear_regression)



barplot(table(no_block$reson_on_lack), main = "Frequency of Reasons for Low Engagement", col = "#0eeb6a", xlab = "Reason", ylab = "Frequency")


boxplot(nan_overall_expY_1 ~ aware_with_nanY, data = yes_block, main = "Boxplot of Overall Satisfaction by Awareness", col = c("orange", "green"))



cor_matrix <- cor(yes_block[, c("stress_manageY_1", "Satisf_NA_1", "time_manageY_1", "satis_with_communiY_1", "nan_grades_1", "nan_overall_expY_1")])
corrplot::corrplot(cor_matrix, method = "circle")



summary(linear_regression)


head(new_data3)


# Variable names

print(names(new_data3))

View(new_data3)

ncol(new_data3)
names(new_data3) <- c("consent", "category_student_support", "satisfaction_non-academic-support", "stress_level_support", "time_management_support", "satisfaction_communication_channels", "awareness_support_services", "grade_support_services", "Ysatisfaction_overall_academic_experience", "suggest_new_support", "new_support_suggestion", "reason_for_lack_of_engagement", "other_reasons_for_lack", "will_engagement_impact_performance", "Nsatisfaction_overall_academic_experience", "satisfaction_academic_performance", "awareness_non-academic-support", "prospect_of_engagement_academic_improvement", "suggestion_new_academic_support", "age", "gender", "years_spent", "local_or_inter", "school_department", "major", "overall_student_satisfaction")


print(names(new_data3))

View(new_data3)


cleaned_data <- new_data3[, c(2, 21:26)]
View(cleaned_data)

cleaned_data <- cleaned_data %>%
    mutate(
        local_or_inter = case_when(
            grepl("local", local_or_inter, ignore.case = TRUE) ~ "Local",
            grepl("international", local_or_inter, ignore.case = TRUE) ~ "International"
        )
    )


View(cleaned_data)

cleaned_data2 <- cleaned_data %>%
    group_by(gender, category_student_support, years_spent) %>%
    summarise(
        mean_satisfaction = mean(overall_student_satisfaction),
        median_satisfaction = median(overall_student_satisfaction),
        min_satisfaction = min(overall_student_satisfaction),
        max_satisfaction = max(overall_student_satisfaction),
        count = n()
    )

View(cleaned_data2)


cleaned_data3 <- cleaned_data %>%
    group_by(gender, category_student_support) %>%
    summarise(
        mean_satisfaction = mean(overall_student_satisfaction),
        count = n()
    )


View(cleaned_data3)


cleaned_data4 <- cleaned_data %>%
    group_by(local_or_inter, category_student_support) %>%
    summarise(
        mean_satisfaction = mean(overall_student_satisfaction),
        count = n()
    )


View(cleaned_data4)




# Plotting
library(ggplot2)

ggplot(cleaned_data2, aes(x = gender, y = mean_satisfaction, fill = category_student_support)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = "Mean Satisfaction by Gender and Category of Student Support", x = "Gender", y = "Mean Satisfaction")


ggplot(cleaned_data, aes(x = years_spent, y = overall_student_satisfaction)) +
    geom_boxplot() +
    labs(title = "Satisfaction by Years Spent", x = "Years Spent", y = "Overall Satisfaction")


ggplot(cleaned_data2, aes(x = count, y = mean_satisfaction)) +
    geom_point() +
    labs(title = "Scatter Plot of Overall Satisfaction against Count of Observations", x = "Count", y = "Mean Satisfaction") +
    geom_smooth(
        method = "lm", level = 0.97
    )


ggplot(cleaned_data4, aes(x = local_or_inter, y = mean_satisfaction, color = category_student_support, fill = category_student_support)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(
        title = "Barplot of the Mean Satisfaction against Category of Student",
        x = "Local or International Student (Student Category)",
        y = "Mean Satisfaction"
    )



# 1. T-Test for Overall Satisfaction
# To examine the difference in academic satisfaction between students receiving non-academic support and those not receiving such support, a Welch Two-Sample t-test were conducted to assess the differences in overall satisfaction scores between two groups: those who received support services ("yes_block") and those who did not ("no_block").The analysis revealed a statistically significant difference in overall satisfaction scores between the two groups (t = 2.5257, df = 30.067, p-value = 0.01705).The mean overall satisfaction score for the support group was 5.22, while the mean for the non-support group was 4.27. This result suggests that the provision of support services has a discernible impact on overall student satisfaction.


# 2. Linear Regression Analysis

# A linear regression model was employed to explore the relationship between overall student satisfaction and the category of student support. The results indicate that the category of student support significantly predicts overall student satisfaction (p-value = 0.00493). The model suggests that, on average, students receiving support services exhibit a higher overall satisfaction score (intercept = 4.2727, category_student_supportYes coefficient = 0.9514). The adjusted R-squared value of 0.08539 indicates that the model explains approximately 8.54% of the variance in overall satisfaction.


# The findings suggest that the provision of support services significantly influences overall student satisfaction. Moreover, the linear regression model highlights the predictive power of the category of student support in explaining variations in satisfaction scores. These insights underscore the importance of targeted support services in enhancing the overall academic experience.

data <- read.csv("project.csv")
View(data)


data <- data[-c(1, 2), ]
View(data)


library(dplyr)
# deleting unnecessery colums
data <- data %>%
    select(
        -StartDate, -EndDate, -Status, -IPAddress, -Progress, -Duration..in.seconds.,
        -Finished, -RecordedDate, -ResponseId, -RecipientLastName, -RecipientFirstName,
        -RecipientLastName, -RecipientEmail, -ExternalReference, -LocationLatitude,
        -LocationLongitude, -DistributionChannel, -UserLanguage
    )

# View(data)


na_count <- sum(is.na(data$scl_dpt))
na_count
empty_string_count <- sum(data$scl_dpt == "")
empty_string_count

# deleting evry row that has an empty string in its scl dpt
data <- subset(data, scl_dpt != "")
# View(data)


View(data)

print(names(data))




data <- data %>%
    mutate(
        Category_Yes_No = ifelse(No_or_YesBlock_1 <= 3, "No", "Yes")
    )


View(data)


data <- data %>%
    select(
        consent, No_or_YesBlock_1, Category_Yes_No, everything()
    )

View(data)

data <- data %>%
    mutate(
        overall_student_satisfaction = ifelse(
            No_or_YesBlock_1 > 3, nan_overall_expY_1, satis_on_experience_1
        )
    )

View(data)

ncol(data)
# Change Variable Name for readability

names(data) <- c("consent", "category_student_support", "Category_Yes_No", "satisfaction_non-academic-support", "stress_level_support", "time_management_support", "satisfaction_communication_channels", "awareness_support_services", "grade_support_services", "Ysatisfaction_overall_academic_experience", "suggest_new_support", "new_support_suggestion", "reason_for_lack_of_engagement", "other_reasons_for_lack", "will_engagement_impact_performance", "Nsatisfaction_overall_academic_experience", "satisfaction_academic_performance", "awareness_non_academic_support", "prospect_of_engagement_academic_improvement", "suggestion_new_academic_support", "age", "gender", "years_spent", "local_or_inter", "school_department", "major", "overall_student_satisfaction")

ncol(data)
View(data)


students_with_support <- data[data$No_or_YesBlock_1 > 3, ]$Ysatisfaction_overall_academic_experience
students_with_no_support <- data[data$No_or_YesBlock_1 <= 3, ]$Nsatisfaction_overall_academic_experience


data$Ysatisfaction_overall_academic_experience <- as.numeric(data$Ysatisfaction_overall_academic_experience)
data$Nsatisfaction_overall_academic_experience <- as.numeric(data$Nsatisfaction_overall_academic_experience)


students_with_support <- data$Ysatisfaction_overall_academic_experience
students_with_no_support <- data$Nsatisfaction_overall_academic_experience




# First Inferential Test
# T.test

t.test_overall_experience <- t.test(students_with_support, students_with_no_support, alternative = "two.sided")
t.test_overall_experience



# t.test_overall_experience

#         Welch Two Sample t-test

# data:  students_with_support and students_with_no_support
# t = 2.5257, df = 30.067, p-value = 0.01705
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#  0.1821807 1.7206407
# sample estimates:
# mean of x mean of y
#  5.224138  4.272727




t.test_overall_performance <- t.test(as.numeric(data$grade_support_services), as.numeric(data$satisfaction_academic_performance), alternative = "two.sided")
t.test_overall_performance
# t.test_overall_performance

#  Welch Two Sample t-test

# data:  as.numeric(data$grade_support_services) and as.numeric(data$satisfaction_academic_performance)
# t = -0.41019, df = 35.384, p-value = 0.6841
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#  -1.0160662  0.6743734
# sample estimates:
# mean of x mean of y
#  3.965517  4.136364


# Second Inferential Test
# Linear Regression


View(data)


data$category_student_support <- as.numeric(data$category_student_support)



linear_regression <- lm(overall_student_satisfaction ~ category_student_support, data = data)


summary(linear_regression)


# Descriptive Analysis

View(data)
data$reason_for_lack_of_engagement <- gsub("Lack of awareness about available services", "Unawareness of Services", data$reason_for_lack_of_engagement)

data$reason_for_lack_of_engagement <- gsub("Limited perceived need for non-academic support", "Low Support Need", data$reason_for_lack_of_engagement)

category_no <- data[data$Category_Yes_No == "No", ]
reason_no_support <- category_no$reason_for_lack_of_engagement
reason_no_support

reason_no_support <- table(reason_no_support)

reason_no_support
?gsub()


barplot(reason_no_support, main = "Frequency of Reasons for Low Engagement", col = "#0eeb6a", xlab = "Reason", ylab = "Frequency")


boxplot(Ysatisfaction_overall_academic_experience ~ awareness_support_services, data = data, main = "Boxplot of Overall Satisfaction by Awareness", col = c("orange", "green", xlab = "Students' Awareness of Support Services", ylab = "Overall Academic Experience"))



names(data)

data <- data %>%
    mutate(
        combined_awareness = ifelse(Category_Yes_No == "Yes", awareness_support_services, awareness_non_academic_support)
    )


data$combined_awareness <- as.factor(data$combined_awareness)
data$overall_student_satisfaction <- as.numeric(data$overall_student_satisfaction)


View(data)
boxplot(overall_student_satisfaction ~ combined_awareness, data = data, main = "Boxplot of Overall Satisfaction by Awareness", col = c("orange", "green"), xlab = "Students Category of Support Awareness", ylab = "Overall Academic Satisfaction")


cleaned_data <- data[, c(1:3, 21:29)]

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
    group_by(gender, Category_Yes_No, years_spent) %>%
    summarise(
        mean_satisfaction = mean(overall_student_satisfaction),
        median_satisfaction = median(overall_student_satisfaction),
        min_satisfaction = min(overall_student_satisfaction),
        max_satisfaction = max(overall_student_satisfaction),
        count = n()
    )

View(cleaned_data2)


cleaned_data3 <- cleaned_data %>%
    group_by(gender, Category_Yes_No) %>%
    summarise(
        mean_satisfaction = mean(overall_student_satisfaction),
        count = n()
    )


View(cleaned_data3)


cleaned_data4 <- cleaned_data %>%
    group_by(local_or_inter, Category_Yes_No) %>%
    summarise(
        mean_satisfaction = mean(overall_student_satisfaction),
        count = n()
    )


View(cleaned_data4)




# Plotting
library(ggplot2)

ggplot(cleaned_data3, aes(x = gender, y = mean_satisfaction, fill = Category_Yes_No)) +
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


ggplot(cleaned_data4, aes(x = local_or_inter, y = mean_satisfaction, fill = Category_Yes_No, color = Category_Yes_No)) +
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



View(data)


names(data)




data$grade_support_services
data$satisfaction_academic_performance


data <- data %>%
    mutate(
        combined_grade = ifelse(Category_Yes_No == "Yes", grade_support_services, satisfaction_academic_performance)
    )


cleaned_data5 <- cleaned_data %>%
    group_by(
        grade_support_services
    ) %>%
    summarise(
        mean = mean(grade_support_services)
    )


cleaned_data5



ggplot(data, aes(x = grade_support_services, y = satisfaction_academic_performance, fill = satisfaction_academic_performance)) +
    geom_bar(position = "dodge", stat = "identity")


?geom_bar()



# Shapiro-Wilk Test for students_with_support
shapiro.test(students_with_support)

# Shapiro-Wilk Test for students_with_no_support
shapiro.test(students_with_no_support)

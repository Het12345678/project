#' Test 2- t test
#' @name test2
#' @description This function is useful to conduct a t test
#' @param file_name is the dataset that will be used for this test
#'
#' @return Returns hypothesis, assumptions, statistical analysis results, decision, conclusion
#' @export
#' @examples
#' data(project)
#' file_name <- project
#' test2(file_name)

data(project)
file_name <- project

test2 <- function(file_name){

  b <- file_name %>%
    dplyr::select("gender", "height") #this filters the dataset to only have the 'height' and 'weight' columns

  test2_male <- b %>%
    dplyr::filter(gender == "Male")

  test2_female <- b %>%
    dplyr::filter(gender == "Female")

#Stating the hypotheses:
hypothesis2 <- function(b) {
  cat("Null Hypothesis: Mu1 is equal to Mu2 \n", #this stores the text written in the function 'hypothesis'
      "Alternative Hypothesis: Mu1 is not equal to Mu2 \n",
      "Mu1 is the mean for sample 1 (male) and Mu2 is the mean for sample 2 (female)")
}

#Proving the Assumptions:
assumptions2 <- function(b){ #this reads the file that is inputted in the function
  t2_p1 <- ggpubr::ggqqplot(b, x = "height", facet.by = "gender" ) + ggplot2::ggtitle("Proving Normality of the Data")
  t2_p2 <- ggplot2::ggplot(b, ggplot2::aes(x = gender, y = height)) +
    ggplot2::geom_boxplot() + #this plots the boxplot to prove equal variances
    ggplot2::ggtitle("Proving Equal Variance") #this is the title of the plot#this plots a two graphs to prove the normality of the data
  par(mfrow = c(2,2))
  cowplot::plot_grid(t2_p1, t2_p2, nrow=2)
}

#Statistical Analysis:
analysis2 <- function(b) {
  t_test <- t.test(test2_male$height, test2_female$height, var.equal = TRUE)
  t_tvalue <- t_test$statistic
  t_pvalue <- t_test$p.value
  t_df <- t_test$parameter
  t_ci <- t_test$conf.int
  dataframe2 <- data.frame(Parameter = c("t value", "p value", "Degrees of Freedom","Lower CI", "Upper CI"), Value = c(t_tvalue, t_pvalue, t_df, t_ci)) #this puts the results obtained above into a data frame format (table)
  return(dataframe2)
}
test2_result <- analysis2(b)

#Decision of the test:

decision2 <- function(analysis2){
  if (analysis2$Value[2] > 0.05) { #if statement that is testing whether the p-value (the 3rd value in  the 'test1' function above), is greater than the significance level of 0.05
    cat("As the P-value is greater than 0.05, at 95% significance level, the null hypothesis is not rejected") #if the p-value is greater than 0.05, then print this statement
  } else {
    cat("As the P-value is less than 0.05, at 95% significance level, the null hypothesis is rejected") #otherwise, if the p-value is less than 0.05, then print this statement
  }
}

#Conclusion to the test:
conclusion2 <- function(analysis2){
  if (analysis2$Value[2] > 0.05) { #if statement that is testing whether the p-value (the 3rd value in  the 'test1' function above), is greater than the significance level of 0.05
    print("As the P-value is greater than 0.05, there is not enough evidence to reject the null hypothesis. Therefore, the mean height of male and female are equal") #as the p-value is greater than 0.05, print this statement
  } else {
    print("As the P-value is less than 0.05, there is enough evidence to reject the null hypothesis. Therefore, the mean height of male and female are not equal") #alternatively, as the p-value is less than 0.05, print this statement
  }
}
print(hypothesis2(b)) #this reveals the hypothesis for test 1
print(assumptions2(b))
print(test2_result)
print(decision2(test2_result)) #this is the decision for the first test
print(conclusion2(test2_result)) #this is the conclusion for test1
}
test2(file_name)


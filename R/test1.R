#' Test 1-lm test
#' @name test1
#' @description This function is useful to conduct a lm test
#' @param file_name is the dataset that will be used for this test
#'
#' @return Returns hypothesis, assumptions, statistical analysis results, decision, conclusion
#' @export
#' @examples
#' data(project)
#' file_name <- project
#' test1(file_name)

library(magrittr)

data(project) #this loads the data (project2022 data) that is already saved into the package
file_name <- project

test1 <- function(file_name){

  a <- file_name %>%
    dplyr::select("height", "weight") #this filters the dataset to only have the 'height' and 'weight' columns

  #Stating the hypotheses:
  hypothesis1 <- function(a) {
    cat("Null Hypothesis: Beta is equal to 0 \n", #this stores the text written in the function 'hypothesis'
        "Alternative Hypothesis: Beta is not equal to 0 \n",
        "Beta is the true slope parameter as in the model Y = alpha + beta X + epsilon")
  }

  #Proving the assumptions:
  assumptions1 <- function(a){ #this reads the file that is inputted in the function
    residuals1 <- lm(formula = height ~ weight, data = a)$residuals #this creates a simple regression model
    par(mfrow = c(2,2)) #this makes the 3 plots that will be created appear in a 2x2 format so that 2 plots will be in the top row and 1 plot will be in the bottom row
    plot(x = a, main = "height vs weight") #this plots  height vs weight
    plot(x = residuals1, main = "Scatter Plot of Regression Residuals vs Fitted Values", xlab = "Fitted Values", ylab = "Regression Residuals") #this plots the scatter plot
    hist(x = residuals1, main = "Histogram of the Regression Residuals", xlab = "Residuals") #this plots the histogram
  }

  #Statistical Analysis:
  analysis1 <- function(a) {
    slope <- lm(formula = height ~ weight, data = a)$coefficients[2] #this calculates the estimate of the slope
    tvalue <-  summary(lm(formula = height ~ weight, data = a))$coefficients[6] #this calculates the t-value
    pvalue <- summary(lm(formula = height ~ weight, data = a))$coefficients[8] #this calculates the p-value
    df <- lm(formula = height ~ weight, data = a)$df.residual #this calculates the degrees of freedom
    ci <- confint.lm(lm(formula = height ~ weight, data = a), level = 0.95)[c(2,4)] ##this calculates the confident interval
    dataframe1 <- data.frame(Parameter = c("Estimated slope", "t value", "p value", "Degrees of Freedom","Lower CI", "Upper CI"), Value = c(slope, tvalue, pvalue, df, ci)) #this puts the results obtained above into a data frame format (table)
    return(dataframe1)
  }
  test1_result <- analysis1(a)

  #Decision of the test:
  decision1 <- function(analysis1){
    if (analysis1$Value[3] > 0.05) { #if statement that is testing whether the p-value (the 3rd value in  the 'test1' function above), is greater than the significance level of 0.05
      cat("As the P-value is greater than 0.05, at 95% significance level, the null hypothesis is not rejected") #if the p-value is greater than 0.05, then print this statement
    } else {
      cat("As the P-value is less than 0.05, at 95% significance level, the null hypothesis is rejected") #otherwise, if the p-value is less than 0.05, then print this statement
    }
  }

  #Conclusion to the test:
  conclusion1 <- function(analysis1){
    if (analysis1$Value[3] > 0.05) { #if statement that is testing whether the p-value (the 3rd value in  the 'test1' function above), is greater than the significance level of 0.05
      cat("As the P-value is greater than 0.05, there is not enough evidence to reject the null hypothesis. Therefore, there is not a linear relationship between height and weight") #as the p-value is greater than 0.05, print this statement
    } else {
      cat("As the P-value is less than 0.05, there is enough evidence to reject the null hypothesis. Therefore, there is a linear relationship between height and weight") #alternatively, as the p-value is less than 0.05, print this statement
    }
  }

  print(hypothesis1(a))
  assumptions1(a) #this reveals the plots for the assumptions
  print(test1_result) #this prints the results obtained
  print(decision1(test1_result)) #this is the decision for the first test
  print(conclusion1(test1_result)) #this is the conclusion for test1
}
test1(file_name) #this outputs all sections of the first test


#' Test 3-chi test
#' @name test3
#' @description This function is useful to conduct a chi test
#' @param file_name is the dataset that will be used for this test
#'
#' @return Returns hypothesis, assumptions, statistical analysis results, decision, conclusion
#' @export
#' @examples
#' data(project)
#' file_name <- project
#' test3(file_name)

data(project)
file_name <- project

test3 <- function(file_name){

  c <- file_name %>%
    dplyr::select("gender", "phys") #this filters the dataset to only have the 'height' and 'weight' columns

  female_none <- length(which(c$gender=="Female"&c$phys=="None"))
  female_none
  female_moderate <- length(which(c$gender=="Female"&c$phys=="Moderate"))
  female_moderate
  female_intense <- length(which(c$gender=="Female"&c$phys=="Intense"))
  female_intense

  male_none <- length(which(c$gender=="Male"&c$phys=="None"))
  male_none
  male_moderate <- length(which(c$gender=="Male"&c$phys=="Moderate"))
  male_moderate
  male_intense <- length(which(c$gender=="Male"&c$phys=="Intense"))
  male_intense

  gender_phys <- matrix(c(female_none, male_none, female_moderate, male_moderate, female_intense,male_intense), ncol=2,byrow=TRUE)
  colnames(gender_phys) <- c("Female", "Male")
  rownames(gender_phys) <- c("None","Moderate","Intense")
  gender_phys <- as.table(gender_phys)
  gender_phys

#Stating the hypotheses:
hypothesis3 <- function(c) {
  cat("Null Hypothesis: Gender and amount of physical activity are independant of each other \n", #this stores the text written in the function 'hypothesis'
      "Alternative Hypothesis: Gender and amount of physical activity are dependant on each other \n")
}

#Proving the assumptions:

assumptions3 <- function(c){ #this reads the file that is inputted in the function
  none_total <- gender_phys[1,1] + gender_phys[1,2]
  moderate_total <- gender_phys[2,1] + gender_phys[2,2]
  intense_total <- gender_phys[3,1] + gender_phys[3,2]
  female_total <- gender_phys[1,1] + gender_phys[2,1] + gender_phys[3,1]
  male_total <- gender_phys[1,2] + gender_phys[2,2] + gender_phys[3,2]
  total_total <- female_total + male_total

  r1_col1 <- (none_total*female_total)/(total_total)
  r2_col1 <- (moderate_total*female_total)/(total_total)
  r3_col1 <- (intense_total*female_total)/(total_total)
  r1_col2 <- (none_total*male_total)/(total_total)
  r2_col2 <- (moderate_total*male_total)/(total_total)
  r3_col2 <- (intense_total*male_total)/(total_total)

  expected_values <- matrix(c(r1_col1, r1_col2, r2_col1, r2_col2, r3_col1, r3_col2), ncol=2,byrow=TRUE)
  colnames(expected_values) <- c("Female", "Male")
  rownames(expected_values) <- c("None","Moderate","Intense")
  expected_values <- as.table(expected_values)
  return(expected_values)
}

#Statistical Analysis:
analysis3 <- function(c) {
  chitest <- chisq.test(gender_phys)
  xvalue <- chitest$statistic
  chi_pvalue <- chitest$p.value
  chi_df <- chitest$parameter
  dataframe3 <- data.frame(Parameter = c("x value", "p value","Degrees of Freedom"), Value = c(xvalue, chi_pvalue, chi_df)) #this puts the results obtained above into a data frame format (table)
  return(dataframe3)
}
test3_result <- analysis3(c)


#Decision of the test:
decision3 <- function(analysis3){
  if (analysis3$Value[2] > 0.05) { #if statement that is testing whether the p-value (the 3rd value in  the 'test1' function above), is greater than the significance level of 0.05
    cat("As the P-value is greater than 0.05, at 95% significance level, the null hypothesis is not rejected") #if the p-value is greater than 0.05, then print this statement
  } else {
    cat("As the P-value is less than 0.05, at 95% significance level, the null hypothesis is rejected") #otherwise, if the p-value is less than 0.05, then print this statement
  }
}

#Conclusion to the test:
conclusion3 <- function(analysis3){
  if (analysis3$Value[2] > 0.05) { #if statement that is testing whether the p-value (the 3rd value in  the 'test1' function above), is greater than the significance level of 0.05
    print("As the P-value is greater than 0.05, there is not enough evidence to reject the null hypothesis. Therefore, gender and amount of physical activity are independant of each other") #as the p-value is greater than 0.05, print this statement
  } else {
    print("As the P-value is less than 0.05, there is enough evidence to reject the null hypothesis. Therefore, gender and amount of physical activity are dependant on each other") #alternatively, as the p-value is less than 0.05, print this statement
  }
}

print(hypothesis3(c))
print(assumptions3(c))
print(test3_result)
print(decision3(test3_result)) #this is the decision for the first test
print(conclusion3(test3_result)) #this is the conclusion for test1
}
test3(file_name)


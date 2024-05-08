#' Will get the confidence interval of the percentage of students that pass
#'
#' @param marks A vector containing the marks of all the students
#' @param percentage The percentage we are interested in
#' @param max_mark The maximum possible mark on the test
#'
#' @return The confidence interval for the percentage
#' @export

getPerCI <- function(marks, percentage, max_mark)
{
  UseMethod("getPerCI")
}

#'@export

getPerCI.default <- function(marks, percentage, max_mark)
{
  per_mark <- percentage*max_mark/100
  count <- length(marks[marks>=per_mark])
  per_pass <- count/length(marks)*100
  se <- sd(c(rep(1, count), rep(0, length(marks)-count)))/sqrt(length(marks))*100
  return(c(per_pass - 1.96*se, per_pass + 1.96*se))
}


#' Will get the confidence interval of the average mark achieved
#'
#' @param marks A vector containing the marks of all the students
#'
#' @return The confidence interval for average mark
#'@export

getAvCI <- function(marks)
{
  UseMethod("getAvCI")
}

#'@export

getAvCI.default <- function(marks)
{
  return(c(mean(marks)- 1.96*sd(marks)/sqrt(length(marks)), mean(marks) + 1.96*sd(marks)/sqrt(length(marks))))
}

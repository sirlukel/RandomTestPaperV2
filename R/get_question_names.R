#' Will get the names of each of the questions in a test
#'
#' @param t The test that the question names should be extracted from
#'
#' @return A vector of all the question names from the test
#' @export
#'

get_question_names <- function(t)
{
  UseMethod('get_question_names')
}

#'@export

get_question_names.default <- function(t)
{
  question_names <- c()
  for(i in t$questions)
  {
    question_names <- c(question_names, i$name)
  }
  return(question_names)
}

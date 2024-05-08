#' Creates a new object with class 'student'
#'
#' @param answer A list of option numbers that the student will use as the answer to the question
#' @param marks The current amount of marks that the student has
#' @param ability a numeric value giving the ability of the students
#'
#' @return A new Student class object
#' @export

new_student <- function(answer, marks, ability){
  structure(list(given_answer = as.integer(answer),total_marks = as.numeric(marks), ability = ability), class = "student")
}

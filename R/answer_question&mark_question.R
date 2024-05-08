#' Given a student and a question, gives a random answer to the question from the student
#'
#' @param s A student that will answer the question
#' @param q A question that the student will answer
#'
#' @return A new student with the updated marks from the student answering the question
#' @export

answer_question <- function(s, q){
  UseMethod("answer_question")
}

#'@export

answer_question.default <- function(s, q){
  #If the student knows the number of correct options they will only select that many else they will select a random number of choices
  if(q$given_no_correct){
    #If partial scores are available, the student will not select more than that many options, but may not select the full number to avoid getting an incorrect answer
    #Will still consider this as may want to change how this works
    if(q$partial_score){
      choice <- sample(1:q$option_num, size = sample(1:length(q$correct), size = 1),replace = F)
    }else{
      choice <- sample(1:q$option_num, size = length(q$correct),replace = F)
    }
  }else{
    choice <- sample(1:q$option_num, size = sample(1:q$option_num,1),replace = F)
  }
  s <- new_student(sort(choice), s$total_marks, s$ability)
  #marks the question once choices are selected
  s<- mark_question(s,q)
  return(s)
}




#' Mark a question that a student has answered
#'
#' @param s A student that has an answer to the question given
#' @param q A question that is being marked with the students answers
#'
#' @return A student with the marks updated based on if the answer was correct
#' @export

mark_question <- function(s, q){
  UseMethod("mark_question")
}

#'@export

mark_question.default <- function(s, q){
  added_marks<-0
  if(q$partial_score == TRUE){
    #If we are allowed partial marks we add score for each correctly chosen option
    #currently you can get marks for each correct option but get the question 'wrong' if you select any wrong options. This may change.
    for (i in s$given_answer){
      if (i %in% q$correct) {added_marks <- (added_marks+ as.numeric(q$correct_score))}
      else{added_marks<- as.numeric(q$wrong_loss)
      break
      }
    }
  }
  else
  {
    #If partial marks are not allowed we only credit marks if all choices are selected and no additional options are chosen
    if (length(q$correct) == length(s$given_answer)){
      all_right <- T
      for (i in s$given_answer){
        if (!(i %in% q$correct)) {
          all_right <- F
          break
        }
      }
      if(all_right){
        added_marks <- as.numeric(q$correct_score)
      }else{added_marks <- as.numeric(q$wrong_loss)}
    }
    else{ added_marks<- as.numeric(q$wrong_loss)}
  }
  s <- new_student(s$given_answer, s$total_marks + added_marks, s$ability)
  return(s)
}

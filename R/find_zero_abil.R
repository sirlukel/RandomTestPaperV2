
#'@export
find_zero_abil <- function(q){
  UseMethod("find_zero_abil")
}

#'@export
find_zero_abil.default <- function(q){
  if(!q$partial_score & q$wrong_loss == 0)
    s <- new_student(0,0,0)
    for(i in 1:1000){
    s <- answer_question(s,q)
    }
  prob <- s$total_marks/(q$correct_score*1000)
  return(prob)
}


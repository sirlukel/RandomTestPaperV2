#'Creates a new object with class question
#'
#'@param option_num an integer representing the number of options the question has
#'@param correct_score a numeric value representing how many marks you are awarded for answering the question correctly
#'@param wrong_loss a numeric value representing how many marks you are awarded for answering the question incorrectly. Usually a negative value
#'@param partial_score a logical value stating if a student should be awarded a portion of the marks for choosing some correct choices, but no incorrect choices
#'@param given_no_correct a logical value stating if the student is told how many answers to the question are correct
#'@param correct a list of integers giving the number of each correct answer. There should be no repeated numbers and no number higher than option_num
#'
#'@return A question type object
#'@export

new_question <- function(name,option_num, correct_score, wrong_loss, partial_score, given_no_correct,  difficulty, discrimination, correct){
  #Updates score per correct answer if you are allowed partial marks
  if (partial_score == TRUE) {correct_score <- as.numeric(correct_score)/length(correct)}
  structure(list(name = name, option_num = option_num,correct_score = correct_score, wrong_loss = wrong_loss, partial_score = partial_score, given_no_correct = given_no_correct, difficulty = difficulty, discrimination = discrimination, random_guess = -1, correct= sort(correct)), class = "question")
}



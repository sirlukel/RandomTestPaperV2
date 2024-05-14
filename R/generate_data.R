Depends: dplyr

#' Given a question will calculate an estimate of the probability of getting the question correct through random guessing
#'
#' @param q A question
#' @param n A integer giving the sample size to use for the estimation
#'
#' @return A probability of getting the question correct through random guessing
#' @export

find_no_abil <- function(q,n){
  UseMethod("find_no_abil")
}

#' @export
find_no_abil.default <- function(q, n){
  q$wrong_loss <- 0
  if(!q$partial_score)
    s <- new_student(0,0,0)
  for(i in 1:n){
    s <- answer_question(s,q)
  }
  prob <- s$total_marks/(q$correct_score*n)
  return(prob)
}

#' Given a student and a test will simulate the result the student acheived when answering the question
#'
#' @param s A student that will be answering the question
#' @param q A question that will be answered by the student
#'
#' @return The simulated mark achieved by the student
#' @export

gen_res_from_mod <- function(s, q){
  mark <- 0
  if(!q$partial_score){
    a <- q$discrimination
    b <- q$difficulty
    if(q$random_guess == -1){
      c <- as.numeric(find_no_abil(q,10000))
    }else{
      c <- as.numeric(q$random_guess)
    }
    abil <- s$ability
    p <- c + (1-c)*1/(1+exp(-1.7*a*(abil-b)))
    if(runif(1) < p){
      mark <- q$correct_score
    }else{
      mark <- q$wrong_loss
    }
  }
  return(mark)
}

#' Given a test and a student will simulate the results the student received
#'
#' @param t The test to be answered by the student
#' @param s The student to take the test
#' @param sname A name used to identify the student
#'
#' @return A data frame containing the results from the student taking the test
#' @export
#'
gen_test_res_from_mod <- function(t,s, sname)
{
  n <- length(t$questions)
  stname <- rep(sname, n)
  qname <- c()
  ability <- rep(s$ability, n)
  scores <- c()
  diff <- c()
  disc <- c()
  cs <- c()
  for(i in 1:n){
    q <- t$questions[[i]]
    scores <- c(scores,gen_res_from_mod(s,q))
    qname <- c(qname, q$name)
    diff <- c(diff, q$difficulty)
    disc <- c(disc, q$discrimination)
    if(q$random_guess == -1){
      cs <- c(cs,as.numeric(find_no_abil(q,10000)))
    }else{
      cs <- c(cs,q$random_guess)
    }
  }

  return(data.frame(student_name = stname, ability = ability,question_name = qname, difficulty = diff, discrimination = disc, random_chance = cs, score = scores))
}

#' Given a test and a vector of students, will simulate the results the students received
#'
#' @param t The test to be answered by the students
#' @param students A vector of students to take the test or the number of students to be generated
#' @param ran_type 'u' or 'n', the distribution of abilities of students if you are generating students
#' @param need_rand Boolean - do we need to find the random probability of getting the question correct for the questions on the test
#'
#' @return A data frame containing the results from the student taking the test
#' @export
#'
gen_student_data <- function(t, students, ran_type = 'u', need_rand = F){
  if (is.numeric(students)){
    if(ran_type == 'u'){
      abilities <- runif(students, -5,5)
    }else if(ran_type == 'n'){
      abilities <- rnorm(students, 0, 1)
    }
    students <- list()
    for (i in 1:length(abilities)){
      students[[i]] <- new_student(0,0,abilities[i])
    }
  }
  if (need_rand){
    t <- find_test_qs_rand(t)
  }
  student_data <- data.frame()
  for (i in 1:length(students)){
    student_data <- bind_rows(student_data,gen_test_res_from_mod(t, students[[i]], paste("Student", i, sep = " ")))
  }
  return(student_data)
}

#' Given a test will find the probabity of getting each question correct through random guessing
#'
#' @param t The test to be updated
#'
#' @return The updated test
#' @export
#'

find_test_qs_rand <- function(t){
  updated_qs <- list()
  for (i in 1:length(t$questions)){
    q <- t$questions[[i]]
    q$random_guess <- find_no_abil(q,10000)
    updated_qs[[i]] <- q
  }
  check <- FALSE
  if (t$min_marks <0){
    check <- TRUE
  }
  return(new_test(updated_qs, t$name, check))
}

#' Will generate a random question
#'
#' @param name The name of the question
#' @param partial Whether the question can have partial marks
#' @param min_neg The most points that can be subtracted for getting the question incorrect
#' @param max_opt The maximum number of options for the question
#' @param max_score The highest score that can be achieved on the question for answering correctly
#' @param set_disc Used to force a specific discrimination value
#' @param set_diff Used to force a specific difficulty value
#'
#'
#' @return The generated question
#' @export
#'

gen_ran_question <- function(name, partial, min_neg, max_opt, max_score, set_disc = NA, set_diff = NA){
  option_num <- sample(2:max_opt,1)
  correct_score <- sample(1:max_score, 1)
  wrong_loss <- sample(-min_neg:0,1)
  given_no_correct <- sample(c(T,F),1)

  if(is.na(set_diff)){
    diff <- runif(1,-5,5)
  }else {diff <- set_diff}

  if(is.na(set_disc)){
    disc <- runif(1,0,5)
  }else{disc <- set_disc}

  no_cor <- sample(1:option_num,1)
  correct <- sample(1:option_num, no_cor)
  new_q <- new_question(name, option_num, correct_score, wrong_loss, partial, given_no_correct, diff, disc, correct)
  new_q$random_guess <- find_no_abil(new_q, 10000)
  return(new_q)
}

#' Will generate a test
#'
#'
#' @param n The number of questions on the test
#' @param name The name of the test
#' @param can_neg Boolean - whether you allow for a negative mark to be achieved overall on the test
#' @param dupe How many unique questions should the test have (-1 for all unique)
#'
#'
#' @return The generated test
#' @export
#'

gen_stand_ran_test <- function(n,name, can_neg, dupe = -1){
  questions <- list()
  if (dupe != -1) {
    question_rep <- list()
    for (i in 1:dupe){
      question_rep[[i]] <- gen_ran_question(paste("The_Question", i), FALSE, 0, 20,1, set_diff = rnorm(1,0,1), set_disc = runif(1,0,2))
    }
  }
  for (i in 1:n){
    if(dupe != -1){
      j = i
      while(j > dupe)
      {
        j <- j - dupe
      }
      questions[[i]] <- question_rep[[j]]
    }
    else{
      questions[[i]] <- gen_ran_question(paste("Question",i), FALSE, 0, 20,1)
    }
  }
  t <- new_test(questions, name, can_neg)
  return(t)
}

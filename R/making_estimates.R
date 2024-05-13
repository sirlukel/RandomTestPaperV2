#' Finds the log-likelihood for a score being achieved on a question
#'
#' @param ability  The ability of the student answering the question
#' @param a The discrimination of the question
#' @param b The difficutly of the question
#' @param c The chance of randomly getting the question correct
#' @param score The score achieved on the question by the student
#'
#' @return The log-likelihood
#' @export
#'
question_likelihood <- function(ability, a, b, c, score){
  if(a < 0)
  {
    return(-1000000)
  }else
  {
    p <- c +(1-c)/(1+exp(-1.7*a*(ability-b)))
    value <- ifelse(score>0, log(p), log(1-p))
    if(!is.finite(value))
    {
      value <- -1000000
    }
    return(value)
  }
}

#' Finds the log-likelihood for the scores acheived by a student on all questions
#'
#' @param ability  The ability of the student answering the questions
#' @param discs Vector of discriminations of the questions
#' @param diffs Vector of difficulties of the questions
#' @param cs Vector of the chances of randomly getting the questions correct
#' @param scores The scores achieved on the questions by the student
#'
#' @return The log-likelihood
#' @export
#'
all_questions_likelihood <- function(ability, discs, diffs, cs, scores){
  p <- 0
  repeated <- length(scores)/length(discs)
  discs <- rep(discs, repeated)
  diffs <- rep(diffs, repeated)
  cs <- rep(cs, repeated)
  p <- sum(sapply(1:length(scores), FUN = \(i) question_likelihood(ability, discs[i], diffs[i], cs[i], scores[i])))
  return(p)
}


#' Finds the log-likelihood from all students answering all questions
#'
#' @param abils  Vector of abilities of the students answering the questions
#' @param discs Vector of discriminations of the questions
#' @param diffs Vector of difficulties of the questions
#' @param cs Vector of the chances of randomly getting the questions correct
#' @param scores The scores achieved on the questions by the student
#'
#' @return The log-likelihood
#' @export
#'
all_students_likelihood <- function(abils, discs, diffs, cs, scores){
  p <- 0
  ns <- length(abils)
  repli <- length(scores)/ns
  p <- sum(sapply(1:ns, FUN = \(i) all_questions_likelihood(abils[i], discs, diffs, cs, scores[(repli*(i-1)+1):(i*repli)])))
  return(p)
}



#' Finds estimates for all abilities given question parameters
#'
#' @param discs Vector of discriminations of the questions
#' @param diffs Vector of difficulties of the questions
#' @param cs Vector of the chances of randomly getting the questions correct
#' @param scores The scores achieved on the questions by the student
#' @param ns The number of students
#' @param nq The number of unique questions
#'
#' @return A list of the optimised abilities and the total log-likelihood for these estimates
#' @export
#'
find_optimised_abilities <- function(discs, diffs, cs, scores, ns, nq){
  abils <- c()
  repli <- length(scores)/ns
  totalp <- 0
  temp <- sapply(1:ns, FUN = \(y) optimise(\(x) all_questions_likelihood(x,discs,diffs,cs,scores[(repli*(y-1)+1):(y*repli)]), interval = c(-5,5), lower = -5, upper = 5, maximum = TRUE, tol = 0.05))
  abils <- unlist(temp[1,])
  totalp <- sum(unlist(temp[2,]))
  return(list(abils = abils, totalp = totalp))
}



#' Alternate approach to optimising all the abilities using optim rather than optimise
#'
#' @param discs Vector of discriminations of the questions
#' @param diffs Vector of difficulties of the questions
#' @param cs Vector of the chances of randomly getting the questions correct
#' @param scores The scores achieved on the questions by the student
#' @param ns The number of students
#' @param nq The number of unique questions
#'
#' @return A list of the optimised abilities and the total log-likelihood for these estimates
#' @export
#'
find_optim_abilities <- function(discs, diffs, cs, scores, ns, nq){
  abils <- c()
  totalp <- 0
  temp <- optim(rep(0, ns), \(x) all_students_likelihood(x,discs,diffs,cs,scores) ,control = list(fnscale = -1))
  return(list(abils = temp$par, totalp = temp$value))
}


#' Uses random walk method with n iterations to find optimised question params and abilities
#'
#' @param cs Vector of the chances of randomly getting the questions correct
#' @param scores The scores achieved on the questions by the student
#' @param ns The number of students
#' @param nq The number of unique questions
#' @param n The number of iterations to use
#' @param tuning The standard deviation of the adjustments at each step
#' @param group Boolean - True if we should adjust all question parameters at the same time
#'
#' @return A list of the optimised estimates for the parameters
#' @export
#'
find_all_optimised_1 <- function(cs, scores, ns, nq, n, tuning = 0.01, group = T)
{
  abils <- rep(0, ns)
  discs <- c(1,rep(2.5, nq-1))
  diffs <- rep(0, nq)

  p <- find_optimised_abilities(discs, diffs, cs, scores, ns, nq)$totalp
  if(group){
    for(i in 1:n){
      temp_discs <- discs
      if(nq > 1){
        temp_discs[2:nq] <- temp_discs[2:nq] + rnorm(nq-1, mean = 0, sd = tuning)
      }
      temp_discs[temp_discs < 0] <- 0
      temp_diffs <- diffs + rnorm(nq, mean = 0, sd = tuning)
      new_p <- find_optimised_abilities(temp_discs, temp_diffs, cs, scores, ns, nq)$totalp
      if(new_p > p){
        p <- new_p
        discs <- temp_discs
        diffs <- temp_diffs
      }
    }
  }else{
    k <- n/nq
    for(i in 1:nq){
      for (j in 1:k)
      {
        temp_discs <- discs
        temp_diffs <- diffs
        if(i != 1){
          temp_discs[i] <- temp_discs[i] + rnorm(1, mean = 0, sd = tuning)
        }
        temp_diffs[i] <- temp_diffs[i] + rnorm(1, mean = 0, sd = tuning)
        new_p <- find_optimised_abilities(temp_discs, temp_diffs, cs, scores, ns, nq)$totalp
        if(new_p > p){
          p <- new_p
          discs <- temp_discs
          diffs <- temp_diffs
        }
      }
    }
  }
  abils <- find_optimised_abilities(discs, diffs, cs, scores, ns, nq)$abils
  return(list(abils = abils, discs = discs, diffs = diffs))
}


#' Uses optim to find optimised question params and abilities
#'
#' @param cs Vector of the chances of randomly getting the questions correct
#' @param scores The scores achieved on the questions by the student
#' @param ns The number of students
#' @param nq The number of unique questions
#' @param group Boolean - False if we want to maximise the profile log-likelihood, True if we want to maximise the full log-likelihood
#'
#' @return A list of the optimised estimates for the parameters
#' @export
#'
find_all_optimised_2 <- function(cs, scores, ns, nq, group = F)
{
  if(nq == 1){
    if(group){
      temp <- optim(c(0, rep(0, ns)), fn = \(x) all_students_likelihood(x[2:(ns+1)],1, x[1],cs,scores),control = list(fnscale = -1))
      diffs <- temp$par[1]
      abils <- temp$par[2:(ns+1)]
    }else{
      temp <- optimise(\(x) find_optimised_abilities(1, x ,cs,scores,ns,nq)$totalp, interval = c(-5,5), lower = -5, upper =5, maximum = T, tol = 0.05)
      diffs <- temp$maximum
      abils <- find_optimised_abilities(1, diffs, cs, scores,ns,nq)$abils
    }
    results <- list(abils = abils, discs = 1, diffs = diffs)
  }
  else{
    if (!group){
      temp <- optim(c(rep(2.5,nq-1), rep(0, nq)), fn = \(x) find_optimised_abilities(c(1,x[1:(nq-1)]), x[(nq):(2*nq-1)],cs,scores,ns,nq)$totalp,control = list(fnscale = -1))
      abils <- find_optimised_abilities(c(1,temp$par[1:nq-1]), temp$par[nq:(2*nq-1)], cs, scores,ns,nq)$abils
    }else{
      temp <- optim(c(rep(2.5,nq-1), rep(0, nq), rep(0, ns)), fn = \(x) all_students_likelihood(x[(2*nq):(2*nq+ns-1)],c(1,x[1:(nq-1)]), x[(nq):(2*nq-1)],cs,scores),control = list(fnscale = -1))
      abils <- temp$par[(2*nq):(2*nq+ns-1)]
    }
    results <- list(abils = abils, discs = c(1,temp$par[1:(nq-1)]), diffs = temp$par[(nq):(2*nq-1)])
  }
  return(results)
}



#' Checks a grid of all all values in range for discrimination and difficulty
#
#' @param cs Vector of the chances of randomly getting the questions correct
#' @param scores The scores achieved on the questions by the student
#' @param ns The number of students
#' @param nq The number of unique questions
#'
#' @return A list of the optimised estimates for the parameters
#' @export
#'
find_all_optimised_3 <- function(cs, scores, ns, nq)
{
  discs <- c()
  diffs <- c()
  results <- sapply(seq(-5,5,0.1), FUN = \(x) find_optimised_abilities(1,x,cs[1],scores[seq(1, ns*nq, nq)],ns, nq)$totalp)
  best <- which.max(results)
  discs <- c(1)
  column <- round((best/101 - floor(best/101))*101)
  diffs <- c(diffs,-5 + 0.1*column)
  if (nq != 1){
    for(i in 2:nq)
    {
      results <- sapply(seq(0,5,0.1), FUN = \(x) sapply(seq(-5,5,0.1), FUN = \(y) find_optimised_abilities(x,y,cs[i],scores[seq(i, ns*nq, nq)],ns, nq)$totalp))
      best <- which.max(results)
      discs <- c(discs,0.1*floor(best/101))
      column <- round((best/101 - floor(best/101))*101)
      diffs <- c(diffs,-5 + 0.1*column)
    }
  }
  abils <- find_optimised_abilities(discs, diffs, cs, scores, ns, nq)$abils
  return(list(abils = abils, discs = discs, diffs = diffs))
}

#' Scales the values of estimates so that the first discrimination is correct
#
#' @param estimates A list with the estimated values
#' @param disc0 The true value of discrimination for the first question
#'
#' @return A list of the scaled estimates for the parameters
#' @export
#'
normalise_results <- function(estimates, disc0)
{
  d <- 1/disc0
  estimates$abils <- estimates$abils*d
  estimates$discs <- estimates$discs/d
  estimates$diffs <- estimates$diffs*d
  return(estimates)
}

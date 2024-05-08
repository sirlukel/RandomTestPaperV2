#' Will save the data.frame of questions or a list of tests to a .csv file
#'
#' @param x The item to be saved
#'
#' @return NA
#' @export


save_item <- function(x){
  UseMethod("save_item")
}


#Made for testing/development, should not be used by the user in its current state
#'@export

save_item.question <-function(x = question()){
  if(length(x) == 0)
  {
    file.remove("questions.csv")
  }
  else{
    if(!file.exists("questions.csv"))
    {
      file.create("questions.csv")
      header <- matrix(c('name', 'option_num', 'correct_score', 'wrong_loss', 'partial_score', 'given_no_correct', 'difficultly','discrimination','random_guess', 'correct',0,0,0,0,0,0,0,0,0,0),nrow = 2, byrow = TRUE)
      write.table(header,file = "questions.csv", sep = ",", col.names = FALSE, row.names = FALSE, quote = FALSE)
    }
    q<- x[-length(x)]
    q$correct <- paste(x$correct, collapse = ",")
    write.table(q, file = 'questions.csv',append = TRUE, sep = ",", col.names = FALSE, row.names = FALSE)

  }
}

#'@export

save_item.data.frame <- function(x){

  if(length(x) ==0)
  {
    file.remove("questions.csv")
  }
  else{
    if(!file.exists("questions.csv"))
    {
      file.create("questions.csv")
      header <- matrix(c('name', 'option_num', 'correct_score', 'wrong_loss', 'partial_score', 'given_no_correct','difficultly','discrimination','random_guess', 'correct',0,0,0,0,0,0,0,0,0,0),nrow = 2, byrow = TRUE)
      write.table(header,file = "questions.csv", sep = ",", col.names = FALSE, row.names = FALSE, quote = FALSE)
    }
    write.table(x, file = 'questions.csv',append = FALSE, sep = ",", col.names = TRUE, row.names = FALSE)
  }
}

#'@export

save_item.list <- function(x){
  if(length(x) == 0){
    if(file.exists("tests.csv")){
      file.remove("tests.csv")
    }
  }else{
    if(!file.exists("tests.csv"))
    {
      file.create("tests.csv")
    }
    m<-0
    for(i in 1:length(x))
    {
      if(length(x[[i]]) > m){m <-length(x[[i]])}
    }
    print(m)
    for(i in 1:length(x))
    {
      extra <- m-length(x[[i]])
      print(extra)
      x[[i]] <- c(x[[i]], rep(NA,extra))
    }
    print(x)
    write.table(x,file = 'tests.csv',append = FALSE, sep = ",", col.names = FALSE, row.names = FALSE)
  }
}

#'@export

save_item.default <- function(x)
{
  print("Tried to save invalid item")
}

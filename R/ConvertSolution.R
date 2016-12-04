#' Converts lines containing certain strings to other lines
#'
#' @param x The vector of input strings (representing one line each)
#' @param answerstring The tag indicating lines to convert to hide
#' @param hidestring What to replace the answer lines with
#' @param stopstring The tag indicating what to prompt user to do
#' @param collapsemany If you have multiple answer strings in order, collapse
#' @return The vector of transformed strings
#'
#' @description
#' When I teach, I like to have a completed solution made first (so I know it
#' works) and then modify this for students to fill in. It could be simple
#' matters of filling in a function name or supplying an argument
#' or it could be writing a complex series of functions. This requires manual
#' deletion or changing of lines each time the solution file is changed.
#' With this function, anything with the answerstring in it will be converted to
#' the hidestring in the output (which could then be written to an exercise
#' file). I sometimes insert stop functions, as well, to keep students from
#' charging forward without thinking -- they have to do what it says at this
#' this point. This can also be included.
#'
#' @examples
#' input <- c("# Count the number of tips", "phy <- ape::rcoal(15)",
#  "print(Ntip(phy)) # Answer, "# Stop and think: How can you know this answer
#  is correct?")
#  output <- ConvertSolution.R(input)
#' @export
ConvertSolution.R <- function(x, answerstring='# Answer',
hidestring='____add answer___', stopstring='# Stop and think: ', collapsemany=TRUE) {
  to.convert <- which(grepl(answerstring, x))
  x[grepl(answerstring, x)] <- hidestring
  stop.indices <- grepl(stopstring, x)
  for (i in sequence(length(stop.indices))) { #yeah, stupid. I welcome pull requests.
    x[i] <- paste(gsub(stopstring, 'stop("', x[i]), ')')
  }
  to.delete <- c()
  if(collapsemany) {
    for (i in sequence(length(x)-1)) {
      if(i%in%to.convert && (i-1)%in%to.convert) {
        to.delete <- append(to.delete, i)
      }
    }
    if(length(to.delete)>0) {
      x<-x[-to.delete]
    }
  }
  return(x)
}

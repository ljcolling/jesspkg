#' @export
find_words <- function(text, word_list) {

  check_file <- function(check) {

    base::grepl(text, pattern = check)

  }

  passed_checks <- purrr::map(word_list, function(x) check_file(x))

  has_words <- any(unlist(passed_checks))
  return(has_words)
}


#' @export
import_file <- function(filename) {

    text <- readr::read_file(filename)
    text <- stringr::str_replace_all(text, "\n", " ")
    return(text)
}


#' @export
find_cov <- function(text, pattern = "(?<!analysis of |an)covariance") {

    has_cov <- grepl(text,
                     pattern = pattern,
                     perl = TRUE,
                     ignore.case = TRUE)

    return(has_cov)
}

#' Process names into its components
#'
#' @description takes up to five words from a \code{string} containing a name and divides them
#'  into there components.
#'
#' @param word1-word5 \code{character string}, should be a single word from a
#'  \code{character string} containing a name
#'
#' @return A \code{list} with name components filled. Missing components are filled with
#'  \code{NA_caracter}
#'
#' @section Name Components:
#' \itemize{
#'  \item{prefix: Prefix title, e.g. "Mr", "Mrs", "Dr", "Gov", "President"}
#'  \item{first: First name}
#'  \item{middle: Middle name}
#'  \item{middle_initial: Single Character e.g. "Q"}
#'  \item{last: Last name, can be more than one word}
#'  \item{suffix: Suffix title, e.g. "Sr", "Jr", "ESQ"}
#' }
#'
#' @export
process_words <- function(word1, word2, word3, word4, word5) {
  assertthat::assert_that(!is.na(word1), assertthat::is.string(word1))

  if(missing(word2)) {
    # 1 word
    to_return = list(prefix = NA_character_, first = NA_character_, middle = NA_character_,
                     middle_initial = NA_character_, last = word1, suffix = NA_character_)
  } else if(missing(word3)) {
    # 2 words
    assertthat::assert_that(assertthat::is.string(word2))
    to_return <- two_words(word1, word2)
  } else if(missing(word4)) {
    # 3 words
    assertthat::assert_that(assertthat::is.string(word2), assertthat::is.string(word3))
    to_return <- three_words(word1, word2, word3)
  } else if(missing(word5)) {
    # 4 words
    assertthat::assert_that(assertthat::is.string(word2), assertthat::is.string(word3),
                            assertthat::is.string(word4))
    to_return <- four_words(word1, word2, word3, word4)
  } else {
    # 5 words
    assertthat::assert_that(assertthat::is.string(word5))
    to_return <- five_words(word1, word2, word3, word4, word5)
  }
  to_return
}

#' Functions that process name words
#'
#' @description Function for specific word totals (i.e. 2, 3, 4, or 5 words) that
#'  process them into name components. \strong{Not exported}.
#'
#' @inheritParams process_words
#'
#' @return \code{list} with name components filled. Missing components are filled with
#'  \code{NA_caracter}
#'
#' @name name_word_functions
#'

#' @rdname name_word_functions
two_words <- function(word1, word2) {
  assertthat::assert_that(assertthat::is.string(word1), assertthat::is.string(word2))

  to_return <- list(prefix = NA_character_, first = NA_character_, middle = NA_character_,
              middle_initial = NA_character_, last = NA_character_, suffix = NA_character_)
  to_return$last <- word2

  if(CheckPrefix(word1)) {
    to_return$prefix <-  word1
  } else {
    to_return$first <- word1
  }

  to_return
}

#' @rdname name_word_functions
three_words <- function(word1, word2, word3) {
  assertthat::assert_that(assertthat::is.string(word1), assertthat::is.string(word2),
                          assertthat::is.string(word3))

  to_return <- list(prefix = NA_character_, first = NA_character_, middle = NA_character_,
              middle_initial = NA_character_, last = NA_character_, suffix = NA_character_)

  if(CheckPrefix(word1)) {
    to_return$prefix <- word1
    if(CheckSuffix(word3)) {
      to_return$last <- word2
      to_return$suffix <- word3
    } else if(CheckLast(word2)) {
      to_return$last <- paste(word2, word3)
    } else {
      to_return$first <- word2
      to_return$last <- word3
    }
  } else {
    to_return$first <- word1
    if(CheckSuffix(word3)) {
      to_return$last <- word2
      to_return$suffix <- word3
    } else if(CheckMI(word2)) {
      to_return$middle_initial <- word2
      to_return$last <- word3
    } else {
      if(CheckLast(word2)) {
        to_return$last <- paste(word2, word3)
      } else {
        to_return$middle <- word2
        to_return$middle_initial <- stringr::str_sub(word2, end = 1)
        to_return$last <- word3
      }
    }
  }
  to_return
}

#' @rdname name_word_functions
four_words <- function(word1, word2, word3, word4) {
  assertthat::assert_that(assertthat::is.string(word1), assertthat::is.string(word2),
                          assertthat::is.string(word3), assertthat::is.string(word4))

  to_return <- list(prefix = NA_character_, first = NA_character_, middle = NA_character_,
              middle_initial = NA_character_, last = NA_character_, suffix = NA_character_)

  if(CheckPrefix(word1)) {
    to_return$prefix <- word1
    if(CheckSuffix(word4)) {
      to_return$suffix <- word4
      if(CheckLast(word2)) {
        to_return$last <- paste(word2, word3)
      } else {
        to_return$first <- word2
        to_return$last <- word3
      }
    } else {
      to_return$first <- word2
      if(CheckLast(word3)) {
        to_return$last <- paste(word3, word4)
      } else {
        to_return$last <- word4
        if(CheckMI(word3)) {
          to_return$middle_initial <- word3
        } else {
          to_return$middle <- word3
          to_return$middle_initial <- stringr::str_sub(word3, end = 1)
        }
      }
    }
  } else {
    to_return$first <- word1
    to_return$suffix <- word4
    if(CheckLast(word2)) {
      to_return$last <- paste(word2, word3)
    } else {
      to_return$last <- word3
      if(CheckMI(word2)) {
        to_return$middle_initial <- word2
      } else {
        to_return$middle <- word2
        to_return$middle_initial <- stringr::str_sub(word2, end = 1)
      }
    }
  }
  to_return
}

#' @rdname name_word_functions
five_words <- function(word1, word2, word3, word4, word5) {
  assertthat::assert_that(assertthat::is.string(word1), assertthat::is.string(word2),
                          assertthat::is.string(word3), assertthat::is.string(word4),
                          assertthat::is.string(word5))

  to_return <- list(prefix = NA_character_, first = NA_character_, middle = NA_character_,
              middle_initial = NA_character_, last = NA_character_, suffix = NA_character_)

  to_return[c(1:2, 5:6)] <- c(word1, word2, word4, word5)
  if(CheckMI(word3)) {
    to_return$middle_initial <- word3
  } else {
    to_return$middle <- word3
    to_return$middle_initial <- stringr::str_sub(word3, end = 1)
  }
  to_return
}

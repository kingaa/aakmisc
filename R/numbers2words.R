##' Convert integers to English words.
##'
##' \code{numbers2words} spells out integers in English.
##' The code is lifted from Andy Teucher \url{https://github.com/ateucher/useful_code/blob/master/R/numbers2words.r},
##' who in turn stole it from John Fox.
##' It has been improved somewhat by AAK
##'
##' @name numbers2words
##' @rdname numbers2words
##'
##' @param x integer to format.
##' 
##' @examples
##' numbers2words(49968883)
##' numbers2words(c(85999000,54,540,5400,54000,540000))
##' numbers2words(1e13+3)
##' 
##' @export
##' 
numbers2words <- function (x) {
  ## Function by John Fox found here:
  ## http://tolstoy.newcastle.edu.au/R/help/05/04/2715.html
  ## Tweaks by AJH to add commas and "and"
  helper <- function (x) {
    digits <- rev(strsplit(as.character(x), "")[[1]])
    nDigits <- length(digits)
    if (nDigits == 1) as.vector(ones[digits])
    else if (nDigits == 2)
      if (x <= 19)
        as.vector(teens[digits[1]])
      else
        trim(
          paste(
            tens[digits[2]],
            Recall(as.numeric(digits[1]))
          )
        )
    else if (nDigits == 3)
      trim(
        paste(
          ones[digits[3]],
          "hundred and",
          Recall(makeNumber(digits[2:1]))
        )
      )
    else {
      nSuffix <- ((nDigits + 2) %/% 3) - 1
      if (nSuffix > length(suffixes)) stop(paste(x, "is too large!"))
      trim(
        paste(
          Recall(
            makeNumber(
              digits[nDigits:(3*nSuffix + 1)]
            )
          ),
          suffixes[nSuffix],"," ,
          Recall(makeNumber(digits[(3*nSuffix):1]))
        )
      )
    }
  }
  trim <- function (text) {
    ## Tidy leading/trailing whitespace, space before comma
    text <- gsub("^\ ", "", gsub("\ *$", "", gsub("\ ,",",",text)))
    text <- gsub("- ","-",text)
    ## Clear any trailing "zero"
    text <- gsub("[-\ ]zero$","",text)
    ## Clear any trailing " and"
    text <- gsub(" and$","",text)
    ## Clear any trailing comma
    text <- gsub("\ *,$","",text)
    text
  }
  makeNumber <- function(...) as.numeric(paste(..., collapse=""))
  ## Disable scientific notation
  opts <- options(scipen=100)
  on.exit(options(opts))
  ones <- c("zero", "one", "two", "three", "four", "five", "six",
    "seven", "eight", "nine")
  names(ones) <- 0:9
  teens <- c("ten", "eleven", "twelve", "thirteen", "fourteen",
    "fifteen", "sixteen", " seventeen", "eighteen", "nineteen")
  names(teens) <- 0:9
  tens <- c("twenty-", "thirty-", "forty-", "fifty-", "sixty-",
    "seventy-", "eighty-", "ninety-")
  names(tens) <- 2:9
  x <- round(x)
  suffixes <- c("thousand", "million", "billion", "trillion")
  if (length(x) > 1) return(trim(sapply(x, helper)))
  helper(x)
}

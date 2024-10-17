#' R6 Class to open and scrape PDF Files
#'
#' @description  Scrape text from PDF file, and manage that file

PDFTool <- R6::R6Class("PDFTool",
  public = list(
    #' Open pdf file for parsing
    #'
    #' @param pdf a path to pdf or [base::raw] vector containing the pdf of test results
    #' @param start the string deliminating the start of a result
    #' @param end the string deliminating the end of a result
    #'
    #' @return A new `PDFTool` object
    initialize = \(pdf, start, end) {
      private$data <- pdf
      private$pdf <- pdftools::pdf_text(pdf)
      start <- private$pagesContaining(start)
      assertthat::assert_that(any(start), 
                              msg = "Not a valid pdf, doesn't appeart to contain results")
      private$results <- data.frame(start = which(start),
                                    end = which(private$pagesContaining(end)))
    },
    #' Vector of results (combined)
    #' 
    #' @return vector of combined results
    getResults = \() {
      n <- nrow(private$results)
      results <- character(n)
      for(i in 1:n) {
        results[i] <- paste(private$getPages(private$results[i, "start"]:private$results[i, "end"]),
                                              collapse = "\n")
      }
      results
    },
    #' get specific pdf for a result
    #' 
    #' @param resNum integer of the result (index of `getResult()`)
    #' @param output path to where pdf will be written
    #' 
    #' @return path to pdf
    getPDFs = \(resNum, output) {
      resNum = as.integer(resNum)
      assertthat::assert_that(length(resNum) == 1, msg = "resNum must be single integer")
      private$pdfOf(output, private$results[resNum, "start"]:private$results[resNum, "end"])
    }
  ),
  private = list(
    results = NULL,
    pdf = NULL,
    data = NULL,
    # Get specific page of text
    #
    # @param page `integer` of page desired or coerciable to integer
    #
    # @return a character string containing specific text
    getPages = \(page) {
      assertthat::assert_that(is.integer(page))
      private$pdf[page]
    },
    # Pages numbers containing specified search term. 
    #
    # @param search character to be searched for, will be passed to [stringr::str_detect]
    # @param logvect return a logical vector instead of numbers
    #
    # @return  `logical` of vector corresponding to pages containing search term.
    pagesContaining = \(search) {
      stringr::str_detect(private$pdf, stringr::coll(search))
    },
    # Create new subseted pdf
    #
    # @param output path of new pdf.  Must be different from source pdf
    # @param pages integer vector of pages to return, e:g. `3:5` or `c(1, 3, 5)`
    #
    # @return path to the new pdf (`output`)
    pdfOf = \(output, pages) {
      pages <- as.integer(pages)
      assertthat::assert_that(!assertthat::are_equal(output, private$data), 
                              msg = "output cannot be same path as source file")
      pdftools::pdf_subset(private$data, pages, output)
    }
  )
)

#' Get Xpert Keyed Value
#'
#' @details
#' \code{keyed_ts} returns the \code{POSIXct} date/time of the keyed value in UCT/GMT time
#' 
#'
#' @param key key of value to extract
#' @param txt source of extraction
#'
#' @return the value after the key
keyed_value <- function(key, txt) {
  stringr::str_extract(txt, paste0("(?<=", stringr::str_replace_all(key, "\\s", "\\\\s"), ":\\s{1,30})[:alnum:]+"))
}

#' @rdname keyed_value
#' @param tz [TZ] object containing either single time zone of map of time zones and machine names
#' @param machine_name the identifier used to lookup the time zone
keyed_ts <- function(key, txt, tz, machine_name) {
  browser()
  assertthat::assert_that(inherits(tz, "TZ"))
  value <- dplyr::tibble(val = stringr::str_extract(txt, paste0("(?<=", stringr::str_replace_all(key, "\\s", "\\\\s"),
                                                                ":\\s{1,30})[:graph:]{8}\\s[:graph:]{8}")),
                         tz = tz$getTZ(machine_name)) %>%
    dplyr::rowwise() %>% dplyr::mutate(val = as.POSIXct(val, tryFormats = "%m/%d/%y %H:%M:%S", tz = tz))
  # Convert to UTC
  attr(value$val, "tzone") <- "GMT"
  return(value$val)
}
#' Xpert Parsing R6
#' 
#' @description
#' The class used to parse Xpert HPV results
#' 
#' @export
Xpert <- R6::R6Class("Xpert",
  public = list(
    #' @description Parse new pdf of xpert result(s)
    #'
    #' @param pdf path to pdf or [raw] vector of pdf
    #' @param xpertTZ timezone of xpert machine (see [OlsonNames()]) or [TZ] R6 object of machine mapped timezimes
    #'
    #' @importFrom magrittr %>%
    #' @return a `Xpert` R6 Object
    initialize = \(pdf, xpertTZ = Sys.timezone()){
      private$pdf <- PDFTool$new(pdf, "Xpert HPV HR_16_18-45", "Supervisor Initial/Date")
      xpert <-private$pdf$getResults()
      tbl <- stringr::str_replace_all(
        stringr::str_extract(xpert, "SAC[[:graph:]\\s\\n]+(?=\\n\\s\\-\\sDetail:)|SAC[[:graph:]\\s\\n]+(?=\\nUser:)"),
        "(?<=HPV)\\s(?=\\d{2})", "_") %>%
        lapply(readr::read_table, col_names = c("analyte", "ct", "end_point", "result", "probe_check")) %>%
        lapply(dplyr::select, c("analyte", "ct", "result"))
      private$results <- dplyr::tibble(
        sample_id = keyed_value("Sample ID\\*?", xpert),
        test_result =
          stringr::str_replace_all(
            stringr::str_extract(xpert, "(?<=Test\\sResult:\\s{1,30})HPV[[:graph:]\\s\\n]+(?=\\n\\-\\nAnalyte)"),
            "\\n\\s{2,}", " "),
        status = keyed_value("Status", xpert),
        error = keyed_value("Error Status", xpert),
        error_message = stringr::str_extract(xpert, "(?<=Errors\\n)[:graph:]+"),
        instrument_sn = keyed_value("Instrument S/N", xpert),
        cartridge_sn = keyed_value("Cartridge S/N\\*?", xpert),
        reagant_lot = keyed_value("Reagent Lot ID\\*?", xpert),
        notes = stringr::str_extract(xpert, "(?<=Notes:[^[:graph:]\r\n]{0,30})[:graph:].*(?!\n{1}])"),
        user = stringr::str_extract(xpert, "(?<=User:[^[:graph:]\r\n]{0,30})[:graph:].*(?!\n{1}])"),
        results = tbl) %>%
        tidyr::unnest(cols = results) %>%
        tidyr::pivot_wider(
          names_from = analyte,
          values_from = c(ct, result),
          names_glue = "{analyte}_{.value}"
        ) %>% dplyr::mutate(
          start_time = keyed_ts("Start Time", xpert, xpertTZ, instrument_sn),
          end_time = keyed_ts("End Time", xpert, xpertTZ, instrument_sn),
        ) %>%
        dplyr::relocate(tidyselect::starts_with(c("SAC", "HPV_16", "HPV_18", "P3", "P4", "P5")), .after="error")
      class(private$results) <- c("xpert_results", class(private$results))
    },
    #' @description Get `xpert_result` (a `data.frame`) for a particular index
    #'
    #' @param idx `integer` vector of indices of xpert results to return, if not provided, function will return all parsed results
    #'
    #' @return a `xpert_result` class (a specialized `data.frame`)
    getResults = \(idx) {
      if(!missing(idx)) {
        assertthat::assert_that(is.integer(idx), msg = "idx must be integer vector")
        return (private$results[idx, ])
      } 
      return(private$results)
    },
    #' @description Write the single xpert result pdf to disk
    #'
    #' @param idx single `integer` of results parsed in this class
    #' @param output path to output pdf (you must specify extension `.pdf` yourself). If not provided will create a file named using `sample_id` in a temp directory
    #'
    #' @return the path of the saved pdf
    getPDFs = \(idx, output) {
      if (missing(output)) {
        output <- paste0(tempdir(), "/", private$results[idx,"sample_id"], ".pdf")
      }
      return (private$pdf$getPDFs(idx,output))
    }
  ),
  private = list(
    pdf = NULL,
    results = NULL
  )
)

#' Time Zone R6
#' 
#' @description
#' The class used to mapping machine name to time zome
#' 
#' @export
TZ <- R6::R6Class("TZ",
  public = list(
    #' @description Create a `TZ` object
    #'
    #' @param ... either a character of a single time zone or mapped time zones i.e. 
    #'     `TZ$new("110016067" = "America/Mexico_City", "110016068" = "America/Sao_Paulo")` where the names are the literal
    #'     Xpert machine names on the generated reports and the time zones are one of [OlsonNames()]
    #' 
    #' @return a TZ object
    initialize = \(...) {
      m <- list(...)
      if (length(m) == 1) {
        m <- m[[1]]
        private$assert_tz(m)
        private$map <- m
      } else {
        assertthat::assert_that(all(names(m) != ""), msg = "All time zones much be named. i.e. \"110016067\" = \"America/Mexico_City\"")
        private$assert_tz(m)
        private$map <- m
      }
    },
    #' @description Return a time zone string
    #'
    #' @param device_names the literal machine names for which you want to lookup a timezones.
    #' 
    #' @return a time zone stirng
    getTZ = \(device_names) {
      if (length(private$map) > 1) {
        assertthat::assert_that(!missing(device_names), 
                                msg = "You must specific device names if there is more than one time zone")
        assertthat::assert_that(all(device_names %in% names(private$map)), msg = "Device name(s) not found in time zone list")
        return (sapply(device_names, \(x) { private$map[[x]] }, USE.NAMES = F)) 
      } else return(private$map)
    }
  ),
  private = list(
    map = NULL,
    isOlsonName = \(x) {
      return(x %in% OlsonNames())
    },
    assert_tz = \(x) {
      assertthat::assert_that(all(private$isOlsonName(x)), 
                              msg = "time zones must be one of OlsonNames() i.e. tz %in% OlsonNames()")
    }
  )
)

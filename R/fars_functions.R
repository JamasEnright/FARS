#' Read FARS file
#'
#' This is a simple function that reads in a file (called \code{filename}) and returns a data file.
#' The function expects the file to be in a CSV format.
#'
#' It checks to make sure the file exists first, and generates an error if it does not.
#'
#' @param filename A file name for the function to read in
#'
#' @return This function returns the file as a data file.
#'
#' @examples
#'
#' \dontrun{
#' fars_read("inputfile.csv")
#' fars_read("accident_2001.csv")
#' fars_read(filenamevar)
#' fars_read("madeupfile.ext")
#' }
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#' @export
fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}

#' Make FARS filename
#'
#' This function takes in year (\code{year}) and returns a properly formatted string pointing to
#' the file in the local directory.
#'
#' @param year The year of the data
#'
#' @return A properly formatted file name, of the format "accident_\code{year}.csv.bz2"
#'
#' @examples
#'
#' \dontrun{
#' make_filename(1991)
#' make_filename(2001)
#' make_filename("ababab")
#' }
#'
#' @export
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}

#' Read FARS year data
#'
#' This reads in data for a number of years as specified in the vector \code{years}.
#' Each file is read for each year, and added to an overall data set.
#'
#' If there is any year without an associated data file, the function will report a warning and cease working.
#'
#' @param years A vector of intergers, representing the years to read in
#'
#' @return This function returns a data set of the years specified.
#'
#' @examples
#'
#' \dontrun{
#' fars_read_years(c(1991,1992,1993))
#' fars_read_years(2001)
#' fars_read_years(c(1991,2002,2013))
#' fars_read_years("ababab")
#' }
#'
#' @importFrom magrittr '%>%'
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @export
fars_read_years <- function(years) {
        lapply(years, function(year) {
                file <- make_filename(year)
                tryCatch({
                        dat <- fars_read(file)
                        dplyr::mutate(dat, year = year) %>%
                                dplyr::select(MONTH, year)
                }, error = function(e) {
                        warning("invalid year: ", year)
                        return(NULL)
                })
        })
}

#' Summarize FARS years
#'
#' This takes in a vector of \code{years} and reads the data for them. Then this function
#' creates a summary of the number of observations in each year and month.
#'
#' @param years A vector of intergers, representing the years to summarise
#'
#' @return This function returns a dataset containing the year, month and number of observations.
#'
#' @examples
#'
#' \dontrun{
#' fars_summarize_years(c(1991,1992,1993))
#' fars_summarize_years(2001)
#' fars_summarize_years(c(1991,2002,2013))
#' fars_summarize_years("ababab")
#' }
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr bind_rows
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom tidyr spread
#' @export
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by(year, MONTH) %>%
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}

#' Map the Accidents for a State
#'
#' This function maps the accidents for a given state (represented by \code{state.num}) and
#' \code{year} onto a map of that state.
#'
#' The function checks that the state is a valid state in the data set.
#'
#' If there are no accidents, no plot is produced.
#'
#' @param state.num An integer representing the state of interest
#' @param year An integer representing the year of interest
#'
#' @return This function returns a map object got a given state, with the accident locations
#'    plotted on it
#'
#' @examples
#'
#' \dontrun{
#' fars_map_state(12,2015)
#' fars_map_state(21,2007)
#' fars_summarize_years(87,"aaa")
#' }
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @export
fars_map_state <- function(state.num, year) {
        filename <- make_filename(year)
        data <- fars_read(filename)
        state.num <- as.integer(state.num)

        if(!(state.num %in% unique(data$STATE)))
                stop("invalid STATE number: ", state.num)
        data.sub <- dplyr::filter(data, STATE == state.num)
        if(nrow(data.sub) == 0L) {
                message("no accidents to plot")
                return(invisible(NULL))
        }
        is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
        is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
        with(data.sub, {
                maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
                          xlim = range(LONGITUD, na.rm = TRUE))
                graphics::points(LONGITUD, LATITUDE, pch = 46)
        })
}

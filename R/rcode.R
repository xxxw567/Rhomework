#' Read csv file
#'
#' Reads data from a csv file. If the file does not exist, the function will
#' stop and return an error.
#'
#' @param filename The name of the csv file to read
#'
#' @return A data.frame object containing the data read from \code{filename}
#'
#' @examples
#' \dontrun{
#' fars_data <- fars_read("accident_2015.csv.bz2")
#' }
#'
#' @importFrom dplyr tbl_df
#' @importFrom readr read_csv
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#' Generate filename
#'
#' Generates a filename including the provided \code{year}.
#'
#' @param year The year of interest provided as either character or numeric
#'
#' @return A character vector  in the following form: \code{accident_<year>.csv.bz2}
#'
#' @examples
#' \dontrun{
#' fars_filename <- make_filename(2015)
#' }
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' Extract monthly data
#'
#' For the specified years this function returns their monthly data. If there
#' is no data for the specified year available, the function will return an
#' error.
#'
#' @param years Years of interest provided as list or vector
#'
#' @return A list object. Each element holds monthly data for one year as
#'  data.frame with columns `MONTH` and `year`
#'
#' @examples
#' \dontrun{
#' fars_year_data <- fars_read_years(2013)
#' fars_year_data <- fars_read_years(c(2013, 2015))
#' }
#'
#' @importFrom dplyr mutate select %>%
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

#' Summarize years
#'
#' For the specified years this function summarizes their data i.e., it return
#' how many incidients occured in each month.
#'
#' @inheritParams fars_read_years
#'
#' @return A data.frame that provides the amount of monthly data per year
#'
#' @importFrom dplyr group_by summarize %>%
#' @importFrom tidyr spread
#'
#' @examples
#' \dontrun{
#' fars_summary <- fars_summarize_years(2013)
#' fars_summary <- fars_summarize_years(c(2013, 2014))
#' }
#'
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' Plot incidents on map
#'
#' If there is data for a specified state.num, \code{fars_map_state} plots
#' incidents on a map. The canvas limit is set to greater than 900 Longitude
#' and 90 Latitude. If there is no data for a specified \code{state.num} the
#' function exits with an error.
#'
#' @param state.num State number as one-dimensional numeric vector
#' @inheritParams make_filename
#'
#' @examples
#' \dontrun{
#' fars_map_state(1, 2013)
#' }
#'
#' @importFrom dplyr filter
#' @importFrom graphics points
#' @importFrom maps map
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

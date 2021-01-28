#' Makes a data frame of time intervals based on a input vector of dates
#'
#' @param x Input vector of dates. These correspond to the starting dates
#'          of each interval. The interval ends the day prior to the start
#'          of the next interval.
#' @param labels A vector of labels that uniquely identifies each interval.
#' @seealso \code{\link{refine_window}}
#' @examples
#' x <- ymd('2020-1-1', '2020-4-1', '2020-7-1', '2020-10-1', '2021-1-1')
#' make_intervals(x, labels=paste(c('Q1','Q2','Q3','Q4'), '2020'))
#' @export
make_intervals <- function(x, labels=null){
  n <- length(x)
  a = x[1:(n-1)]
  b = x[2:n]
  output <- tibble(start = a, end = b - ddays(1)) %>%
    mutate(interval = lubridate::interval(start, end))
  if( !is.null(labels) ){
    output <- output %>% mutate(label = labels)
  }

  return(output)
}


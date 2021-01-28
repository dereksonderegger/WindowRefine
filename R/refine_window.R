#' Refine a Window given subsequent periods and difference in time.
#'
#' In some censured data cases, we are given only the month or quarter
#' that two sequential observations are given, but we know the number of
#' days between the observations. Suppose we know that the first observation
#' was in January, the second was in February and that the measurements were
#' 10 days apart. Then we KNOW the first measurement was between Jan 21st
#' and Jan 31 as the earliest the second could have occurred is February 1st.
#'
#' @param window The window for the first observation without and other constraints.
#' @param obs_periods The period labels for subsequent observations. For example
#'                    the month name, or "Q1 2020". These labels need to correspond
#'                    to labels in the `period_df` parameter.
#' @param obs_deltas The number of days between the first observation and the
#'                   nth observation.
#' @param period_df A data frame with columns `interval` and `label` where the
#'                  interval is a `lubridate::interval` object and label is a
#'                  character string corresponding to values in the `obs_periods`
#'                  parameter.
#' @seealso \code{\link{make_intervals}}
#' @examples
#' period_df <- make_intervals(
#'   ymd('2020-1-1', '2020-1-11', '2020-1-21', '2020-1-31'),
#'   labels = c('P1','P2','P3'))
#' window = period_df$interval[1]
#' obs_periods = c('P2', 'P3')
#' obs_deltas  = c(5, 23)
#'
#' refine_window(window, obs_periods, obs_deltas, period_df)
#' @export
refine_window <- function(window, obs_periods, obs_deltas, period_df ){
  for(i in 1:length(obs_periods)){
    period = period_df %>% filter(label == obs_periods[i]) %>% pull(interval)
    window = interval( max(int_start(window), int_start(period) - ddays(obs_deltas[i])),
                       min(int_end(window), int_end(period) - ddays(obs_deltas[i])) )
  }
  return(window)
}

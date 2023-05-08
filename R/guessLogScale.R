
# This function decides whether data should be treated as log scale based on the range.
# The function returns TRUE, indicating that the values should be assigned as a log scale if
# the difference between the log of the maximal value and the minimal value is greater than
# the value of log10threshold (defaults to 1 or a 10-fold difference in pre-log-transformed values).
# There are two possible return values:
#       FALSE if the log10 range is less than 1 or if there are negative values.
#       TRUE  if the log10 range is greater than or equal to 1 and there are no negative values.

guessLogScale <- function( numeric_values, log10threshold = 1.0 ){
  values.range <- range( numeric_values, na.rm = TRUE )
  all( numeric_values > 0, na.rm = TRUE ) &&
    isTRUE( log10( values.range[2] ) - log10( values.range[1] ) >= log10threshold )
}

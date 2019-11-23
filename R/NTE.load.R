#' loads the data from an NTE-3000 generated data file
#' into a data frame
#'
#' @param filename filename including path
#' @return data.frame
#' @examples
#' d = NTE.load('20191121.txt')
#' @export
NTE.load <- function(filename) {
  if (!(file.exists(filename))) { warning(paste("File",filename,"does not exist!"))}
  d <- read.csv(filename, header=FALSE, sep='\t', stringsAsFactors = FALSE)
  names(d)=c('pressure','T1','T2','T.substrate','thickness','timedate','na')
  d = d[,1:6]
  d = na.omit(d)

  d[1:5] <- sapply(d[1:5],as.numeric)
  d = na.omit(d)

  d$time <- chron(times. = substr(d$timedate,1,8))
  d$time.sec = period_to_seconds(hms(d$time))
  d$time.step = c(1E-6,diff(d$time.sec))
  d$pressure.mBar = 1.333*d$pressure
  d$deposition.rate = c(0,diff(d$thickness)) / d$time.step
  d$deposition.rate.smooth = predict(sm.spline(d$time.sec, d$deposition.rate, df=60), d$time.sec)
  d$deposition.rate[which(abs(d$deposition.rate)>4)] <- NA
  d
}

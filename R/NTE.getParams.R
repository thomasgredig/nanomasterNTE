#' return average parameters from deposition
#'
#' @param d data frame with NTE data
#' @return data frame
#' @examples
#' d = NTE.load('20191121.txt')
#' NTE.getParams(d)
#' @export
NTE.getParams <- function(d) {
  d2 = NTE.depStartEnd(d)
  data.frame(
    base.pressure.torr = mean(d$pressure[1:nrow(d)*0.2]),
    deposition.pressure = mean(d2$pressure),
    T.deposition.oC = mean(d2$T.substrate),
    T.deposition.oC.sd = sd(d2$T.substrate),
    deposition.time.s = max(d2$time.sec) - min(d2$time.sec),
    thickness.mu = max(d2$thickness) - min(d2$thickness),
    rate.mu.per.s = mean(d2$deposition.rate),
    rate.mu.per.s.sd = sd(d2$deposition.rate)
  )
}

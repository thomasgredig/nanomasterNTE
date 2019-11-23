#' guess the deposition start and end time
#'
#' @param d data frame with NTE data
#' @param buffer how much data to show before / after plot, 1=deposition time
#' @return data frame
#' @examples
#' d = NTE.load('20191121.txt')
#' g1 = NTE.plotDeposition(d)
#' @export
NTE.depStartEnd <- function(d,buffer = 0) {
  #q1 = which(d$thickness>8)
  q1 = which(d$deposition.rate>0.2)
  if (length(q1)==0) { return (NA); }
  # add buffer of about 15%
  q1.buf = round(length(q1)*buffer)
  q.start = max(0,q1[1] - q1.buf)
  q.end = min(nrow(d),q1[length(q1)]+q1.buf)

  # return subset
  d[(q.start:q.end),]
}

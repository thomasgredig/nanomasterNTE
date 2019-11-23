#' makes 4 plots with NTE data
#'
#' @param d data frame with NTE data
#' @param sz size of data points in graphs
#' @return ggplot graph
#' @examples
#' d = NTE.load('20191121.txt')
#' @export
NTE.plotSummary <- function(d,sz=2) {
  p1 = ggplot(d, aes(time.sec/3600,pressure*1e6)) +
    geom_path(size=sz, col='red') +
    xlab('time (h)') +
    ylab(expression(paste('p (',mu,'Torr)'))) +
    theme_bw()

  p2 = ggplot(d, aes(time.sec/3600,thickness)) +
    geom_path(size=sz, col='red') +
    xlab('time (h)') +
    ylab(expression(paste('t (m.u.)'))) +
    theme_bw()

  p3 = ggplot(subset(d, T.substrate<300), aes(time.sec/3600,T.substrate)) +
    geom_point(size=sz, col='red') +
    xlab('time (h)') +
    ylab(expression(paste('T'[sub],' ('^o,'C)'))) +
    theme_bw()

  p4 = ggplot(d, aes(time.sec/3600,deposition.rate)) +
    geom_point(size=sz, col='red') +
    xlab('time (h)') +
    ylab(expression(paste('r (m.u./s)'))) +
    theme_bw()

  p.sum = plot_grid(p1, p2, p3, p4, labels = c('A', 'B', 'C', 'D'), label_size = 12)
  p.sum
}

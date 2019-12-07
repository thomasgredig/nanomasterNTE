#' graph of the deposition, colors the actual deposition portion
#'
#' @param d data frame with NTE data
#' @param buffer how much data to show before / after plot, 1=deposition time
#' @return ggplot graph
#' @examples
#' d = NTE.load('20191121.txt')
#' g1 = NTE.plotDeposition(d)
#' @export
NTE.plotDeposition <- function(d, buffer=0.5) {
  d2 = NTE.depStartEnd(d, 0)
  p.sum = NA
  if (!is.na(d2)) {
    deposition.start = d2$time.sec[1]/3600
    deposition.end = d2$time.sec[nrow(d2)]/3600
    Tdep.max = mean(d2$T.substrate) + sd(d2$T.substrate)
    Tdep.min = mean(d2$T.substrate) - sd(d2$T.substrate)
    p.max = (mean(d2$pressure) + sd(d2$pressure))*1e6
    p.min = (mean(d2$pressure) - sd(d2$pressure))*1e6
    rate.max = (mean(na.omit(d2$deposition.rate)) + sd(na.omit(d2$deposition.rate)))
    rate.min = (mean(na.omit(d2$deposition.rate)) - sd(na.omit(d2$deposition.rate)))

    d2 = NTE.depStartEnd(d, 0.5)
    p1 = ggplot(d2, aes(time.sec/3600,pressure*1e6)) +
      geom_rect(aes(xmin=deposition.start, xmax=deposition.end, ymin=-Inf, ymax=Inf),fill='grey90',alpha=0.9) +
      geom_rect(aes(xmin=deposition.start, xmax=deposition.end, ymin=p.max, ymax=p.min),
                fill='beige',alpha=1) +
      geom_path(size=2, col='red') +
      xlab('time (h)') +
      ylab(expression(paste('p (',mu,'Torr)'))) +
      ggtitle(paste('pressure:',signif(mean(d2$pressure),2),'Torr')) +
      theme_bw(base_size = 8)

    p2 = ggplot(d2, aes(time.sec/3600,thickness)) +
      geom_rect(aes(xmin=deposition.start, xmax=deposition.end, ymin=-Inf, ymax=Inf),fill='beige',alpha=0.1) +
      geom_path(size=2, col='red') +
      xlab('time (h)') +
      ylab(expression(paste('t (m.u.)'))) +
      ggtitle(paste('thickness:',signif(max(d2$thickness)-min(d2$thickness),2),'m.u.')) +
      theme_bw(base_size = 8)

    p3 = ggplot(d2, aes(time.sec/3600,T.substrate)) +
      geom_rect(aes(xmin=deposition.start, xmax=deposition.end, ymin=-Inf, ymax=Inf),fill='grey90',alpha=0.1) +
      geom_rect(aes(xmin=deposition.start, xmax=deposition.end, ymin=Tdep.max, ymax=Tdep.min),
                fill='beige') +
      geom_point(size=2, col='red') +
      xlab('time (h)') +
      ylab(expression(paste('T'[sub],' ('^o,'C)'))) +
        ggtitle(paste('T substrate:',signif(mean(d2$T.substrate),2),'oC')) +
        theme_bw(base_size = 8)

      p4 = ggplot(d2, aes(time.sec/3600,deposition.rate)) +
        geom_rect(aes(xmin=deposition.start, xmax=deposition.end, ymin=-Inf, ymax=Inf),fill='grey90',alpha=0.1, color='grey') +
        geom_rect(aes(xmin=deposition.start, xmax=deposition.end, ymin=rate.max, ymax=rate.min),
                  fill='orange',alpha=1, color='orange') +
        geom_point(size=2, col='red') +
        xlab('time (h)') +
        ylab(expression(paste('r (m.u./s)'))) +
        ggtitle(paste('rate:',signif(mean(d2$deposition.rate),2),'m.u./s')) +
        theme_bw(base_size = 8)

      p.sum = plot_grid(p1, p2, p3, p4, labels = c('A', 'B', 'C', 'D'), label_size = 12)
  }
  p.sum
}

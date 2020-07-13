plot.spectrum <- function(dat, lab = "", main = "",
                          y_max = 1, tick = c(8, 4), unit = 1)
{
  dat_FFT <- abs(fft(as.vector(dat)))
  
  data_len  <- length(dat_FFT)
  freq_tick <- c(data_len, tick, 2)
  
  plot(dat_FFT / max(dat_FFT), type = "l", main = main,
       ylab = "|normarized frequency sprectrum|",
       ylim = c(0, y_max),
       xlab = sprintf("frequency [1/%s]", lab),
       xlim = c(1, data_len / 2),
       xaxt = "n"
  )
  
  axis(side = 1, at = data_len / freq_tick * unit + 1,
       labels = sprintf("1/%d", freq_tick),
       cex.axis = 0.7
  )
}
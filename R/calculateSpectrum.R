#' Function calculating and plotting the amplitude spectrum.
#'
#' The function calculates and plots the amplitude/energy spectrum via the FFT
#' and also the corresponding frequencies or periods.
#'
#' This function takes a numeric vector containing the time series and returns the
#' corresponding frequency spectrum over the full bandwidth 0Hz -> sample frequency.
#'
#' @param timeSignal Numeric vector; Time signal which should be transformed
#' into the frequency domain.
#' @param timeStep Numeric value (default: 1); Elapsed time between two data points.
#' @param xAxis Character string (default: "period"); Either "frequency"
#' or "period". Defining if on the x-axis the linear frequency or the reciprocal
#' oscillation period is displayed.
#' @param yAxis Character string (default: "amplitude"); Either "amplitude"
#' or "energy". The spectral energy is proportional to the square of the
#' amplitude. Normalized by the maximum energy.
#' @param xAxisLimits Numeric vector of length 2 (default: c(0, 1));
#' Defines the range of the spectrum being displayed. 0 -> 0Hz/Inf Period, 1 -> Nyquist Frequency/Nyquist Period (twice the time step).
#' The range is the same as for band edges during filter design in package signal (i.e. fir1())
#' @param region Character string (default: "half"); Either "half" or "whole".
#' Defines wether the region till the Nyquist frequency is shown or the range until
#' the sample frequency with aliases.
#' @param plotFlag Logical value (default: TRUE); Defining if amplitude/energy
#' spectrum should be plotted or not.
#'
#' @return Returns a list containing information about the time and frequency
#' paratmeters, the original time series, the frequency spectrum and quantiles
#' of the spectral energy.
#'
#' @author Daniel Beiter, \email{daniel.beiter@@gfz-potsdam.de}
#' @keywords frequency analysis, frequency spectrum

#' @import ggplot2
#' @export
#'
#' @examples
#' N <- 200 # number of samples
#' d_t <- 2 # time step (temporal resolution)
#' A_1 <- 3 # signal amplitude
#' A_2 <- 5
#' A_3 <- 2
#' T_1 <- 5 # signal periods
#' T_2 <- 10
#' T_3 <- 25
#'
#' t <- seq(from = 0, length.out = N, by = d_t) # setting up time vector
#' y <- A_1 * cos(2*pi / T_1 * t) +
#'      A_2 * cos(2*pi / T_2 * t) +
#'      A_3 * cos(2*pi / T_3 * t) # calculating signal values
#'
#' frequencySpectrum <- calculateSpectrum(timeSignal = y, timeStep = d_t) # calculates the frequency spectrum
#'
calculateSpectrum <- function(timeSignal, timeStep = 1, xAxis = "period", yAxis = "amplitude", xAxisLimits = c(0, 1), region = "half", plotFlag = TRUE) {
  ### testing arguments before proceeding
  if (!is.numeric(timeSignal))
    stop("Argument \"timeSignal\" must be a numeric vector!")
  if (!(is.numeric(timeStep) & length(timeStep) == 1))
    stop("Argument \"timestep\" needs to be a single numeric value!")
  if (!is.logical(plotFlag) | is.na(plotFlag))
    stop("Argument \"plotFlag\" must be either TRUE or FALSE and not \"", plotFlag, "\"!")
  if (!xAxis %in% c("frequency", "period"))
    stop("Use either \"frequency\" or \"period\" for xAxis! Value used was: \"", xAxis, "\"!")
  if (!yAxis %in% c("amplitude", "energy"))
    stop("Use either \"amplitude\" or \"energy\" for yAxis!")
  if (!(region %in% c("half", "whole")))
    stop("Use either \"half\" or \"whole\" for region!")
  if (!(diff(xAxisLimits) > 0 & xAxisLimits[1] >= 0 & xAxisLimits[2] <= 1))
    stop("Argument \"xAxisLimits\" must be a numeric vector with first value >= 0 and second value <= 1!")

  ### essential parameters
  N <- length(timeSignal)
  d_t <- timeStep
  f_s <- 1 / timeStep
  d_f <- 1 / (N * timeStep)

  ### additional parameters
  T_min <- 2 * timeStep
  T_max <- N * timeStep
  f_min <- 1 / T_max
  f_max <- 1 / T_min

  ### creating time series
  t <- seq(from = 0, length.out = N, by = d_t)
  timeSeries <- data.frame(time = t,
                           value = timeSignal)

  ### creating frequency spectrum
  f <- seq(from = 0, length.out = N, by = d_f)
  Y <- round(fft(timeSignal - mean(timeSignal)), 3)
  frequencySpectrum <- data.frame(frequency = f,
                                  period = 1 / f,
                                  coefficients = Y,
                                  amplitude = 2 / N * Mod(Y),
                                  phase = Arg(Y),
                                  energy = Mod(Y) ^ 2 / max(Mod(Y) ^ 2))

  if (region == "half") {
    if (N %% 2 == 0)
      frequencySpectrum <- frequencySpectrum[1:(N / 2 + 1), ]
    if (N %% 2 == 1)
      frequencySpectrum <- frequencySpectrum[1:((N + 1) / 2), ]
    xAxisLimits2 <- xAxisLimits * f_s / 2
  } else
    xAxisLimits2 <- xAxisLimits * f_s

  ### calculate supporting values for plot appearance
  xLabel <- paste0(toupper(substring(xAxis, 1, 1)), substring(xAxis, 2))
  yLabel <- paste0(toupper(substring(yAxis, 1, 1)), substring(yAxis, 2))

  ### create plot if plot flag is set
  if (plotFlag) {
    pSpectrum <- ggplot2::ggplot(frequencySpectrum) +
      ggplot2::geom_step(ggplot2::aes_string(x = "frequency", y = yAxis)) +
      ggplot2::labs(x = xLabel, y = yLabel) +
      ggplot2::coord_cartesian(xlim = xAxisLimits2) +
      ggplot2::theme_bw()
    if (xAxis == "frequency") {
      pSpectrum <- pSpectrum +
        ggplot2::scale_x_continuous(labels = function(x) sprintf("%.2f", x))
    }
    if (xAxis == "period") {
      pSpectrum <- pSpectrum +
        ggplot2::scale_x_continuous(labels = function(x) sprintf("%.2f", 1 / x))
    }
    plot(pSpectrum)
  }

  ### collect parameters for output
  parameters <- list(sampleLength = N,
                     timeStep = timeStep,
                     frequencyStep = d_f,
                     sampleFrequency = f_s,
                     minimumPeriod = T_min,
                     maximumPeriod = T_max,
                     contentDC = round(mean(timeSignal), 1))

  ### calculate quantiles of energy spectrum
  quantile <- c(0.01, 0.1, 0.25, 0.5, 0.75, 0.9, 0.99)
  energyIndex <- sapply(X = quantile, FUN = function(x) min(which(cumsum(frequencySpectrum$energy[1:(nrow(frequencySpectrum) / 2)]) / sum(frequencySpectrum$energy[1:(nrow(frequencySpectrum) / 2)]) >= x)))
  signalEnergy <- data.frame(quantile = quantile,
                             frequency = frequencySpectrum$frequency[energyIndex],
                             period = frequencySpectrum$period[energyIndex])

  ### put everything together into one list
  output <- list(timeSeries, frequencySpectrum, signalEnergy, parameters)
  names(output) <- c("timeSeries", "frequencySpectrum", "signalEnergy", "parameters")
  class(output) <- "FrequencySpectrum"

  return(output)
}

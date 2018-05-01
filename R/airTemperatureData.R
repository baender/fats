#' DWD historical air temperature data from Potsdam (STATIONS_ID 03987)
#'
#' Historical air temperature and relative humidity data from DWD station in Potsdam, Germany.
#' Values have hourly resolution.
#'
#' @docType data
#'
#' @usage data(DWD_Potsdam_HourlyAirTemperature)
#'
#' @format A data frame with 6 columns.
#' STATIONS_ID (integer: 3987), DWD station ID;
#' MESS_DATUM (integer: YYYYMMDDHH), timestamp format of measurement;
#' QN_9 (integer), quality flag;
#' TT_TU (numeric), air temperature in degrees Celius;
#' RF_TU (numeric), relative humidity in percent;
#' eor (character), end of record, terminating column;
#' A detailed explanation can be found under \href{ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate/hourly/air_temperature/historical/DESCRIPTION_obsgermany_climate_hourly_tu_historical_en.pdf}{Data set description}
#'
#' @keywords datasets air temperature
#'
#' @references
#'
#' @source Data can be downloaded from \href{ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate/hourly/air_temperature/historical/}{DWD Open Data Server}.
#' Terms and conditions can be found under \href{https://www.dwd.de/EN/service/copyright/copyright_artikel.html}{Copyright}.
#'
#' @examples
#' temperatureSpectrum <- calculateSpectrum(DWD_Potsdam_HourlyAirTemperature$TT_TU[1:2^15], timeStep = 1 / 24)
"DWD_Potsdam_HourlyAirTemperature"

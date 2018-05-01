#' DWD historical precipitation data from Potsdam (STATIONS_ID 03987)
#'
#' Historical precipitation data from DWD station in Potsdam, Germany.
#' Values have hourly resolution.
#'
#' @docType data
#'
#' @usage data(DWD_Potsdam_HourlyPrecipitation)
#'
#' @format A data frame with 7 columns.
#' STATIONS_ID (integer: 3987), DWD station ID;
#' MESS_DATUM (integer: YYYYMMDDHH), timestamp format of measurement;
#' QN_8 (integer), quality flag;
#' R1 (numeric), hourly precipitation height in mm;
#' RS_IND (integer), binary index, 0 no precipitation, 1 precipitation;
#' WRTR (integer), WR-code for form of precipitation
#' eor (character), end of record, terminating column;
#' A detailed explanation can be found under \href{ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate/hourly/precipitation/historical/DESCRIPTION_obsgermany_climate_hourly_precipitation_historical_en.pdf}{Data set desription}
#'
#' @keywords datasets precipitation
#'
#' @references
#'
#' @source Data can be downloaded from \href{ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate/hourly/precipitation/historical/}{DWD Open Data Server}.
#' Terms and conditions can be found under \href{https://www.dwd.de/EN/service/copyright/copyright_artikel.html}{Copyright}.
#'
#' @examples
#' temperatureSpectrum <- calculateSpectrum(DWD_Potsdam_HourlyPrecipitation$R1[1:2^15], timeStep = 1 / 24)
"DWD_Potsdam_HourlyPrecipitation"

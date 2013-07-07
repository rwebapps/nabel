#' Plot NABEL data
#'
#' The National Air Pollution Monitoring Network (NABEL) provides time series data
#' on air-borne pollutant concentrations from a network of measurement stations across
#' Switzerland. The data can be queried with an HTML form (see sources). This is a
#' simple function that serves as a wrapper for that HTML form. Its main purpose
#' is to demonstrate some of the capabilities of \url{http://opencpu.org/} by
#' Jeroen Ooms.
#'
#' A single \code{pollutant} or meteorological variable is plotted
#' (default is \code{o3}):
#' \enumerate{
#'  \item o3 (Ozone (O3))
#'  \item no2 (Nitrogen dioxide (NO2))
#'  \item so2 (Sulfur dioxide (SO2))
#'  \item co (Carbon monoxide (CO))
#'  \item nmvoc (Non-methane volatile organic compounds (NMVOC))
#'  \item pm10 (Particulate matter (PM10))
#'  \item pm1 (Particulate matter (PM1))
#'  \item cpc (Particulate number concentration (CPC))
#'  \item temp (Temperature (TEMP))
#'  \item prec (Precipitation (PREC))
#'  \item rad (Global radiation (RAD))
#' }
#'
#' One or several measurement \code{stations} have to be included
#' (default is to include all 16):
#' \enumerate{
#'  \item Bern-Bollwerk
#'  \item Lausanne-César-Roux
#'  \item Lugano-Università
#'  \item Zürich-Kaserne
#'  \item Basel-Binningen
#'  \item Dübendorf-Empa
#'  \item Härkingen-A1
#'  \item Sion-Aéroport-A9
#'  \item Magadino-Cadenazzo
#'  \item Payerne
#'  \item Tänikon
#'  \item Lägeren
#'  \item Chaumont
#'  \item Rigi-Seebodenalp
#'  \item Davos-Seehornwald
#'  \item Jungfraujoch
#' }
#'
#' For the time series measurement \code{interval}, you have a choice
#' between \code{hourly} and \code{daily} means
#' (default is \code{hourly}).
#'
#' The depicted time \code{period} can be the last \code{day}, \code{week},
#' \code{month}, or you can \code{free}ly choose the period by specifying
#' a start date with \code{from} and an end date with \code{to} (both need
#' to be given in a format that can be converted to an object of class
#' \code{Date} with \code{as.Date()} - e.g. in ISO 8601 format, i.e.
#' YYYY-MM-DD).
#'
#' @param pollutant pollutant or meteorological variable to plot
#' @param stations measurement stations (either by number or name) to include
#' @param interval plot \code{hourly} or \code{daily} means
#' @param period time series period
#' @param from start day of time series period (if \code{period = "free"})
#' @param to end day of time series period (if \code{period = "free"})
#' @return none (invisible \code{NULL})
#' @note Not all pollutants are measured at all stations.
#' @source \url{http://www.empa.ch/nabel/} and
#' \url{http://www.bafu.admin.ch/luft/luftbelastung/blick_zurueck/datenabfrage/index.html?lang=en}
#' @author Thomas Zumbrunn (\url{http://thomas.zumbrunn.name/})
#' @export
#' @import lattice
#' @import httpRequest
#' @examples
#' ## plot daily mean NO2 concentrations at three stations
#' ## from 1 January 2011 up to today
#' \dontrun{
#' nabel("no2",
#'       c("Bern", "Basel", "Lausanne"),
#'       "daily",
#'       "free",
#'       "2011-01-01",
#'       Sys.Date())
#' }
nabel <- function(pollutant = c("o3", "no2", "so2", "co", "nmvoc", "pm10", "pm1", "cpc", "temp", "prec", "rad"),
		stations = c("Bern-Bollwerk", "Lausanne-C\u00E9sar-Roux", "Lugano-Universit\u00E0", "Z\u00FCrich-Kaserne", "Basel-Binningen", "D\u00FCbendorf-Empa", "H\u00E4rkingen-A1", "Sion-A\u00E9roport-A9", "Magadino-Cadenazzo", "Payerne", "T\u00E4nikon", "L\u00E4geren", "Chaumont", "Rigi-Seebodenalp", "Davos-Seehornwald", "Jungfraujoch"),
		interval = c("hourly", "daily"),
		period = c("day", "week", "month", "free"),
		from = NULL,
		to = NULL) {
  
	## map parameters to HTML form parameters
	## (some of the actual HTML form parameters are the positions
	## of the terms in the following vector/lists)
	schadstoff <- list("o3" = "Ozone (O3)",
			"no2" = "Nitrogen dioxide (NO2)",
			"so2" = "Sulfur dioxide (SO2)",
			"co" = "Carbon monoxide (CO)",
			"nmvoc" = "Non-methane volatile organic compounds (NMVOC)",                     
			"pm10" = "Particulate matter (PM10)",
			"pm1" = "Particulate matter (PM1)",
			"cpc" = "Particulate number concentration (CPC)",
			"temp" = "Temperature (TEMP)",
			"prec" = "Precipitation (PREC)",
			"rad" = "Global radiation (RAD)")
	stationsliste <- c("Bern-Bollwerk",
			"Lausanne-C\u00E9sar-Roux",
			"Lugano-Universit\u00E0",
			"Z\u00FCrich-Kaserne",
			"Basel-Binningen",
			"D\u00FCbendorf-Empa",
			"H\u00E4rkingen-A1",
			"Sion-A\u00E9roport-A9",
			"Magadino-Cadenazzo",
			"Payerne",
			"T\u00E4nikon",
			"L\u00E4geren",
			"Chaumont",
			"Rigi-Seebodenalp",
			"Davos-Seehornwald",
			"Jungfraujoch")                      
	datentyp <- list("hourly" = "stunden",
			"daily" = "tag")
	zeitraum <- list("day" = "1tag",
			"week" = "1woche",
			"month" = "1monat",
			"free" = "frei")	
	
	## read arguments
	## (no thorough checking is done)
	pollutant <- match.arg(pollutant)
  stations <- unlist(stations)
	if (is.numeric(stations)){
		stations <- stationsliste[stations];
	} else {
		stations <- match.arg(stations, several.ok = TRUE);	
	}        
	interval <- match.arg(interval)
	period <- match.arg(period)
	if (period == "free") {
		from <- format(as.Date(from), "%d.%m.%Y")
		to <- format(as.Date(to), "%d.%m.%Y")
	}
	
	## get CSV file with HTTP POST
	params <- c("abfrageflag" = "true",
			"station" = "1",
			"nach" = "schadstoff",
			"ausgabe" = "csv",
			"submit" = "Query")
	params <- c(params, "schadstoff" = match(pollutant, names(schadstoff)))
	for (i in seq(along = stations)) {
		params <- c(params, "stationsliste[]" = match(stations[i], stationsliste))
	}
	params <- c(params, "datentyp" = datentyp[[interval]])
	params <- c(params, "zeitraum" = zeitraum[[period]])
	if (period == "free") {
		params <- c(params, "von" = from)
		params <- c(params, "bis" = to)
	}

	#First test that httpRequest is loaded outside of tryCatch
	postToHost <- postToHost;

	#http Request
	doc <- tryCatch(postToHost(host = "aurora.meteotest.ch",
					path = "/bafu/nabel/abfrage_neu/index.php/ausgabe/index/3",
					accept.charset = "ISO-8559-1",
					data.to.send = as.list(params)),
			error = function(e) stop(paste("No measurements on pollutant '", pollutant, "' available for station(s) ", paste(stations, collapse = ", "), ".", sep = "")),
			NULL)
	
	## convert character encoding to UTF-8 and read into data.frame
	docconv <- iconv(doc, from = "ISO-8859-1", to = "UTF-8")
	head <- readLines(textConnection(docconv, encoding = "UTF-8"), n = 30, encoding = "UTF-8")
	skip <- grep("Date/time;", head) - 1
	dat <- read.table(textConnection(docconv, encoding = "UTF-8"),
			encoding = "UTF-8",
			sep = ";",
			skip = skip,
			header = TRUE,
			fill = TRUE,
			as.is = TRUE,
			check.names = FALSE)
	dat <- dat[dat[, 1] != "0", ]
	
	## rename date/time variable and convert to long form
	names(dat)[1] <- "datetime"
	sta <- names(dat)[-1]
	if (interval == "hourly") {
		dat$datetime <- as.POSIXct(strptime(dat$datetime, format = "%d.%m.%Y %H:%M", tz = "CET"))
	} else {
		dat$datetime <- as.POSIXct(strptime(dat$datetime, format = "%d.%m.%Y", tz = "CET"))
	}
	dat <- reshape(dat,
			direction = "long",
			timevar = "station",
			varying = list(sta))
	names(dat)[2] <- "station"
	names(dat)[3] <- "measurement"
	dat$id <- NULL
	dat$station <- factor(dat$station, labels = sta)
	
	## create and print lattice plot
	ylabs <- list("o3" = expression(paste("Ozone (O3, in ", mu, "g/m", {}^3, ")")),
			"no2" = expression(paste("Nitrogen dioxide (NO2, in ", mu, "g/m", {}^3, ")")),
			"so2" = expression(paste("Sulfur dioxide (SO2, in ", mu, "g/m", {}^3, ")")),
			"co" = expression(paste("Carbon monoxide (CO, in mg/m", {}^3, ")")),
			"nmvoc" = "Non-methane volatile organic compounds (NMVOC, in ppm)",                      
			"pm10" = expression(paste("Particulate matter (PM10, in ", mu, "g/m", {}^3, ")")),
			"pm1" = expression(paste("Particulate matter (PM1, in ", mu, "g/m", {}^3, ")")),
			"cpc" = expression(paste("Particulate number concentration (CPC, in 1/cm", {}^3), ")"),
			"temp" = "Temperature (TEMP, in \u00B0C)",
			"prec" = "Precipitation (PREC, in mm)",
			"rad" = expression(paste("Global radiation (RAD, in W/m", {}^2, ")")))  
	pl <- xyplot(measurement ~ datetime, dat, groups = station,
			type = c("l"),
			auto.key = list(space = "right", points = FALSE, lines = TRUE),
			xlab = "date/time",
			ylab = ylabs[[pollutant]])
	print(pl)
}



\name{nabel}
\alias{nabel}
\title{Plot NABEL data}
\source{
  \url{http://www.empa.ch/nabel/} and
  \url{http://www.bafu.admin.ch/luft/luftbelastung/blick_zurueck/datenabfrage/index.html?lang=en}
}
\usage{
  nabel(pollutant = c("o3", "no2", "so2", "co", "nmvoc", "pm10", "pm1", "cpc", "temp", "prec", "rad"),
    stations = c("Bern-Bollwerk", "Lausanne-César-Roux", "Lugano-Università", "Zürich-Kaserne", "Basel-Binningen", "Dübendorf-Empa", "Härkingen-A1", "Sion-Aéroport-A9", "Magadino-Cadenazzo", "Payerne", "Tänikon", "Lägeren", "Chaumont", "Rigi-Seebodenalp", "Davos-Seehornwald", "Jungfraujoch"),
    interval = c("hourly", "daily"),
    period = c("day", "week", "month", "free"),
    from = NULL, to = NULL)
}
\arguments{
  \item{pollutant}{pollutant or meteorological variable to
  plot}

  \item{stations}{measurement stations (either by number or
  name) to include}

  \item{interval}{plot \code{hourly} or \code{daily} means}

  \item{period}{time series period}

  \item{from}{start day of time series period (if
  \code{period = "free"})}

  \item{to}{end day of time series period (if \code{period
  = "free"})}
}
\value{
  none (invisible \code{NULL})
}
\description{
  The National Air Pollution Monitoring Network (NABEL)
  provides time series data on air-borne pollutant
  concentrations from a network of measurement stations
  across Switzerland. The data can be queried with an HTML
  form (see sources). This is a simple function that serves
  as a wrapper for that HTML form. Its main purpose is to
  demonstrate some of the capabilities of
  \url{http://opencpu.org/} by Jeroen Ooms.
}
\details{
  A single \code{pollutant} or meteorological variable is
  plotted (default is \code{o3}): \enumerate{ \item o3
  (Ozone (O3)) \item no2 (Nitrogen dioxide (NO2)) \item so2
  (Sulfur dioxide (SO2)) \item co (Carbon monoxide (CO))
  \item nmvoc (Non-methane volatile organic compounds
  (NMVOC)) \item pm10 (Particulate matter (PM10)) \item pm1
  (Particulate matter (PM1)) \item cpc (Particulate number
  concentration (CPC)) \item temp (Temperature (TEMP))
  \item prec (Precipitation (PREC)) \item rad (Global
  radiation (RAD)) }

  One or several measurement \code{stations} have to be
  included (default is to include all 16): \enumerate{
  \item Bern-Bollwerk \item Lausanne-César-Roux \item
  Lugano-Università \item Zürich-Kaserne \item
  Basel-Binningen \item Dübendorf-Empa \item Härkingen-A1
  \item Sion-Aéroport-A9 \item Magadino-Cadenazzo \item
  Payerne \item Tänikon \item Lägeren \item Chaumont \item
  Rigi-Seebodenalp \item Davos-Seehornwald \item
  Jungfraujoch }

  For the time series measurement \code{interval}, you have
  a choice between \code{hourly} and \code{daily} means
  (default is \code{hourly}).

  The depicted time \code{period} can be the last
  \code{day}, \code{week}, \code{month}, or you can
  \code{free}ly choose the period by specifying a start
  date with \code{from} and an end date with \code{to}
  (both need to be given in a format that can be converted
  to an object of class \code{Date} with \code{as.Date()} -
  e.g. in ISO 8601 format, i.e. YYYY-MM-DD).
}
\note{
  Not all pollutants are measured at all stations.
}
\examples{
## plot daily mean NO2 concentrations at three stations
## from 1 January 2011 up to today
\dontrun{
nabel("no2",
      c("Bern", "Basel", "Lausanne"),
      "daily",
      "free",
      "2011-01-01",
      Sys.Date())
}
}
\author{
  Thomas Zumbrunn (\url{http://thomas.zumbrunn.name/})
}


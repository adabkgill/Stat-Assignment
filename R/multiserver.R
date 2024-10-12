#' Multiserver Queue System Simulation
#'
#' Simulates a multi-server queue system where customers are served based on their arrival and service times.
#' This function calculates the service start and end times for each customer based on the availability of servers.
#'
#' @param Arrivals A numeric vector of customer arrival times.
#' @param ServiceTimes A numeric vector of service times for each customer.
#' @param NumServers An integer specifying the number of servers available (default is 1).
#' @return A tibble containing the following columns:
#'   \describe{
#'     \item{Arrivals}{The time when each customer arrived.}
#'     \item{ServiceBegins}{The time when each customer's service began.}
#'     \item{ChosenServer}{The server that served each customer.}
#'     \item{ServiceEnds}{The time when each customer's service ended.}
#'   }
#' @examples
#' set.seed(2048)
#' arrival_time <- cumsum(rexp(100, 1/60))
#' service_time <- rexp(length(arrival_time), 1/150) + 20
#' Multiserver(arrival_time, service_time, 3)
#' @export
Multiserver <- function(Arrivals, ServiceTimes, NumServers = 1) {
  if (any(Arrivals <= 0 | ServiceTimes <= 0) || NumServers <= 0) {
    stop("Arrivals, ServiceTimes must be positive & NumServers must be positive")
  }
  if (length(Arrivals) != length(ServiceTimes)) {
    stop("Arrivals and ServiceTimes must have the same length")
  }
  NumCust <- length(Arrivals)
  AvailableFrom <- rep(0, NumServers)
  ChosenServer <- ServiceBegins <- ServiceEnds <- rep(0, NumCust)
  for (i in seq_along(Arrivals)) {
    avail <- min(AvailableFrom)
    ChosenServer[i] <- which.min(AvailableFrom)
    ServiceBegins[i] <- max(avail, Arrivals[i])
    ServiceEnds[i] <- ServiceBegins[i] + ServiceTimes[i]
    AvailableFrom[ChosenServer[i]] <- ServiceEnds[i]
  }
  out <- tibble::tibble(Arrivals, ServiceBegins, ChosenServer, ServiceEnds)
  return(out)
}

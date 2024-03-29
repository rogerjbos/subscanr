% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/subscan.R
\encoding{UTF-8}
\name{get_subscan_extrinsic}
\alias{get_subscan_extrinsic}
\title{get_subscan_extrinsic}
\usage{
get_subscan_extrinsic(network = "Karura", extrinsic_index)
}
\arguments{
\item{network}{string indicating which Polkadot endpoint to use; defaults to 'Karura'.}

\item{extrinsic_index}{string extrinsic id to pull.}
}
\value{
list
}
\description{
Get all events for a specific extrinsic id from the Subscan api
https://docs.api.subscan.io
}
\examples{
get_subscan_extrinsic(extrinsic_index = '398539-2')

}
\author{
Roger J. Bos, \email{roger.bos@gmail.com}
}
\concept{Get all events for a specific extrinsic id from the Subscan api}

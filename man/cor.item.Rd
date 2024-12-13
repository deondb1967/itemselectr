% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cor.item.R
\name{cor.item}
\alias{cor.item}
\title{Inter-item correlations}
\usage{
cor.item(x)
}
\arguments{
\item{x}{Data frame containing items that are all scored in the same direction}
}
\value{
A list containing (a) the inter-item correlation matrix, (b) for each item its average correlation with the remaining items, (c) the squared multiple correlation of each item with the remaining items, and (d) the average inter-item correlation across all the items, and (e) the median inter-item correlation across all the items.
}
\description{
Statistics related to the inter-item correlations of the items
}
\examples{
cor.item(work_stress[1:9])
}

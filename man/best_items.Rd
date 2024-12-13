% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/best_items.R
\name{best_items}
\alias{best_items}
\title{Fits all possible combinations of k items from a pool of n items to a single-factor model.}
\usage{
best_items(x, k)
}
\arguments{
\item{x}{A data frame containing the items.}

\item{k}{The number of items to be selected.}
}
\description{
The function can be useful in selecting items from a larger pool of items. The function allows for comparisons of all possible combinations of k items from the pool with respect to fit with a single factor model and reliability. The function works best if the number of possible unique combinations of items is less than 5000. It becomes extremely slow if there are more than 5000 possible combinations.
}
\examples{
# Say we want to select five items from pool of nine stress items
# The choose() function shows how many possible unique combinations there are
choose(ncol(work_stress[1:9]), 5)
out <- best_items(work_stress[1:9], 5)
out
}

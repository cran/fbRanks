\name{team.and.score.filters}
\alias{team.and.score.filters}
\title{ Helper Function to Determine Teams or Scores to Include }
\description{
  Helper function not exported for users.  This creates a vector of teams names that match the filter information passed in in ...  and a vector of TRUE, FALSE for each row of x$scores to include.  This function is used throughout the package whenever filtering on scores or teams columns is requested.
}
\usage{
team.and.score.filters(x, ...)
}
\arguments{
  \item{x}{ A list with elements \code{scores} which is a scores data frame with uniform team names and \code{teams} which is a teams data frame.  Typically is a fbRanks object in most function calls.  }
  \item{...}{ Other filters to apply when printing.  These must match column names in either \code{x$teams} or \code{x$scores}.  For example, if \code{x$teams} has a column named 'country' with values 'UK','Canada' and 'Germany', you can pass in \code{country="UK"} to only show UK ranks. }
 }

\value{
  A list with \code{include.teams}, a vector of team names, and \code{include.scores}, a vector of TRUE, FALSE for each row in x$scores.
}
\author{ 
  Eli Holmes, Seattle, USA.  

  eeholmes(at)u(dot)washington(dot)com
}

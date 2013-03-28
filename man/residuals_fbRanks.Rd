\name{residuals.fbRanks}
\alias{residuals}
\alias{residuals.fbRanks}
\title{ Residuals function for fbRanks Objects }
\description{
  The \code{\link{rank.teams}} function outputs fbRanks objects.  \code{residuals(fbRanks)}., where fbRanks is one's output from a \code{\link{rank.teams}} call, return the model residuals for each match in the \code{fbRanks$scores} data frame.
}
\usage{
\method{residuals}{fbRanks}(object, ...)
}
\arguments{
  \item{object}{ A fbRanks object.  }
  \item{...}{ Other filters to apply when returning residuals.  These must match column names in either \code{x$teams} or \code{x$scores}.  For example, if \code{x$teams} has a column named 'country' with values 'UK','Canada' and 'Germany', you can pass in \code{country="UK"} to only show UK residuals. }
 }

\value{
  A list of the residuals for each team.  It's rather verbose.  You probably want to assign it to a variable and work with that.  Look at \code{\link{predict.fbRanks}}.  That provides similar output in the $scores variable.
}
\author{ 
  Eli Holmes, Seattle, USA.  

  eeholmes(at)u(dot)washington(dot)com
}
\seealso{
\code{\link{predict.fbRanks}}
}
\examples{ 

data(B00data) #load a set of games
#fit a model using just the matches from the RCL D1 league
x=rank.teams(scores=B00.scores, teams=B00.teams, venue="RCL D1")

#Show the predicted versus actual scores
b00.resids=residuals(x)

\dontrun{
#you can compare to the residuals for a model to fit to all the data (including RCL D1)
x=rank.teams(scores=B00.scores, teams=B00.teams)

#Just a printing of the ranks with no extra info shown.
b00.resids=residuals(x, venue="RCL D1")
}
}
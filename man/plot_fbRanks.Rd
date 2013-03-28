\name{plot.fbRanks}
\alias{plot}
\alias{plot.fbRanks}
\title{ Plotting function for fbRanks Objects }
\description{
  The \code{\link{rank.teams}} function outputs fbRanks objects.  \code{plot(fbRanks)}., where fbRanks is one's output from a \code{\link{rank.teams}} call, will plot the rankings for each cluster of teams in the match data.  Specific teams, regions, or leagues can be labeled on the plots.
}
\usage{
\method{plot}{fbRanks}(x, ..., which = "residuals", annotate = list(title = TRUE))

}
\arguments{
  \item{x}{ A fbRanks object.  }
  \item{...}{ Extra elements to filter the ranks with, e.g. country. Must be column names in scores or teams dataframes.}
  \item{which}{ What to plot.  Options are "residuals" and "hist". If "hist" then annotate needs Name element.}
  \item{annotate}{ Info to add to title.  If title=TRUE, then the team name is added to the plot. }
}

\value{
  Some plots of team residuals and ranks relative to other teams.
}
\author{ 
  Eli Holmes, Seattle, USA.  

  eeholmes(at)u(dot)washington(dot)com
}
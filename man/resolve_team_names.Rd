\name{resolve.team.names}
\alias{resolve.team.names}
\title{ Helper Function to Create Uniform Team Names in Score Data Frame }
\description{
  Helper function not exported for users.  Used primarily in rank.teams() function to construct the team predictor variable.
}
\usage{
resolve.team.names(scores, team.resolver, team.data=NULL, use.team.select=TRUE)
}

\arguments{
  \item{scores}{ A scores data frame. }
  \item{team.resolver}{ A team name resolver.  It gives a unique team name (display name) associated with the team name used in leagues and tournaments. }
  \item{team.data}{ The team info data frame.  Only needed if team select GUI is used. }
  \item{use.team.select}{ Whether to call up the GUI \code{\link{team.name.select}} to select names missing from the team name resolver.} 
}

\details{  
  This replaces the team names in scores data frame with a unique name from the team name resolver (in name column).  It is used primarily in the \code{\link{create.fbRanks.dataframes}} function.  The \code{scores} element of a fbRanks object is the score data frame with the uniform names.  The \code{raw.scores} element is the scores with the original names (as used in the match).
}
\value{
Scores data frame with unique and uniform names for the teams in \code{home.team} and \code{away.team} columns.
}
\author{ 
  Eli Holmes, Seattle, USA.

  eeholmes(at)u(dot)washington(dot)com
}

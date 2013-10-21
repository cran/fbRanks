\name{B00data}
\alias{B00.scores}
\alias{B00.teams}
\alias{B00.founders}
\docType{data}
\title{ Match and Team Data for B00 Youth Soccer Matches }
\description{
  Example data set for soccer rating and prediction.  These are a set of ca 2000 select soccer matches for boys age Aug 2001 to Jul 2000 in Washington, Oregon, and British Columbia during the 2012-2013 season.
}
\usage{
data(B00data)
}
\format{
The data are supplied as a scores data frame and a teams data frame.  The score data frame is the raw match data taken from multiple leagues and tournaments and teams appear under slightly different names in different tournaments.  The teams data frame has the display name for a team and then a series of columns called alt.name.1, alt.name.2, etc with the alternate names used by the team.  Teams in this area compete in leagues governed by different associations and only play each other during open tournaments.  Teams within an association will meet teams from different leagues within the association cups.
}
\source{
The match results were recorded from the league and tournament websites during the 2012-2013 season.
}
\examples{
str(B00.scores)
}
\keyword{datasets}


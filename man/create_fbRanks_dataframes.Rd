\name{create.fbRanks.dataframes}
\alias{create.fbRanks.dataframes}
\title{ Helper Function to Construct the Scores and Teams Data Frames from csv files. }
\description{
  Helper function.  Reads in .csv files to create the scores, team.resolver, and teams data.frames.
}
\usage{
create.fbRanks.dataframes(scores.file, 
                          team.resolver=NULL, 
                          teams.file=NULL, 
                          date.format="\%Y-\%m-\%d")
}

\arguments{
  \item{scores.file}{ A single file name or vector of file names (csv files) which have the match data. Must have columns named "date","home.team","home.score", and "away.team","away.score".  Extra columns can be added, e.g. "surface" or "attack.adv" (home, away, neutral), and these can then be used as response variables.}
  \item{team.resolver}{ Optional.  A team name resolver.  It gives a unique team name (display name) associated with the team names appearing in the scores file home.team and away.team columns. This needs to have 2 columns.  One column is "name" and each name in this column must appear as in the "name" column of the teams.file.  The second column is "alt.name" and these are the names that appear in the scores file. If a team uses different names in different leagues and tournaments, then each alternate name is one row in the team.resolver file.  If not passed in, it will be constructed from the scores file home.team and away.team columns. }
  \item{teams.file}{ Optional. A single file name or vector of file names (csv files) which have the team names and team data. One column must be called "name" and is the display name for the team.  The names in the "name" column must match those appearing in the "name" column of the team.resolver file, however there can be extra teams in the team file that do not appear in the team.resolver file.  Features of teams, like age, state, club, etc, can also be added to the file and then can be used for filtering for printing or using as response variables.  If left off, a teams data.frame will be constructed from the home.team and away.team names in the scores file. }
  \item{date.format}{ What format the dates in the scores file are in.  Some common formats are "\%m/\%d\%Y" for 12/31/2012, "\%m/\%d/\%y" for 12/31/12, "\%d.\%m.\%Y" for 31.12.2012. The default is 2012-12-31.} 
}

\details{  
  This creates scores and teams dataframes from the scores files and team files
  If use.display.names=TRUE, then the names in scores data frame will be the display name in the teams file
}
\value{
This returns a list with elements \code{scores} and \code{teams} which are dataframes ready for the \code{\link{rank.teams}} function.  \code{scores} has the team names replaced with the display name, while \code{raw.scores} has the original names (in scores file).
}
\author{ 
  Eli Holmes, Seattle, USA.

  eeholmes(at)u(dot)washington(dot)com
  }
\examples{
\dontrun{
#This shows how a series of .csv files can be imported to create
#a scores and teams data frames

#this is the list of files with the league data
match.files = c(
  "OPL NPL.csv", "OPL 1st Div.csv",
  "OPL 2nd Div.csv", "OPL 3rd Div.csv","OPL 4th Div.csv",
)
#In this case the team file is a single file, but it could be a list
team.file="teams_b00.csv"
#This does error-checking and outputs dataframes in the proper format
b00_data=create.fbRanks.dataframes(scores.file=match.files, teams.file=team.file, date.format="%m/%d/%Y")

}
}

\name{team.name.select}
\alias{team.name.select}
\alias{edit_team_data}
\title{ Helper Function to Create Uniform Team Names in Score Data Frame }
\description{
  Helper function. Calls up a GUI to help enter team info. Requires tcltk package.  Only \code{edit.team.data} is exported.
}
\usage{
team.name.select(newname, team.resolver, team.data, scores, type="alt.name")
edit_team_data(team.resolver, team.data, scores)
}

\arguments{
  \item{newname}{ A list of unknown alt.names or display names. }
  \item{team.resolver}{ A team name resolver.  It gives a unique team name (display name) associated with the team name used in leagues and tournaments. }
  \item{team.data}{ The team info data frame.  Only needed if team select GUI is used. }
  \item{scores}{ A scores data frame. }
  \item{type}{ "alt.name"/"disp.name": Whether the newname is a list of bad alt.names (team name in match file that does not appear in the alt.name column of team resolver) or display names (name column of team resolver that does not match a name in the name column of the team file).} 
}

\details{  
  This calls up a GUI to use to select teams from the team data file to use for unknown alt.names (team names in the match file) or team resolver display names (in the name column of team resolver).  This will return updated data.frames for team.resolver and team.data.  These should be saved if you do not want to go through the process of entering team data again.
}
\value{
team.resolver and team.data data frames with with updated info.  ok and updated also returned to indicate that there were no errors and whether team.resolver and team.data were updated.
}
\author{ 
  Eli Holmes, Seattle, USA.

  eeholmes(at)u(dot)washington(dot)com
}

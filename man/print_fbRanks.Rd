\name{print.fbRanks}
\alias{print}
\alias{print.fbRanks}
\title{ Printing function for fbRanks Objects }
\description{
  The \code{\link{rank.teams}} function outputs fbRanks objects.  \code{print(fbRanks)}., where fbRanks is one's output from a \code{\link{rank.teams}} call, will print out the rankings for each cluster of teams in the match data.
}
\usage{
\method{print}{fbRanks}(x, ..., scaling="median total", base=2, 
        log.transform.attack=FALSE, header=TRUE, silent=FALSE, type="", file="")
}
\arguments{
  \item{x}{ A fbRanks object.  }
  \item{...}{ Other filters to apply when printing.  These must match column names in either \code{x$teams} or \code{x$scores}.  For example, if \code{x$teams} has a column named 'country' with values 'UK','Canada' and 'Germany', you can pass in \code{country="UK"} to only show UK ranks. }
  \item{scaling}{ How to scale the ranks.  By default, they are scaled with 0 at the median team in the database.  Pass in a number to scale to that number. }
  \item{base}{ By default, the ranks are shown base(2) but you can other bases.  Pass in exp(1) to change to base e.}
  \item{log.transform.attack}{ Whether to show the attack and defense scores as logged transformed.}
  \item{header}{ Whether to add some header information to the print out.}
  \item{type}{If type="html", html of the ranks table is output.}
  \item{file}{If type="html", then the output can be output to a file.}
  \item{silent}{ If FALSE no output but the ranks are returned.  }
 }

\value{
  A print out of team ranks.
}
\author{ 
  Eli Holmes, Seattle, USA.  

  eeholmes(at)u(dot)washington(dot)com
}
\examples{ 
#This loads two data frames: B00.scores and B00.teams
data(B00data)
#rank the teams in the RCL D1 league
x=rank.teams(scores=B00.scores, teams=B00.teams, venue="RCL D1")

#Just a printing of the ranks with no extra info shown.
#some 2 team clusters are shown.  Those are teams with just 1 game (against each other)
print(x)

\dontrun{
#rank all teams
x=rank.teams(scores=B00.scores, teams=B00.teams, venue="RCL D1")

#You can also do filtered prints
#print the ranks with the regions shown
print(x, region="all")

#print ranks for just a region
print(x, region="WA")

#print ranks for a league; fall.league is a column in the teams data frame
print(x, fall.league=c("RCL D1 U12","RCL D2 U12"))

#print ranks for teams in a tournament; venue is a column in the scores data frame
print(x, venue="Baker Blast")
}
}
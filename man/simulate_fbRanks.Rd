\name{simulate.fbRanks}
\alias{simulate}
\alias{simulate.fbRanks}
\title{ Simulate function for fbRanks Objects }
\description{
  The \code{\link{rank.teams}} function outputs fbRanks objects.  \code{simulate(fbRanks)}, where fbRanks is one's output from a \code{\link{rank.teams}} call, will simulate games and produce predicted standings based
  on different points rules.
}
\usage{
\method{simulate}{fbRanks}(
         object, 
         nsim=100000, 
         seed=NULL, ..., 
         newdata=list(home.team="foo", away.team="bar"),
         bracket.names=NULL,
         max.date="2100-6-1", min.date="1900-5-1", silent=FALSE,
         points.rule="tournament 10pt",tie.rule=list(tie.rule.gd.max=10),
         non.equal.games.rule=list(n.games=3,rule="proportional"),
         show.matches=FALSE, groups.column=NULL)
}
\arguments{
  \item{object}{ A fbRanks object.  }
  \item{...}{ Other arguments for to use for filtering the scores data.frame or newdata data.frame. See examples.}
  \item{newdata}{ An (optional) data.frame of data to use for predicting.  At the minimum home.team, away.team and any predictors in x (the fbRanks model) are required. You can also pass in arguments to specify that newdata
  be constructed from the scores data.frame in the fbRanks object. See examples.}
  \item{bracket.names}{An optional list with the team in each bracket.  If not used, all teams are assumed to be in the same bracket unless groups.column is passed in.  In that case, groups.column is used to set the brackets.  See examples.}
  \item{max.date, min.date}{ Used for filtering the scores data.frame or newdata data.frame.}
  \item{silent}{ whether to print anything }
  \item{ nsim }{ the number of simulations. }
  \item{ points.rule }{ A text rule or a list with pwin, pdraw, pshutout, pgoal, pgoal.max.  See details. }
    \item{ tie.rule }{ Currently only a GD rule with max GD per game is specified. }
  \item{ non.equal.games.rule }{ How to deal with teams with unequal number of games. }
  \item{ show.matches }{ whether to print the match predicitons }
  \item{ groups.column }{ if your scores data.frame or newdata data.frame has a column that indicates the bracket information.  See examples. }
  \item{ seed }{ not used }
}
\value{
  A simulation based on an estimated model of attack and defense strengths for each team.
}
\details{
The following points rules are available. \code{"tournament10pt"}: 6 points for a win, 
3 points for a draw, 1 point for each goal scored (up to a maximum of 3 per game for both teams),
and 1 point for a shutout.  In the event of a 0-0 tie, both teams are awarded 4 points.  \code{"league3pt"}: 3 points for a win, 1 points for a draw, 
0 point for each goal scored, and 0 point for a shutout.  A custom points rule can be specified by passing in a list with the elements 
pwin, pdraw, pshutout, pgoal, pgoal.max   to specify points for win, draw, shutout, and per goal
up  to a max of \code{pgoal.max} goals.

If teams have non-equal number of games, if \code{non.equal.games.rule="proportional"}, the default, 
then the total points are divided by the number of games played.

If specifying what to simulate using newdata, ensure that the data format is the same as that in \code{object$date.format}.  Also if there are any NA attack or defense strengths for your teams, \code{predict} will throw an error and exit.

}
\author{ 
  Eli Holmes, Seattle, USA.  
  eeholmes(at)u(dot)washington(dot)com
}
\examples{
#When specifying filters such as age, name, or league, these must correspond to column names in the 
#scores or team files.
data(B00data) #load a set of games
#fit a model using a particular date range for B00 teams
#add predictors surface and adv (home/away advantage)
\dontrun{
x=rank.teams(scores=B00.scores, teams=B00.teams, min.date="2012-5-1", max.date="2012-9-8",
    silent=TRUE, add=c("surface","adv"))

#simulate a league
#in this case, since newdata is not passed in, simulate tries to construct
#newdata from x$scores using the venue information
simulate(x, venue="RCL D1", points.rule="league3pt")
}

#simulate a tournament
#B00data includes a dataframe B00.founders with the brackets for
#the preliminary rounds of the Founders B00 2012 tournament
#fit a model with all data up to the start of the Founders Cup
\dontrun{
x=rank.teams(scores=B00.scores, teams=B00.teams, min.date="2012-5-1", max.date="2012-12-14",
    silent=TRUE, add=c("surface","adv"))
simulate(x, newdata=B00.founders, groups.column="venue")
}
}
\name{rank.teams}
\alias{rank.teams}
\title{ Team Ranks Via Poission Regression }
\description{
  Creates ranks using a dataframe of match records.  This, along with \code{\link{print.fbRanks}} and \code{\link{predict.fbRanks}}, are the main functions in the package.
}
\usage{
rank.teams(scores=NULL, teams=NULL, 
       family="poisson", fun="glm",
       max.date="2100-6-1", min.date="1900-5-1", date.format="\%Y-\%m-\%d",
       intercept=FALSE, time.weight.eta=0, add=NULL, silent=FALSE, ...)
}

\arguments{
  \item{scores}{  A data frame of match results.  Must have columns "date", "home.team", "home.score", "away.team", "away.score".  Missing scores must be denoted NaN.  Extra columns to be used for filtering in print results or as explanatory variables can be included, e.g. surface or attack.adv. }
  \item{teams}{  A data frame with the team data.  Must have columns "name" and "alt.name.x", where x can be anything, e.g. 1. Extra columns to be used for filtering in print results or as explanatory variables can be included, e.g. age.  None of the column names in the teams data frame are allowed to be the same as names in the scores data frame.}
  \item{family}{ Passed to glm or lmer. } 
  \item{fun}{ "glm" or "lmer" } 
  \item{max.date}{ Latest match date to use when fitting the model. }
  \item{min.date}{ Earliest match date to use when fitting the model. } 
  \item{date.format}{ The date formate for max.date, min.date and dates in the scores dataframe. } 
  \item{add}{ Vector of explanatory variables to add to the model. Must be a character vector that corresponds to names of columns in scores or teams dataframes.  }
  \item{intercept}{ Whether to include an intercept in the model. }
  \item{time.weight.eta}{ How much time weighting to include. 0 is no weighting.  0.1 would weight the most recent games quite strongly. }
  \item{silent}{ Suppresses printing. }
    \item{...}{ Other filters to apply when ranking.  These must match column names in either \code{teams} or \code{scores}.  For example, if \code{teams} has a column named 'country' with values 'UK','Canada' and 'Germany', you can pass in \code{country="UK"} to only rank using the matches for UK teams. }
}
\details{  
The function uses Dixon and Coles time-weighted poisson model to estimate attack and defense strengths using the \code{glm} function.  Extra explanatory variables (factors or continuous) can be added to the model.  The output is a print out of the attack+defense strengths (in total column) and the exp(attack) and exp(defense) strengths in separate columns.  Take the ratio of team A's attack strength to the team B's defense strength to get the expected goals scored by team A in a match between A and B.

The 2 raised to the difference in total strengths of two teams is the relative scoring rate of team A to team B.  Thus if the difference in total strength is 1 (team A - team B = 1) then team A scores 2^1 times faster than team B (in a match up between the two) and expected to score 2 goals for every 1 of team B.  If the diffence is 3, then team A is expected to score 2^3 = 8 goals for every 1 of team B. }
\value{
  A list of class \code{fbRanks} with the following components:
  \item{glm.fit}{ A list with the glm fit for each cluster, if family="lmer". }
  \item{lmer.fit}{ A list with the lmer fit for each cluster, if family="lmer". }
  \item{graph}{ A list with some information about the graph describing the interconnectedness of the teams from the \code{igraph} package.  The elements are graph (output from \code{graph.edgelist}), membership (output from \code{cluster}), csize (output from \code{cluster}), no (output from \code{cluster}), names (output from \code{get.vertex.attribute}).}
  \item{scores}{ The scores dataframe with all team names replaced with display names. }
  \item{teams}{ The teams data.frame. }
  \item{max.date}{ The most recent match used in model fit. }
  \item{min.date}{ The oldest match used in model fit. }
  \item{time.weight.eta}{ The time weighting used. }
  \item{date.format}{ The date format to use when displaying output using the fbRanks object. }
}
\references{ 
Dixon and Coles (1997) Modeling Association Football Scores and Inefficiencies in the Football Betting Market, Applied Statistics, Volume 46, Issue 2, 265-280
}
\author{ 
  Eli Holmes, Seattle, USA.

  eeholmes(at)u(dot)washington(dot)com
}
\seealso{ 
  \code{\link{print.fbRanks}}  
}
\examples{
#load the example data set
data(B00data)

#rank teams in the RCL D1 league using just the league data
x=rank.teams(scores=B00.scores, teams=B00.teams, venue="RCL D1")

#repeat with surface (turf, grass) as an explanatory variable
ranks2=rank.teams(scores=B00.scores, teams=B00.teams, venue="RCL D1", add=c("surface","adv"))

#Slightly fewer goals per game are scored on turf
coef(ranks2$fit$cluster.1)["surface.fTurf"]

#Slightly more goals per game are scored at home
coef(ranks2$fit$cluster.1)["adv.fhome"]

#get the ranks based on summer data
# x=rank.teams(scores=B00.scores, teams=B00.teams, min.date="2012-5-1", max.date="2012-9-8", silent=TRUE)

# See the vignette Basic Team Ranking for more examples

}

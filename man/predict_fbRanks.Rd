\name{predict.fbRanks}
\alias{predict}
\alias{predict.fbRanks}
\title{ Predict function for fbRanks Objects }
\description{
  The \code{\link{rank.teams}} function outputs fbRanks objects.  \code{predict(fbRanks)}, where fbRanks is one's output from a \code{\link{rank.teams}} call, will predict the result (win, loss, tie) and goals for a set of matches between teams.
}
\usage{
\method{predict}{fbRanks}(object, ..., 
       newdata=list(home.team="foo", away.team="bar"),
       max.date="2100-6-1", min.date="1900-5-1", 
       rnd=TRUE, silent=FALSE, show.matches=TRUE, 
       verbose=FALSE, remove.outliers=TRUE, n=100000)
}
\arguments{
  \item{object}{ A fbRanks object.  }
  \item{...}{ Other arguments for to use for filtering the scores data.frame or newdata data.frame. You can use any column in the scores or teams data frames. Name of the argument should be the column name and values passed in as a vector. See examples.}
  \item{newdata}{ A data.frame of data to use for predicting.  At the minimum home.team, away.team and any predictors in x (the fbRanks model) are required.}
  \item{max.date, min.date}{ Used for filtering the scores data.frame or newdata data.frame.}
  \item{rnd}{ whether to round the output of predictions. }
  \item{silent}{ whether to print anything }
  \item{ show.matches }{ whether to print the match predicitons }
  \item{ verbose }{ whether to print some prediction performances stats. }
  \item{ remove.outliers }{ whether to show predictions for teams whose estimates violate normality. }
  \item{ n }{ the number of simulate matches to use when computing the probabilities of win, tie, loss }
}
\value{
  A prediction for a set of matches based on an estimated model of attack and defense strengths for each team.
}
\author{ 
  Eli Holmes, Seattle, USA.  
  eeholmes(at)u(dot)washington(dot)com
}
\seealso{
\code{\link{rank.teams}}, \code{\link{simulate.fbRanks}}
}
\examples{
\dontrun{
#This will load two dataframes: B00.scores and B00.teams
data(B00data)

#fit a model using a particular date range for B00 teams
#set the data range to just be the summer games of WA teams
x=rank.teams(scores=B00.scores, teams=B00.teams, min.date="2012-5-1", max.date="2012-9-8", silent=TRUE)

#x is a fbRanks object with the fitted model and ranks, and the scores and teams data.frames
#the scores data.frames has all the data, not just the summer data

#Now we can use the summer data to predict the fall RCL D1 games (top B00 league in WA)
predict(x, venue="RCL D1")

#We can also predict all the games for a particular team
predict(x, name="Seattle United Copa B00")
}
}
\name{coef.fbRanks}
\alias{coef}
\alias{coef.fbRanks}
\title{ Return Coefficients from fbRanks Objects }
\description{
  The \code{\link{rank.teams}} function outputs fbRanks objects.  \code{coef(fbRanks)}, where fbRanks is one's output from a \code{\link{rank.teams}} call, will return a list with vectors of coefficients for each fit and lists of coefficients for each term in a model.  The fit element of a fbRanks object is a list with a fit (class "glm", "speedglm" or "glmnet") for each cluster.  A cluster is a collection of teams with a path, represented by a series of games, between every team in the cluster.  "attack" and "defend" are always model terms, but the user may have added on additional predictor variables (such as "surface").
}
\usage{
\method{coef}{fbRanks}(object, ...)

}
\arguments{
  \item{object}{ A fbRanks object.  }
  \item{...}{ Not used.  }
}

\value{
  A list with elements \code{coef.vector} and \code{coef.list}. \code{coef.vector} is like the familar vector returned by a coef() call except that these vectors are held in a list where each element of the list is for the fit for a different cluster.  \code{coef.list} is the vector separated into the coefficients for each model term.  At the minimum, \code{coef.list} is a list with elements "attack" and "defend" because these terms always appear in the fbRanks model.  Additional terms added by the user may also appear.
}
\author{ 
  Eli Holmes, Seattle, USA.  

  eeholmes(at)u(dot)washington(dot)com
}
\examples{
\dontrun{
#This will load two dataframes: B00.scores and B00.teams
data(B00data)

#fit a model using a particular date range for B00 teams
#set the data range to just be the summer games of WA teams
x=rank.teams(scores=B00.scores, teams=B00.teams, 
             min.date="2012-5-1", max.date="2012-9-8", silent=TRUE)

#there are multiple clusters
names(coef(x)$coef.list)

#show the coefficients for cluster 1 as a list
#notice that fewer goals are scored on Turf
coef(x)$coef.list[[1]]
}
}
\name{create.newdata.dataframe}
\alias{create.newdata.dataframe}
\title{ Helper Function to Replace Team Names in Score File with a Uniform Name from Team Data Frame. }
\description{
  Helper function not exported for users.  Used by predict.fbRanks() to construct the newdata data frame.
}
\usage{
create.newdata.dataframe(x, newdata, min.date, max.date, ...)
}

\arguments{
  \item{x}{ A fbRanks object.  }
  \item{newdata}{ A data.frame of data to use for predicting.  At the minimum home.team, away.team and any predictors in x (the fbRanks model) are required.}
  \item{max.date, min.date}{ Used for filtering the matches in newdata.}
  \item{...}{ Other arguments for to use for filtering the scores data.frame or newdata data.frame. You can use any column in the scores or teams data frames. Name of the argument should be the column name and values passed in as a vector.}
}

\details{  
  This is taking a list or data frame passed in in the \code{newdata} argument of a \code{\link{predict.fbRanks}} call and constructing a data frame that can be passed to glm or lmer.  It ensures that the uniform names (name column of \code{x$teams}) are used.  It uses \code{\link{resolve.team.names}}.
}
\value{
A list with elements \code{scores} and \code{teams}.
}
\author{ 
  Eli Holmes, Seattle, USA.

  eeholmes(at)u(dot)washington(dot)com
  }


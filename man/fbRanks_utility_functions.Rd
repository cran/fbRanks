\name{fbRanks.utility.functions}
\alias{str_strip.white}
\alias{str_proper}
\alias{str_remove.nonascii}
\alias{str_remove}
\alias{detect.normality.outliers}
\alias{scale.for.ranks}
\title{ Helper Functions in the fbRanks Package }
\description{
  Helper functions not exported for users.
}
\usage{
str_strip.white(string, sub = " ")
str_proper(string)
str_remove.nonascii(string, sub = "")
str_remove(string, start = 1L, end = -1L, sub = "")
detect.normality.outliers(x,alpha=0.05)
scale.for.ranks(type = 1, base = 2, silent = TRUE, str.disp.mult=1)
}

\arguments{
  \item{string}{ A character vector.  }
  \item{start, end}{ Location of where to start and end for removing characters. }
  \item{x}{ A vector of numbers}
  \item{alpha}{ A significance level. }
  \item{type}{ Type of scale. }
  \item{sub}{What to replace the removed characters with.}
  \item{base}{The base to which to scale the total strength.  By default it is base(2).  You want this to match the base used in your print call.}
  \item{silent}{ Whether to output the scale to the console (silent=FALSE). }
  \item{str.disp.mult}{ Multiply the strength numbers in the column names by this number. }
}

\details{  
  \code{str_strip.white} strips ending and internal extra white space.
  \code{str_proper} does proper capitalization.
  \code{str_remove.nonascii} remove non-ascii characters from strings.
  \code{str_remove} remove characters from strings.  Like sub but removes the specified characters between start and end.  
  \code{detect.normality.outliers} detects the values that violate normality to a particular alpha level.  
  \code{scale.for.ranks} prints a scale for fbRanks ratings. 
}
\value{
  \code{str_strip.white} returns a vector with the extra white space striped.
  
  \code{detect.normality.outliers} returns a vector of TRUE, FALSE values.
  
  \code{scale.for.ranks} will return the simulations used to construct the scale if the output is assigned to a variable.
  \code{team.name.select} calls up a GUI that allows the user to select the team name from a list or input a new team name.

}
\author{ 
  Eli Holmes, Seattle, USA.

  eeholmes(at)u(dot)washington(dot)com
  }


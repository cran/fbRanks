\name{construct.team.file}
\alias{construct.team.file}
\title{ Helper Function to Construct a Team File }
\description{
  Helper function.
}
\usage{
construct.team.file(scores.file="scores.csv", 
                    add.to.name="", 
                    file="team_template.csv", 
                    ignore="")
}

\arguments{
  \item{scores.file}{ A single file name or vector of file names which have the match data.  The function will throw an error if the column headers are not the same. }
  \item{file}{ Name to give your team file. }
  \item{ignore}{ A list of strings to ignore when matching teams.  See example. } 
  \item{add.to.name}{ Any text to append to the display names. }
}

\details{  
This will determine all the unique team names that differ only by 3 letters in the match files.  It will choose 1 as the display name and put the rest in the alt.name columns.  The result is a a template of the teams.csv file.  This will then need to be edited heavily.
}
\value{
Nothing is output.  Only a .csv file is written.
}
\author{ 
  Eli Holmes, Seattle, USA.

  eeholmes(at)u(dot)washington(dot)com
}
\examples{
\dontrun{
# A list of league match files to read in
file_list = c(
  "RCL D1.csv", "RCL D2.csv", "RCL D3.csv", "RCL D4.csv",
  "tournaments.csv")
ignore=c()
#use the current directory
construct.team.file(scores.file=file_list, file="team_template.csv", ignore=ignore)
}
}

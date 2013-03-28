\name{scrape.matches}
\alias{scrape.matches}
\alias{scrape.demosphere}
\alias{scrape.sportaffinity}
\alias{scrape.sportaffinity.brackets}
\alias{scrape.scoreboard}
\alias{scrape.korrio}
\alias{scrape.gotsport}
\alias{scrape.custom1}
\alias{scrape.custom2}
\alias{scrape.custom3}
\alias{scrape.custom4}
\alias{scrape.json1}
\alias{scrape.usclub}
\title{ Webscraping Match Scores }
\description{
  Webscrapers for various types of match score content managers. 
}
\usage{
scrape.korrio(url, file="Korrio", url.date.format="\%B \%Y \%a \%d", 
     date.format="\%Y-\%m-\%d", append=FALSE, get.surface=FALSE, ...)
scrape.demosphere(url, file="Demosphere", url.date.format="\%B \%d \%Y", 
     date.format="\%Y-\%m-\%d", append=FALSE, get.surface=FALSE, ...)
scrape.gotsport(url, file="GotSport", tb.num=10, url.date.format="\%m/\%d/\%Y", table.style=1,
     date.format="\%Y-\%m-\%d", append=FALSE, ...)
scrape.sportaffinity(url,file="SportAffinity", url.date.format="\%B \%d, \%Y", 
     date.format="\%Y-\%m-\%d", append=FALSE, ...)
scrape.sportaffinity.brackets(url, file, venue=NULL, ...)
scrape.scoreboard(html.file, file="ScoreBoard", url.date.format="\%a \%m/\%d/\%Y", 
     date.format="\%Y-\%m-\%d", append=FALSE, get.surface=FALSE, ...)
scrape.custom1(url, file="Custom1", weeks=NULL, first.td.tag=3, last.td.tag=7, td.per.row=5, 
    append=FALSE, ...)
scrape.custom2(url, file="Custom2", year=NULL, date.format="\%Y-\%m-\%d", append=FALSE, ...)
scrape.custom3(url, file="Custom3", year=NULL, date.format="\%Y-\%m-\%d", append=FALSE, ...)
scrape.custom4(url, file="Custom4", year=NULL, date.format="\%Y-\%m-\%d", append=FALSE, ...)
scrape.json1(url, file="Json1", date.format="\%Y-\%m-\%d", append=FALSE, ...)
scrape.usclub(url, file="USClub", url.date.format="\%A\%m/\%d/\%Y", 
    date.format="\%Y-\%m-\%d", append=FALSE, ...)
}

\arguments{
  \item{url}{ URL to the webpage with the match information. }
  \item{html.file}{ the html file(s) to scrape. }
  \item{file}{ file where the match data is to be saved.  Will be saved as a comma-delimined flat file.}
  \item{append}{ whether to append the match data to the existing file. } 
  \item{date.format}{ the date format to be used in the date column in the outputted match file. } 
  \item{get.surface}{ Some websites have surface (turf, grass) information and this can be scraped if desired. } 
  \item{tb.num}{ the table number to scrape.  Some websites put have the table in different places. } 
  \item{url.date.format}{ the date format on the webpage. } 
  \item{table.style}{ the table style.  Some content managers use different formats on different pages. } 
  \item{first.td.tag, last.td.tag, td.per.row}{ custom information for dealing with badly formed table html. } 
  \item{weeks}{ dates for webpages that show week number instead of a date. Must be in YYYY-mm-dd format.} 
  \item{year}{ The year to associate with the match dates since the match dates are not shown with the year on the webpage.}
  \item{venue}{ For scraping sport affinity brackets, the user should use the column name venue if the bracket name should be added to the score data.  Otherwise no bracket info is added, only the scores are scraped.}
  \item{...}{ Other columns to append to the match file.  For example, a column denoting the league or venue name.}
}

\details{
These webscrapers are customized for various match delivery platforms: Korrio, Demosphere, GotSport, SportAffinity and ScoreBoard.  Look at the bottom of the match website to determine the platform and thus scraper to use.  These scrapers will go out of date quickly as website structure is changed.  Thus you will probably need to modify the scrapers for your own purposes.  The custom1 scraper shows how to scrape a page with improperly formed html.  Custom2, Custom3 and Custom4 are for scores not provided by a standard content provider. The json1 scraper shows how to scrape JSON data.

Type the scraper name (e.g. scrape.custom1) to see a comments and a url example.
}
\value{
The scrapers output a comma-delimited file with the match data.  For examples, see the vignettes.
}
\author{ 
  Eli Holmes, Seattle, USA.
  ee(dot)holmes(at)u(dot)washington(dot)edu
}

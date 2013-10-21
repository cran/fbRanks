\name{scrape.matches}
\alias{scrape.matches}
\alias{scrape.demosphere}
\alias{scrape.demosphere.main}
\alias{scrape.sportaffinity}
\alias{scrape.sportaffinity.main}
\alias{scrape.sportaffinity.brackets}
\alias{scrape.scoreboard}
\alias{scrape.korrio}
\alias{scrape.gotsport}
\alias{scrape.gotsport.main}
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
    date.format="\%Y-\%m-\%d", table.style=1, year=NULL, append=FALSE, 
    get.surface=FALSE, ...)
scrape.demosphere.main(url, div.resolver, name="Demosphere", basedir=".", 
    url.date.format="\%B \%d \%Y", date.format="\%Y-\%m-\%d", U12=2001, 
    table.style=1, append=FALSE, get.surface=FALSE, ...)
scrape.gotsport(url, file="GotSport", tb.num=10, url.date.format="\%m/\%d/\%Y", 
    table.style=1, date.format="\%Y-\%m-\%d", append=FALSE, ...)
scrape.gotsport.main(url, name="test", basedir=".", tb.num=10, 
    url.date.format="\%m/\%d/\%Y", table.style=1, date.format="\%Y-\%m-\%d", 
    U12=2001, append=FALSE, ...)
scrape.sportaffinity(url,file="SportAffinity", url.date.format="\%B \%d, \%Y", 
    date.format="\%Y-\%m-\%d", append=FALSE, ...)
scrape.sportaffinity.brackets(url, file, venue=NULL, ...)
scrape.sportaffinity.main(url, name="SportAffinity", basedir=".", 
    url.date.format="\%B \%d, \%Y", date.format="\%Y-\%m-\%d", U12=2001, 
    append=FALSE, add.to.base.url="tour/public/info/", ..., 
    U.designation="Under ", name.delimiter="Under ", name.skip=3)
scrape.scoreboard(html.file, file="ScoreBoard", url.date.format="\%a \%m/\%d/\%Y", 
     date.format="\%Y-\%m-\%d", append=FALSE, get.surface=FALSE, ...)
scrape.custom1(url, file="Custom1", weeks=NULL, first.td.tag=3, last.td.tag=7, 
    td.per.row=5, append=FALSE, ...)
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
  \item{file}{ file where the match data is to be saved.  Will be saved as a comma-delimined flat file. This should include the directory if needed (i.e. not to be saved in the working directory.}
  \item{name}{ name of the file to be saved.  Needed when the name of the file need to be dynamically created for scrapers that scrape all age groups. }
  \item{basedir}{ base directory where gender-age files are to be saved. Needed when the name of the file need to be dynamically created for scrapers that scrape all age groups. }
  \item{U12}{ The year that a U12 player is associated with. Needed when multiple ages are being scraped and the gender-age must be computed. }
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
  \item{div.resolver}{For scrape.demosphere.main. tells what division names are on the Demosphere page for scraping the main page}
  \item{U.designation}{For scrape.sportaffinity.main. U.designation is the text right before the age (a 2 digit number);  so if the ages are denoted like "Boys Under 11",  U.designation = "Under " or "der ".  If ages were denoted "BU11", U.designation="U".}
  \item{name.delimiter}{For scrape.sportaffinity.main. name.delimiter and name.skip help form the division name. Say divisions are like "Boys Under 11 Foobar" and you want to use Foobar as the division. Then name.delimiter="Under " and name.skip=3.  This says the delimiter is "Under " and after that skip another 3 spaces to find the division name. If you want 11 Foobar as your division, use name.skip=0. If you want Under 11 Foobar, name.delimiter="Boys" name.skip=1}
  \item{name.skip}{For scrape.sportaffinity.main. See above.}
  \item{add.to.base.url}{For scrape.sportaffinity.main. The scraper constructs the urls for the individual ages.  It needs to know if it shoud add anything to the base url of the main page to the info it gets from the age links.}
\item{...}{ Other columns to append to the match file.  For example, a column denoting the league or venue name.}
}

\details{
These webscrapers are customized for various match delivery platforms: Korrio, Demosphere, GotSport, SportAffinity and ScoreBoard.  Look at the bottom of the match website to determine the platform and thus scraper to use.  These scrapers will go out of date quickly as website structure is changed.  Thus you will probably need to modify the scrapers for your own purposes.  The custom1 scraper shows how to scrape a page with improperly formed html.  Custom2, Custom3 and Custom4 are for scores not provided by a standard content provider. The json1 scraper shows how to scrape JSON data.

Type the scraper name (e.g. scrape.custom1) at the command line to see comments and a url example for each scraper.
}
\value{
The scrapers output a comma-delimited file with the match data.  For examples, see the vignettes.
}
\author{ 
  Eli Holmes, Seattle, USA.
  ee(dot)holmes(at)u(dot)washington(dot)edu
}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{lol_get_match_time}
\alias{lol_get_match_time}
\title{Get the match information}
\usage{
lol_get_match_time(region = "la1", match_id, API)
}
\arguments{
\item{region}{The region where is the summoner you want to query, it should be a character vector with length 1. Valid entries are:
BR1,EUN1    EUW1, JP1, KR, LA1, LA2, NA1, OC1, TR1, RU}

\item{match_id}{The match id you want to get information about. It must be a character vector with length 1.You can get this using function lol_get_matches()}

\item{API}{this is your personal API key, to get yours, please, visit the developer portal of Riot Games. The parameter must be a character vector with length 1. This API key expires every 24h.}
}
\value{
A list with two data.frames():
- First one contains:
- Second one contains:
}
\description{
This function allows you to get the more granular data of a specific match, this contains data about its participants, winners, gold earned, K/D/A, events by time frame, etc...
}
\keyword{division,}
\keyword{leagueoflegends}
\keyword{queue,}
\keyword{rank,}
\keyword{region,}
\keyword{summoners,}

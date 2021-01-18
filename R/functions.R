#' Get Summoners from a specific region, division, queue, and tier
#'
#' This function allows you to get the data from some summoners of the specified region, division, queue and tier.
#'
#' @param region The region you want to query, it should be a character vector with length 1. Valid entries are:
#' BR1,EUN1	EUW1, JP1, KR, LA1, LA2, NA1, OC1, TR1, RU
#' @param queue Map or game mode you want to query, it must be a character vector with length 1. Valid entries are:
#' RANKED_SOLO_5x5, RANKED_TEAM_5x5
#' @param division The division you want to query the, it must be a character vector with length 1. Valid entries are:
#' DIAMOND, PLATINUM. GOLD, SILVER, BRONZE, IRON
#' @param rank This is refered to the tier you want to query, it must be a character vector with length 1. Valid entries are:
#' I, II, III, IV
#' @param API this is your personal API key, to get yours, please, visit the developer portal of Riot Games. The parameter must be a character vector with length 1. This API key expires every 24h.
#'
#' @keywords summoners, region, queue, division, rank, leagueoflegends
#' @export
#' @return A data.frame with columns:
#' @example
#' ## Not run
#' lol_get_summoners_data(region="la1", queue="RANKED_SOLO_5x5", division="DIAMOND", rank="I" , API)

lol_get_summoners_data <- function(region="la1", queue="RANKED_SOLO_5x5", division="DIAMOND", rank="I" , API){
  https <- paste("https://",
                 region,
                 ".api.riotgames.com/lol/league-exp/v4/entries/",
                 queue, "/", division, "/", rank, "?page=1&api_key=",
                 API, sep = ""
  )
  data <- GET(https)
  data <- httr::content(data)

  data <- map(data, .f = function(x){data.frame(x[c(1:13)])})
  data <- do.call(rbind, data)
  return(data)
}


#' Get data from a specific summoner
#'
#' This function allows you to get the data from a summoner based on its summoner name and region.
#'
#' @param region The region where is the summoner you want to query, it should be a character vector with length 1. Valid entries are:
#' BR1,EUN1	EUW1, JP1, KR, LA1, LA2, NA1, OC1, TR1, RU
#' @param summoner_name The summoner name you want to get information about. It must be a character vector with length 1.
#' @param API this is your personal API key, to get yours, please, visit the developer portal of Riot Games. The parameter must be a character vector with length 1. This API key expires every 24h.
#'
#' @keywords summoners, region, queue, division, rank, leagueoflegends
#' @export
#' @return A data.frame with columns:
#' @example
#' ## Not run
#' lol_get_summoner_data(region="la1", "JohanLAG", API)

lol_get_summoner_data <- function(region="la1", summoner_name, API){
  summoner_name <- str_replace_all(summoner_name, " ","%20")
  https= paste("https://",region,".api.riotgames.com/lol/summoner/v4/summoners/by-name/",summoner_name,"?api_key=",API, sep="")
  data <- as.data.frame(httr::content(GET(https)))
  return(data)
}

#' Get the champion masteries of a summoner
#'
#' This function allows you to get the data all the champion masteries that a summoner owns based on its account ID (this can be get with lol_get_summoner_data() ).
#'
#' @param region The region where is the summoner you want to query, it should be a character vector with length 1. Valid entries are:
#' BR1,EUN1	EUW1, JP1, KR, LA1, LA2, NA1, OC1, TR1, RU
#' @param summoner_id The summoner id you want to get information about. It must be a character vector with length 1.
#' @param API this is your personal API key, to get yours, please, visit the developer portal of Riot Games. The parameter must be a character vector with length 1. This API key expires every 24h.
#'
#' @keywords summoners, region, queue, division, rank, leagueoflegends
#' @export
#' @return A data.frame with columns:
#' @example
#' ## Not run
#' lol_get_summoner_data(region="la1", summoner_id, API)

lol_get_masteries <- function(region="la1", summoner_id, API){
  https = paste("https://",
                region,
                ".api.riotgames.com/lol/champion-mastery/v4/champion-masteries/by-summoner/",
                summoner_id,
                "?api_key=",
                API, sep="")
  data <- httr::content(GET(https))
  data <- map(data, .f = function(x){data.frame(x[c(1:9)])})
  data <- do.call(rbind, data)
  return(data)
}

#' Get a list of matches of a summoner
#'
#' This function allows you to get the data of the matches of the summoner based on its account ID (this can be get with lol_get_summoner_data() ).
#'
#' @param region The region where is the summoner you want to query, it should be a character vector with length 1. Valid entries are:
#' BR1,EUN1	EUW1, JP1, KR, LA1, LA2, NA1, OC1, TR1, RU
#' @param acc_id The account id you want to get information about. It must be a character vector with length 1.
#' @param API this is your personal API key, to get yours, please, visit the developer portal of Riot Games. The parameter must be a character vector with length 1. This API key expires every 24h.
#'
#' @keywords summoners, region, queue, division, rank, leagueoflegends
#' @export
#' @return A data.frame with columns:
#' @example
#' ## Not run
#' lol_get_matches(region="la1", acc_id, API)

lol_get_matches <- function(region="la1", acc_id=acc_id, API){
  https = paste(
    "https://",region,".api.riotgames.com/lol/match/v4/matchlists/by-account/",acc_id,"?api_key=",API, sep="")
  data <- httr::content(httr::GET(https))[[1]]

  data <- map(data, .f = function(x){data.frame(x[c(1:8)])})
  data <- do.call(rbind, data)
  return(data)
}

#' Get the match information
#'
#' This function allows you to get the data of a specific match, this contains data about its participants, winners, gold earned, K/D/A, etc...
#'
#' @param region The region where is the summoner you want to query, it should be a character vector with length 1. Valid entries are:
#' BR1,EUN1	EUW1, JP1, KR, LA1, LA2, NA1, OC1, TR1, RU
#' @param match_id The match id you want to get information about. It must be a character vector with length 1.You can get this using function lol_get_matches()
#' @param API this is your personal API key, to get yours, please, visit the developer portal of Riot Games. The parameter must be a character vector with length 1. This API key expires every 24h.
#' @param timeline_data logical (delfault is TRUE) if you want a summary of the match by time frame
#'
#' @keywords summoners, region, queue, division, rank, leagueoflegends
#' @export
#' @return A data.frame with columns:
#' @example
#' ## Not run
#' lol_get_match_info(region="la1", match_id, API, timeline_data)

lol_get_match_info <- function(region = "la1", match_id, API, timeline_data=T){
  https <- paste("https://",region,".api.riotgames.com/lol/match/v4/matches/",match_id,"?api_key=",API, sep="")
  data <- httr::content(httr::GET(https))
  game_info <- unlist(data[c(1:10)])

  #Team agg inf
  team_info <- do.call(rbind, map(data$teams, .f = function(x){data.frame(x[c(1:15)])}))
  bans <- data$teams$bans

  ##Participant details
  participant <- data$participants
  participant_info <- map_dfr(participant, .f = function(x){data.frame(x[c(1:5)])})
  participant_stats <- map_dfr(participant, .f = function(x){data.frame(x$stats)})
  participant_timeline <- map_dfr(
    map_depth(participant,
              .depth = 1 ,
              .f = function(x) {
                t(list.rbind(x$timeline))
              }),
    .f = function(x){data.frame(x)})

  participant_timeline <- tibble::rownames_to_column(participant_timeline, "Time") %>%
    mutate(Time = gsub("\\..*","", Time))
  if(is.null(try(participant_timeline$participantId))){return(NULL)}else{}
  participant_timeline$participantId <- as.integer(participant_timeline$participantId)


  participant_id <- data$participantIdentities
  participant_id <- imap_dfr(participant_id,
                             .f = function(x, .y){data.frame(x$player, player_id = .y)} )
  #Joining data
  data <- participant_info %>%
    left_join(participant_stats, by =c("participantId" = "participantId")) %>%
    left_join(participant_id, by =c("participantId" = "player_id")) %>%
    left_join(participant_timeline %>%
                distinct(participantId, role, lane) %>%
                mutate(participantId = as.integer(participantId)),
              by=c("participantId" = "participantId"))


  data_timeline <- data %>%
    left_join(participant_timeline, by = c("participantId" = "participantId"))


  ifelse(timeline_data==T,
         return(data_timeline),
         return(data))
}


#' Get the match information
#'
#' This function allows you to get the more granular data of a specific match, this contains data about its participants, winners, gold earned, K/D/A, events by time frame, etc...
#'
#' @param region The region where is the summoner you want to query, it should be a character vector with length 1. Valid entries are:
#' BR1,EUN1	EUW1, JP1, KR, LA1, LA2, NA1, OC1, TR1, RU
#' @param match_id The match id you want to get information about. It must be a character vector with length 1.You can get this using function lol_get_matches()
#' @param API this is your personal API key, to get yours, please, visit the developer portal of Riot Games. The parameter must be a character vector with length 1. This API key expires every 24h.
#'
#' @keywords summoners, region, queue, division, rank, leagueoflegends
#' @export
#' @return A list with two data.frames():
#' - First one contains:
#' - Second one contains:
#' @example
#' ## Not run
#' lol_get_match_time(region="la1", match_id, API, timeline_data)

lol_get_match_time <- function(region = "la1", match_id, API){
  https = paste("https://", region, ".api.riotgames.com/lol/match/v4/timelines/by-match/", match_id, "?api_key=", API,
                sep = "")
  data <- httr::content(httr::GET(https))
  data_frames <- data$frames[c(1:(length(data$frames)-1))]
  data_participant <- map_depth(lapply(data_frames,'[[',1),.depth=2,
                                .f = function(x){data.frame(x)}) %>%
    map(list.rbind) %>%
    imap_dfr(.f = function(x, .y){data.frame(x, Time_frame = .y)})
  last_time_frame <- data$frames[c(length(data$frames))]
  last_time_frame <- map_depth(lapply(last_time_frame,'[[',1),.depth=2,
                               .f = function(x){data.frame(x)}) %>%
    map(list.rbind) %>%
    imap_dfr(.f = function(x, .y){data.frame(x, Time_frame = length(data$frames))})

  data_participants <- data_participant %>%
    plyr::rbind.fill(last_time_frame)

  events <- lapply(data$frames,'[[',2)
  if(is.null(try(events[[1]]))){return(list(data_participants= NULL, data_events = NULL, frame_interval = NULL))}
  if(length(events[[1]])==0){events <- events[c(2:length(events))]}

  events_df <- do.call(plyr::rbind.fill,
                       imap(
                         map(
                           map_depth(events,
                                     .depth=2,
                                     .f = function(x){data.frame(t(unlist(x)))}),
                           plyr::rbind.fill),
                         .f=function(x, .y){cbind(x, time_frame = .y)})
  )

  cols <- c(assistingParticipantIds =NA_real_, assistingParticipantIds1=NA_real_, assistingParticipantIds2 = NA_real_, assistingParticipantIds3 = NA_real_, type = NA_real_, timestamp = NA_real_, time_frame = NA_real_ ,
            participantId = NA_real_,assistingParticipantIds = NA_real_ ,itemId = NA_real_, skillSlot = NA_real_, levelUpType = NA_real_, wardType = NA_real_, creatorId = NA_real_,
            position.x = NA_real_, position.y = NA_real_, killerId = NA_real_, victimId = NA_real_, afterId = NA_real_, beforeId = NA_real_, monsterType = NA_real_, monsterSubType = NA_real_,
            teamId = NA_real_, buildingType = NA_real_, laneType = NA_real_, towerType = NA_real_)

  events_df <- events_df %>% add_column(!!!cols[setdiff(names(cols), names(events_df))]) %>%
    mutate(assistingParticipant_Ids = paste(assistingParticipantIds, assistingParticipantIds1, assistingParticipantIds2, assistingParticipantIds3, sep=", ")
    ) %>%
    select(type, timestamp, time_frame ,
           participantId,assistingParticipantIds ,itemId, skillSlot, levelUpType, wardType, creatorId,
           position.x, position.y, killerId, victimId, afterId, beforeId, monsterType, monsterSubType,
           teamId, buildingType, laneType, towerType)

  return(list(data_participants= data_participants, data_events = events_df, frame_interval = data$frameInterval))

}

#' Get general information of all the champions
#'
#' This function allows you to get general information of each one the champions of league of legends, this contains its id, rol data, stats and others.
#'
#' @param version Version of the list of champs, default = 11.1.1. Check https://developer.riotgames.com/docs/lol#data-dragon_champions to get more information
#' @param language The language you want to be some character vector such as dialogs. Default is "en_US"
#' @keywords summoners, region, queue, division, rank, leagueoflegends
#' @export
#' @return A data.frame with columns:
#' @example
#' ## Not run
#' lol_get_data_champs(version="11.1.1", language = "en_US")

lol_get_data_champs <- function(version="11.1.1", language = "en_US"){
  data_champs <- content(
    GET(
      paste(
        "http://ddragon.leagueoflegends.com/cdn/", version, "/data/", language, "/champion.json", sep=""
      )
    )
  )
  data_champs <- data_champs$data %>%
    map_dfr(data.frame) %>%
    mutate(tags = paste(tags..Mage., tags..Assassin., tags..Support., tags..Marksman., tags..Fighter., tags..Tank.,
                        X.Assassin., X.Mage., X.Marksman., X.Fighter., X.Support., X.Tank., sep=",")) %>%
    select(id, key, name, title, blurb, info.attack, info.defense, info.magic, info.difficulty, partype,
           stats.hp, stats.hpperlevel, stats.mp, stats.mpperlevel, stats.movespeed, stats.armor, stats.armorperlevel,
           stats.spellblock, stats.spellblockperlevel, stats.attackrange, stats.hpregen, stats.hpregenperlevel, stats.mpregen,
           stats.mpregenperlevel, stats.crit, stats.critperlevel, stats.attackdamage, stats.attackdamageperlevel,
           stats.attackspeedperlevel, stats.attackspeed, tags) %>%
    mutate(tags = str_replace_all(str_remove_all(tags, "NA")," ", ""))
  colnames(data_champs) <- str_remove_all(str_remove_all(colnames(data_champs), "stat.."),"info.")
  return(data_champs)
}

#' Get information of a champion
#'
#' This function allows you to get information of a specific champion.
#'
#' @param version Version of the list of champs, default = 11.1.1. Check https://developer.riotgames.com/docs/lol#data-dragon_champions to get more information
#' @param language The language you want to be some character vector such as dialogs. Default is "en_US"
#' @keywords summoners, region, queue, division, rank, leagueoflegends
#' @export
#' @return A data.frame with columns:
#' @example
#' ## Not run
#' lol_get_data_champ(version="11.1.1", language = "en_US", champion_name ="Nasus")

lol_get_data_champ <- function(version="11.1.1", language = "en_US", champion_name){
  data_champs <- content(
    GET(
      paste(
        "http://ddragon.leagueoflegends.com/cdn/", version, "/data/", language, "/champion/",  champion_name,".json", sep=""
      )
    )
  )
  data_spells <- data_champs$data[[1]]$spells %>% map_dfr(.f=function(x){data.frame(x[c(1:4, 6, 8, 10)])})
  return(data_spells)
}


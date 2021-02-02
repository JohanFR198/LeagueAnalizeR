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
#' @import   httr
#' @import dplyr
#' @import stringr
#' @import plyr
#' @import purrr
#' @importr list
#' @import reshape2
#' @import tibbl

lol_get_summoners_data <- function(region="la1", queue="RANKED_SOLO_5x5", division="DIAMOND", rank="I" , API){
  data <- paste("https://",
                 region,
                 ".api.riotgames.com/lol/league-exp/v4/entries/",
                 queue, "/", division, "/", rank, "?page=1&api_key=",
                 API, sep = ""
  ) %>%
    httr::GET() %>%
    httr::content(as="text") %>%
    jsonlite::fromJSON()
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
#' @import   httr
#' @import dplyr
#' @import stringr
#' @import plyr
#' @import purrr
#' @importr list
#' @import reshape2
#' @import tibbl

lol_get_summoner_data <- function(region="la1", summoner_name="JohanLAG", API=KEY){
  summoner_name <- stringr::str_replace_all(summoner_name, " ","%20")
  https <-  paste("https://",region,".api.riotgames.com/lol/summoner/v4/summoners/by-name/",summoner_name,"?api_key=",API, sep="")
  data <- as.data.frame(httr::content(httr::GET(https)))
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
#' @import   httr
#' @import dplyr
#' @import stringr
#' @import plyr
#' @import purrr
#' @importr list
#' @import reshape2
#' @import tibbl

lol_get_masteries <- function(region="la1", summoner_id="6Om8SoTmGhqgZJ6MDFeBhRxiROgwB4ZZZVwQiqc6XVUcZw", API=KEY){
  https = paste("https://",
                region,
                ".api.riotgames.com/lol/champion-mastery/v4/champion-masteries/by-summoner/",
                summoner_id,
                "?api_key=",
                API, sep="") %>%
    httr::GET() %>%
    httr::content(as="text") %>%
    jsonlite::fromJSON()
  return(https)
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
#' @import   httr
#' @import dplyr
#' @import stringr
#' @import plyr
#' @import purrr
#' @importr list
#' @import reshape2
#' @import tibbl

lol_get_matches <- function(region="la1", acc_id="jkBkWnhZTYCyNR4ivxgSdtOttGqRp0Q9igtrmRwSBBfvK2M", API=KEY){
  https = paste(
    "https://",region,".api.riotgames.com/lol/match/v4/matchlists/by-account/",acc_id,"?api_key=",API, sep="") %>%
    httr::GET() %>%
    httr::content(as="text") %>%
    jsonlite::fromJSON()
  https[[1]] %>% mutate(totalGames=https[[4]])

  return(https)
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
#' @import   httr
#' @import dplyr
#' @import stringr
#' @import plyr
#' @import purrr
#' @importr list
#' @import reshape2
#' @import tibbl

lol_get_match_info <- function(region = "la1", match_id, API, timeline_data=T){
  https <- paste("https://",region,".api.riotgames.com/lol/match/v4/matches/",match_id,"?api_key=",API, sep="")
  data <- httr::content(httr::GET(https))
  game_info <- unlist(data[c(1:10)])

  #Team agg inf
  team_info <- do.call(rbind, purrr::map(data$teams, .f = function(x){data.frame(x[c(1:15)])}))
  bans <- data$teams$bans

  ##Participant details
  participant <- data$participants
  participant_info <- purrr::map_dfr(participant, .f = function(x){data.frame(x[c(1:5)])})
  participant_stats <- purrr::map_dfr(participant, .f = function(x){data.frame(x$stats)})
  participant_timeline <- purrr::map_dfr(
    purrr::map_depth(participant,
              .depth = 1 ,
              .f = function(x) {
                t(rlist::list.rbind(x$timeline))
              }),
    .f = function(x){data.frame(x)})

  participant_timeline <- tibble::rownames_to_column(participant_timeline, "Time") %>%
    mutate(Time = gsub("\\..*","", Time))
  if(is.null(try(participant_timeline$participantId))){return(NULL)}else{}
  participant_timeline$participantId <- as.integer(participant_timeline$participantId)


  participant_id <- data$participantIdentities
  participant_id <- purrr::imap_dfr(participant_id,
                             .f = function(x, .y){data.frame(x$player, player_id = .y)} )
  #Joining data
  data <- participant_info %>%
    dplyr::left_join(participant_stats, by =c("participantId" = "participantId")) %>%
    dplyr::left_join(participant_id, by =c("participantId" = "player_id")) %>%
    dplyr::left_join(participant_timeline %>%
                dplyr::distinct(participantId, role, lane) %>%
                dplyr::mutate(participantId = as.integer(participantId)),
              by=c("participantId" = "participantId"))


  data_timeline <- data %>%
    dplyr::left_join(participant_timeline, by = c("participantId" = "participantId"))


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
#' @import   httr
#' @import dplyr
#' @import stringr
#' @import plyr
#' @import purrr
#' @importr list
#' @import reshape2
#' @import tibbl

lol_get_match_time <- function(region = "la1", match_id ="5019682262", API = API){
  https = paste("https://", region, ".api.riotgames.com/lol/match/v4/timelines/by-match/", match_id, "?api_key=", API,
                sep = "")
  data <- httr::content(httr::GET(https))
  data_frames <- data$frames[c(1:(length(data$frames)-1))]
  data_participant <- purrr::map_depth(lapply(data_frames,'[[',1),.depth=2,
                                .f = function(x){data.frame(x)}) %>%
    purrr::map(rlist::list.rbind) %>%
    purrr::imap_dfr(.f = function(x, .y){data.frame(x, Time_frame = .y)})
  last_time_frame <- data$frames[c(length(data$frames))]
  last_time_frame <- purrr::map_depth(lapply(last_time_frame,'[[',1),.depth=2,
                               .f = function(x){data.frame(x)}) %>%
    purrr::map(rlist::list.rbind) %>%
    purrr::imap_dfr(.f = function(x, .y){data.frame(x, Time_frame = length(data$frames))})

  data_participants <- data_participant %>%
    plyr::rbind.fill(last_time_frame)

  events <- lapply(data$frames,'[[',2)
  if(is.null(try(events[[1]]))){return(list(data_participants= NULL, data_events = NULL, frame_interval = NULL))}
  if(length(events[[1]])==0){events <- events[c(2:length(events))]}

  events_df <- do.call(plyr::rbind.fill,
                       purrr::imap(
                         purrr::map(
                           purrr::map_depth(events[lapply(events, length)>0],
                                            .depth=2,
                                            .f = function(x){data.frame(t(unlist(x)))}),
                           plyr::rbind.fill),
                         .f=function(x, .y){cbind(x, time_frame = .y)})
  )

  cols <- c(assistingParticipantIds =NA_real_, assistingParticipantIds1=NA_real_, assistingParticipantIds2 = NA_real_, assistingParticipantIds3 = NA_real_, type = NA_real_, timestamp = NA_real_, time_frame = NA_real_ ,
            participantId = NA_real_,assistingParticipantIds = NA_real_ ,itemId = NA_real_, skillSlot = NA_real_, levelUpType = NA_real_, wardType = NA_real_, creatorId = NA_real_,
            position.x = NA_real_, position.y = NA_real_, killerId = NA_real_, victimId = NA_real_, afterId = NA_real_, beforeId = NA_real_, monsterType = NA_real_, monsterSubType = NA_real_,
            teamId = NA_real_, buildingType = NA_real_, laneType = NA_real_, towerType = NA_real_)

  events_df <- events_df %>% tibble::add_column(!!!cols[setdiff(names(cols), names(events_df))]) %>%
    dplyr::mutate(assistingParticipant_Ids = paste(assistingParticipantIds, assistingParticipantIds1, assistingParticipantIds2, assistingParticipantIds3, sep=", ")
    ) %>%
    dplyr::select(type, timestamp, time_frame ,
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
#' @import   httr
#' @import dplyr
#' @import stringr
#' @import plyr
#' @import purrr
#' @importr list
#' @import reshape2
#' @import tibbl

lol_get_data_champs <- function(version="11.1.1", language = "en_US"){
  data_champs <- httr::content(
    httr::GET(
      paste(
        "http://ddragon.leagueoflegends.com/cdn/", version, "/data/", language, "/champion.json", sep=""
      )
    )
  )
  data_champs <- data_champs$data %>%
    purrr::map_dfr(data.frame) %>%
    dplyr::mutate(tags = paste(tags..Mage., tags..Assassin., tags..Support., tags..Marksman., tags..Fighter., tags..Tank.,
                        X.Assassin., X.Mage., X.Marksman., X.Fighter., X.Support., X.Tank., sep=",")) %>%
    dplyr::select(id, key, name, title, blurb, info.attack, info.defense, info.magic, info.difficulty, partype,
           stats.hp, stats.hpperlevel, stats.mp, stats.mpperlevel, stats.movespeed, stats.armor, stats.armorperlevel,
           stats.spellblock, stats.spellblockperlevel, stats.attackrange, stats.hpregen, stats.hpregenperlevel, stats.mpregen,
           stats.mpregenperlevel, stats.crit, stats.critperlevel, stats.attackdamage, stats.attackdamageperlevel,
           stats.attackspeedperlevel, stats.attackspeed, tags) %>%
    dplyr::mutate(tags = stringr::str_replace_all(stringr::str_remove_all(tags, "NA")," ", ""))
  colnames(data_champs) <- stringr::str_remove_all(stringr::str_remove_all(colnames(data_champs), "stat.."),"info.")
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
#' @import   httr
#' @import dplyr
#' @import stringr
#' @import plyr
#' @import purrr
#' @importr list
#' @import reshape2
#' @import tibbl

lol_get_data_champ <- function(version="11.1.1", language = "en_US", champion_name){
  data_champs <- httr::content(
    httr::GET(
      paste(
        "http://ddragon.leagueoflegends.com/cdn/", version, "/data/", language, "/champion/",  champion_name,".json", sep=""
      )
    )
  )
  data_spells <- data_champs$data[[1]]$spells %>% purrr::map_dfr(.f=function(x){data.frame(x[c(1:4, 6, 8, 10)])})
  return(data_spells)
}






# Crawler

#First: Take a list of players
crawler_regions <- function(key, regions, rank, division){
  print("This could take a while... Want to play LoL?")
  # Set df to null to bind them later
  matches_info <- NULL
  match_info_df <- NULL
  match_events_df <- NULL
  player_data_df <- NULL

  # For loop goes through all specified regions
  for(j in 1:length(regions)){
    cat("Getting data from region:", regions[j], "\n")

    #Get list of accounts from region j, tier & division
    summoners <- lol_get_summoners_data(region = regions[j], rank = rank, division =division, API=key)
    accounts <- c()
    cat("Get Summoners Account Process will take about:", round(nrow(summoners)/60,2), "minutes \n" )

    # From that list, take all the possible acc_ids
    for(i in 1:nrow(summoners)){
      cat(i)
      acc <- lol_get_summoner_data(region = regions[j],
                                   summoner_name = summoners$summonerName[i],
                                   API=key)$accountId
      accounts[i] <- ifelse(is.null(acc), "1", acc)
      # Rate limit prevention
      Sys.sleep(1)
    }
    cat("Cooldown 60s")
    Sys.sleep(60)
    accounts <- unique(accounts[accounts!="1"])

    # Take last match for each summoner on list
    cat("\n Taking last match for each summoner \n")
    cat("This process will take around:", round(length(accounts)/60, 2), "minutes \n")
    match <- c()
    for(i in 1:length(accounts)){
      if(i==99 || i==199){Sys.sleep(30)}
      cat(i)
      match_id <- as.character(lol_get_matches(region = regions[j], acc_id = accounts[i], API = key)$gameId[1])
      #For some players it does not find match, so thi sets match_id as 1 and we remove it later
      match[i] <- ifelse(is.null(match_id), "1", match_id)
      #Rate limit prevention
      Sys.sleep(1)
    }
    print("First part done!")
    Sys.sleep(30)

    # remove duplicated and non-valid match ids
    match <- unique(match[match!=1])
    cat("Get Match information (without timeline, next step takes that data) \n")
    cat("Getting Match information for:", length(match), "matches \n")
    cat("This process will take:", round((length(match)*1.5)/60, 2), "minutes \n")
    match_info <- vector(mode="list",length = length(match))

    for(i in 1:length(match)){
      cat(i)
      if(i==99){Sys.sleep(30)}
      match_info[[i]] <- lol_get_match_info(region=regions[j],
                                            match_id = match[i],
                                            API=key,
                                            timeline_data = F)
      match_info[[i]]$match_id <- match[i]
      Sys.sleep(1)
    }
    cat("Second part done!  \n")
    cat("Cooldown 60s  \n")
    Sys.sleep(60)
    to_remove <-  which(match_info %>% purrr::map_chr(class) != "data.frame")
    match_info <- match_info[-to_remove]
    summoners_p <- do.call(plyr::rbind.fill,match_info)$summonerName
    matches_info <- plyr::rbind.fill(matches_info, do.call(plyr::rbind.fill,match_info) %>% mutate(REG = regions[j]))

    #Get timeline info
    cat("Getting Match Timeline information for:", length(match), "matches \n")
    match_info <- vector(mode="list",length = length(match))
    match_events <- vector(mode="list",length = length(match))
    cat("This process will take:", round((length(match)*2.3)/60, 2), "minutes  \n")

    for(i in 1:length(match)){
      cat(i)
      if(i==99){Sys.sleep(30)}
      data <- try(lol_get_match_time(region=regions[j],
                                     match_id = match[i],
                                     API=key))
      if(data !=  "Error in events[[1]] : subscript out of bounds\n" & data != "Error : All inputs to rbind.fill must be data.frames\n"){
        match_info[[i]] <- data$data_participants %>% mutate(MATCH_ID = match[i])
        match_events[[i]] <- data$data_events
        match_events[[i]]$match_id = match[i]
        match_events[[i]]$match_id = match[i]}
      Sys.sleep(1)
    }
    ("Third part done!")
    Sys.sleep(60)
    match_events_df <- rbind(match_events_df,do.call(rbind,match_events) %>% mutate(REG = regions[j]))
    match_info_df <- rbind(match_info_df, do.call(rbind,match_info)%>% mutate(REG = regions[j]))

    #Now we need the data of all the players that were on those matches
    Names <- unique(matches_info %>% filter(REG == regions[j]) %>% select(summonerName))$summonerName

    player_data <- vector(mode="list",length = length(Names))

    cat("Getting summoner information for:", length(Names), "summoners \n")
    for(i in 1:length(Names)){
      cat(i, " ")
      if(i %% 100 ==0){Sys.sleep(30)}
      acc <- lol_get_summoner_data(region = regions[j],
                                   summoner_name = Names[i],
                                   API=key)
      player_data[[i]] <- acc
      # Rate limit prevention
      Sys.sleep(1)
    }

    player_data_df <- rbind(player_data_df, do.call(plyr::rbind.fill,player_data) %>% mutate(REG = regions[j]))
    print("Fourth part done!")
    print("We finish the first region, please give a minute to let the server rest")
    #Rate limit prevention
    Sys.sleep(60)
  }
  divisions <- data.frame(accountId = player_data_df$accountId, Division = division)
  out <- list(  matches_info,
                match_info_df,
                match_events_df,
                player_data_df %>% left_join(divisions, by = c("accountId" = "accountId"))
  )
  return(out)
}


#' Get data of matches of a specific summoner
summoner_crawler <- function(summoner_name="Unai Ondulado",  region = "EUW1", API){
  acc_id <- lol_get_summoner_data(region = region, summoner_name = summoner_name,API = API)$accountId
  matches <- lol_get_matches(region = region , acc_id = acc_id, API = API)

  # get data of the last 100 matches
  matchesInfo <- NULL
  nmatches <- nrow(matches)

  cat("Getting general info \n")
  for(i in 1:nmatches){
    cat(i)
    match <- lol_get_match_info(
      region = region,
      match_id = matches$gameId[i],
      API = API,
      timeline_data = T
    )

    if(!is.null(match)){
      matchesInfo <- plyr::rbind.fill(matchesInfo,
                                      match %>%
                                        dplyr::mutate(matchId = matches$gameId[i]))
    }

    Sys.sleep(0.5)
  }
  cat("Cooldown: 30s \n")
  Sys.sleep(30)

  cat("Getting Timeline of each match \n")
  dataPart <- NULL
  dataEvents <- NULL
  for(i in 1:nmatches){
    cat(i)
    list <- lol_get_match_time(region = region,
                               match_id = matches$gameId[i],
                               API = API)

    dataPart <- plyr::rbind.fill(dataPart,
                      list[[1]] %>%
                        dplyr::mutate(matchId = matches$gameId[i]))
    dataEvents <- plyr::rbind.fill(dataEvents,
                        list[[2]] %>%
                          dplyr::mutate(matchId = matches$gameId[i]))

    Sys.sleep(0.5)
  }

  cat("Finished")
  return(list(matchesInfo = matchesInfo, dataParticipants = dataPart, dataEvents=dataEvents))
}

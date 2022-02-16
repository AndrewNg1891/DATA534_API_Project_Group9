#' Send request to GitHub Activity API
#'
#' This function is a helper function for sending requests and return response.
#'
#' @param path API path
#' @param per_page Results per page (max 100).
#' @param page Page number of the results to fetch.
#' @param username (Optional) GitHub username for authentication and increasing rate limit.
#' @param token (Optional) GitHub access token for authentication and increasing rate limit.
#' @return Raw response
github_api <- function(path,per_page,page,username = "",token = "") {
    
    if(!is.numeric(per_page)){
        stop("The per_page parameter should not be non-numerical value")
    }
    
    if(!is.numeric(page)){
        stop("The page parameter should not be non-numerical value")
    }
    
    if (per_page < 1 || per_page > 100 || per_page != round(per_page)) {
        stop("The per_page parameter should be a integer between 1 and 100 (inclusive)", call. = FALSE)
    }
    
    if (page < 1 || page != round(page)) {
        stop("The page parameter should be a positive integer", call. = FALSE)
    }
    
    query <- list(per_page=per_page,page=page)
    
    url <- httr::modify_url("https://api.github.com", path = path)
    if (username == "" || token == "") {
        response <- httr::GET(url, httr::add_headers(accept = "application/vnd.github.v3+json"), query = query)
    } else {
        response <- httr::GET(url, httr::add_headers(accept = "application/vnd.github.v3+json"), query = query, httr::authenticate(username, token))
    }
    
    
    if (httr::http_error(response)) {
        stop("GitHub API request failed", call. = FALSE)
    }
    
    if (httr::http_type(response) != "application/json") {
        stop("API did not return json", call. = FALSE)
    }
    response
}


#' Format response
#'
#' This function is a helper function for formatting response and return list/dataframe/character/raw response.
#'
#' @param response Raw response gets from github_api().
#' @param output_type Indicate formatting type (Possible value: "list","dataframe","character","raw").
#' @return Formatted response.
format_response <- function(response,output_type) {
    result <- NULL
    if (output_type == "list") {
        result <- jsonlite::fromJSON(rawToChar(response$content), simplifyVector = FALSE)
    } else if (output_type == "dataframe") {
        result <- jsonlite::fromJSON(rawToChar(response$content),flatten = TRUE)
    } else if (output_type == "character") {
        result <- rawToChar(response$content)
    } else if (output_type == "raw") {
        result <- response
    } else {
        stop("Unexpected output type, possible values are: 'list', 'dataframe', 'character'")
    }
    
    result
}


#' Get public events
#'
#' This function get public events data and return the data in user selected format.
#'
#' @param per_page (Optional) Results per page (max 100) (default 30).
#' @param page (Optional) Page number of the results to fetch (default 1).
#' @param output_type (Optional) Indicate formatting type (Possible value: "list","dataframe","character","raw") (default: "list").
#' @param username (Optional) GitHub username for authentication and increasing rate limit.
#' @param token (Optional) GitHub access token for authentication and increasing rate limit.
#' @return Formatted response.
#' @export
github_get_public_events <- function(per_page=30, page=1, output_type="list",username = "",token = ""){
    
    response <- github_api("/events",per_page,page,username,token)
    
    result <- format_response(response,output_type)
    
    result
}


#' Get organization events
#'
#' This function get organization events data and return the data in user selected format.
#'
#' @param organization GitHub organization name.
#' @param per_page (Optional) Results per page (max 100) (default 30).
#' @param page (Optional) Page number of the results to fetch (default 1).
#' @param output_type (Optional) Indicate formatting type (Possible value: "list","dataframe","character","raw") (default: "list").
#' @param username (Optional) GitHub username for authentication and increasing rate limit.
#' @param token (Optional) GitHub access token for authentication and increasing rate limit.
#' @return Formatted response.
#' @export
github_get_organization_events <- function(organization, per_page=30, page=1, output_type="list",username = "",token = ""){
        
    path <- paste("/orgs/", organization, "/events", sep="")

    response <- github_api(path,per_page,page,username,token)
    
    result <- format_response(response,output_type)
    
    result
}


#' Get network events
#'
#' This function get network events data and return the data in user selected format.
#'
#' @param owner GitHub repository owner username.
#' @param repo GitHub repository name.
#' @param per_page (Optional) Results per page (max 100) (default 30).
#' @param page (Optional) Page number of the results to fetch (default 1).
#' @param output_type (Optional) Indicate formatting type (Possible value: "list","dataframe","character","raw") (default: "list").
#' @param username (Optional) GitHub username for authentication and increasing rate limit.
#' @param token (Optional) GitHub access token for authentication and increasing rate limit.
#' @return Formatted response.
#' @export
github_get_network_events <- function(owner, repo, per_page=30, page=1, output_type="list",username = "",token = ""){
        
    path <- paste("/networks/", owner, "/", repo, "/events", sep="")

    response <- github_api(path,per_page,page,username,token)
    
    result <- format_response(response,output_type)
    
    result
}



#' Get repository events
#'
#' This function get repository events data and return the data in user selected format.
#'
#' @param owner GitHub repository owner username.
#' @param repo GitHub repository name.
#' @param per_page (Optional) Results per page (max 100) (default 30).
#' @param page (Optional) Page number of the results to fetch (default 1).
#' @param output_type (Optional) Indicate formatting type (Possible value: "list","dataframe","character","raw") (default: "list").
#' @param username (Optional) GitHub username for authentication and increasing rate limit.
#' @param token (Optional) GitHub access token for authentication and increasing rate limit.
#' @return Formatted response.
#' @export
github_get_repo_events <- function(owner, repo, per_page=30, page=1, output_type="list",username = "",token = ""){
        
    path <- paste("/repos/", owner, "/", repo, "/events", sep="")

    response <- github_api(path,per_page,page,username,token)
    
    result <- format_response(response,output_type)
    
    result
}


#' Get last n events
#'
#' This function get last n events data of a certain range and return the dataframe that contains the retrieved events data.
#'
#' @param range The range for retrieving events data (Possible value: "all public"/"network"/"organization"/"repository").
#' @param max_event_num The number of events data that user want.
#' @param owner (Optional) GitHub repository owner username.
#' @param repo (Optional) GitHub repository name.
#' @param organization (Optional) GitHub organization name.
#' @param username (Optional) GitHub username for authentication and increasing rate limit.
#' @param token (Optional) GitHub access token for authentication and increasing rate limit.
#' @return The dataframe that contains the retrieved events data.
#' @export
github_lastn_events_df <- function(range, max_event_num, owner = "", repo = "", organization = "",username = "",token = ""){
    if (max_event_num < 1 || max_event_num != round(max_event_num)) {
        stop("The max_event_num parameter should be a positive integer", call. = FALSE)
    }
    total_pages <- ceiling(max_event_num/100)
    result <- NULL
    datalist = list()
    for (i in 1:total_pages){
        if (range == "all public") {
            temp_df <- github_get_public_events(100, page=i,"dataframe",username,token)
        } else if (range == "network") {
            if (owner == "" || repo == "") {
                stop("The owner and repo parameters are required for getting network events", call. = FALSE)
            }
            temp_df <- github_get_network_events(owner, repo, 100, page=i,"dataframe",username,token)
        } else if (range == "organization") {
            if (organization == "") {
                stop("The organization parameter is required for getting organization events", call. = FALSE)
            }
            temp_df <- github_get_organization_events(organization, 100, page=i,"dataframe",username,token)
        } else if (range == "repository") {
           if (owner == "" || repo == "") {
                stop("The owner and repo parameters are required for getting repository events", call. = FALSE)
            }
            temp_df <- github_get_repo_events(owner, repo, 100, page=i,"dataframe",username,token)
        }
        
        if (length(temp_df) > 0) {
            datalist[[i]] <- temp_df
        } else {
            break
        }
        temp_df <- temp_df[,c("id","type","created_at", "actor.login", "repo.name")]
    }
    binded_df <- dplyr::bind_rows(datalist)
    row_num <- min(c(nrow(binded_df),max_event_num))
    binded_df <- binded_df[1:row_num,]
    binded_df
    
}


#' Count events by type
#'
#' This function counts the number of events by type for a certain range.
#'
#' @param range The range for retrieving events data (Possible value: "all public"/"network"/"organization"/"repository").
#' @param max_event_num The number of events data that user want.
#' @param owner (Optional) GitHub repository owner username.
#' @param repo (Optional) GitHub repository name.
#' @param organization (Optional) GitHub organization name.
#' @param username (Optional) GitHub username for authentication and increasing rate limit.
#' @param token (Optional) GitHub access token for authentication and increasing rate limit.
#' @return The dataframe that contains the count result.
#' @export
github_count_events_bytype <- function(range, max_event_num, owner = "", repo = "", organization = "",username = "",token = ""){
   
    binded_df <- github_lastn_events_df(range,max_event_num,owner,repo,organization,username,token)
    result <- binded_df |> dplyr::count(type, name = "count")
    result
    
}


#' Count events by date
#'
#' This function counts the number of events by date for a certain range and type.
#'
#' @param range The range for retrieving events data (Possible value: "all public"/"network"/"organization"/"repository").
#' @param max_event_num The number of events data that user want.
#' @param owner (Optional) GitHub repository owner username.
#' @param repo (Optional) GitHub repository name.
#' @param organization (Optional) GitHub organization name.
#' @param event_type (Optional) The event type to count (default will count all types events).
#' @param username (Optional) GitHub username for authentication and increasing rate limit.
#' @param token (Optional) GitHub access token for authentication and increasing rate limit.
#' @return The dataframe that contains the count information.
#' @export
github_count_events_bydate <- function(range, max_event_num, owner = "", repo = "", organization = "", event_type = "all",username = "",token = ""){
   
    
    binded_df <- github_lastn_events_df(range,max_event_num,owner,repo,organization,username,token)
    if (event_type == "all") {
        binded_df <- binded_df |> dplyr::mutate(date = lubridate::ymd(as.POSIXlt(created_at)))
    } else {
        binded_df <- binded_df |> dplyr::filter(type == event_type) |> dplyr::mutate(date = lubridate::ymd(as.POSIXlt(created_at)))
    }
    
    result <- binded_df |> dplyr::count(date, name = "count")
    result
    
}


#' Count events by weekday
#'
#' This function counts the number of events by weekdays for a certain range and type.
#'
#' @param range The range for retrieving events data (Possible value: "all public"/"network"/"organization"/"repository").
#' @param max_event_num The number of events data that user want.
#' @param owner (Optional) GitHub repository owner username.
#' @param repo (Optional) GitHub repository name.
#' @param organization (Optional) GitHub organization name.
#' @param event_type (Optional) The event type to count (default will count all types events).
#' @param return_type (Optional) Indicate return dataframe or plot (default: dataframe).
#' @param username (Optional) GitHub username for authentication and increasing rate limit.
#' @param token (Optional) GitHub access token for authentication and increasing rate limit.
#' @return The dataframe/plot that contains the count information.
#' @export
github_count_events_byweekday <- function(range, max_event_num, owner = "", repo = "", organization = "", event_type = "all", return_type = "dataframe",username = "",token = ""){
   
    
    binded_df <- github_lastn_events_df(range,max_event_num,owner,repo,organization,username,token)
    if (event_type == "all") {
        binded_df <- binded_df |> dplyr::mutate(weekdays = lubridate::wday(lubridate::ymd(as.POSIXlt(created_at)),label=TRUE))
    } else {
        binded_df <- binded_df |> dplyr::filter(type == event_type) |> dplyr::mutate(weekdays = lubridate::wday(lubridate::ymd(as.POSIXlt(created_at)),label=TRUE))
    }
    
    counted_df <- binded_df |> dplyr::count(weekdays, name = "count")
    
    if (return_type == "plot") {
        result <- ggplot2::ggplot(counted_df, ggplot2::aes(x = weekdays, y = count)) + ggplot2::geom_bar(stat="identity") 
    } else {
        result <- counted_df
    }
    
    result
}
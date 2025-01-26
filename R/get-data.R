OPENDOTA_URL <- "https://api.opendota.com/api/"

#' @export
get_player_matches <- function(account_id, recent = FALSE) {
    if (recent) {
        base_url <- paste(OPENDOTA_URL, "players/acc_id/recentMatches", sep = "")
    } else {
        base_url <- paste(OPENDOTA_URL, "players/acc_id/matches", sep = "")
    }

    dat <- rjson::fromJSON(file = gsub("acc_id", account_id, base_url))
    dat_cleaned <- purrr::map_depth(dat, 2, ~ifelse(is.null(.x), NA, .x))
    dat_df <- do.call(rbind, lapply(dat_cleaned, data.frame))

    dat_df |>
        dplyr::mutate(account_id = account_id,
               radiant = ifelse(player_slot < 5, TRUE, FALSE),
               win = !xor(radiant, radiant_win))
}

#' Get Player Statistics for a Single Match
#'
#' @details
#' This function connects to the OpenDota API and fetches match data in JSON format. It extracts key statistics
#' for each player in the match and organizes them into a data frame. The `match_id` column is added for reference.
#'
#' @param match_id A numeric or character vector representing the match ID for which player statistics
#'   are to be fetched. If a numeric value is provided, it is automatically converted to a character.
#'
#' @return A data frame with the following columns:
#'   \item{account_id}{Player's account ID.}
#'   \item{kills}{Number of kills the player achieved in the match.}
#'   \item{deaths}{Number of times the player died in the match.}
#'   \item{assists}{Number of assists the player contributed in the match.}
#'   \item{last_hits}{Number of creeps the player last-hit during the match.}
#'   \item{denies}{Number of creeps the player denied during the match.}
#'   \item{level}{The player's level at the end of the match.}
#'   \item{net_worth}{The player's net worth at the end of the match.}
#'   \item{gold_per_min}{The player's gold earned per minute during the match.}
#'   \item{xp_per_min}{The player's experience earned per minute during the match.}
#'   \item{hero_damage}{The amount of damage the player dealt to enemy heroes.}
#'   \item{hero_healing}{The amount of healing the player provided to allied heroes.}
#'   \item{tower_damage}{The amount of damage the player dealt to enemy towers.}
#'   \item{win}{Whether the player’s team won (`1`) or lost (`0`).}
#'   A `match_id` column is added to the data frame indicating the match from which the data was retrieved.
#'
#' @export
get_single_match_player <- function(match_id) {
    if (!is.character(match_id)) {
        match_id <- as.character(match_id)
    }
    base_url <- paste(OPENDOTA_URL, "matches/match_id", sep = "")
    dat <- rjson::fromJSON(file = gsub("match_id", match_id, base_url))
    dat_df <- do.call(
      rbind.data.frame,
      lapply(
        dat$players,
        function(row) {
          row[
            c(
              "account_id",
              "kills",
              "deaths",
              "assists",
              "last_hits",
              "denies",
              "level",
              "net_worth",
              "gold_per_min",
              "xp_per_min",
              "hero_damage",
              "hero_healing",
              "tower_damage",
              "win"
            )
          ]
        })
      
    )
    dat_df |>
      dplyr::mutate(match_id = match_id)
}

get_single_match <- function(match_id) {
  if (!is.character(match_id)) {
    match_id <- as.character(match_id)
  }
  base_url <- paste(OPENDOTA_URL, "matches/match_id", sep = "")
  dat <- rjson::fromJSON(file = gsub("match_id", match_id, base_url))
  as.data.frame(
    dat[sapply(dat, is.atomic)]
  ) |>
    dplyr::mutate(match_id = match_id)
}

#' @export
get_player_heroes <- function(account_id) {
    base_url <- paste(OPENDOTA_URL, "players/acc_id/heroes", sep = "")
    dat <- rjson::fromJSON(file = gsub("acc_id", account_id, base_url))
    dat_df <- do.call(rbind.data.frame, dat)
    dat_df |>
        dplyr::mutate(date = Sys.Date())
}

#' @export
get_mmr_data <- function() {
    dat <- rjson::fromJSON(file = paste(OPENDOTA_URL, "distributions", sep = ""))
    dat_df <- do.call(rbind.data.frame, dat$ranks$rows)
    dat_df |>
        dplyr::mutate(date = Sys.Date())
}

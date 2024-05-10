library(dplyr)
library(purrr)
library(rjson)

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

#' @export
get_single_match <- function(match_id, output_file) {
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
                row[c("account_id", "kills", "deaths", "assists",
                      "net_worth", "gold_per_min", "xp_per_min", "win",
                      "last_hits", "denies")]
            })
        
    )
    dat_df |>
        dplyr::mutate(match_id = match_id)
}

#' @export
get_player_heroes <- function(account_id, output_file) {
    base_url <- paste(OPENDOTA_URL, "players/acc_id/heroes", sep = "")
    dat <- rjson::fromJSON(file = gsub("acc_id", account_id, base_url))
    dat_df <- do.call(rbind.data.frame, dat)
    dat_df |>
        dplyr::mutate(date = Sys.Date())
}

#' @export
get_mmr_data <- function(output_file) {
    dat <- rjson::fromJSON(file = paste(OPENDOTA_URL, "distributions", sep = ""))
    dat_df <- do.call(rbind.data.frame, dat$ranks$rows)
    dat_df |>
        dplyr::mutate(date = Sys.Date())
}

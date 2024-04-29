library(purrr)
library(rjson)

OPENDOTA_URL <- "https://api.opendota.com/api/"

get_player_matches <- function(account_id, output_file, recent = FALSE) {
    if (recent) {
        base_url <- paste(OPENDOTA_URL, "players/acc_id/recentMatches", sep = "")
    } else {
        base_url <- paste(OPENDOTA_URL, "players/acc_id/matches", sep = "")
    }

    dat <- rjson::fromJSON(file = gsub("acc_id", account_id, base_url))
    dat_cleaned <- purrr::map_depth(dat, 2, ~ifelse(is.null(.x), NA, .x))
    dat_df <- do.call(rbind, lapply(dat_cleaned, data.frame))

    dat_df$account_id <- account_id
    dat_df$radiant <- ifelse(dat_df$player_slot < 5, TRUE, FALSE)
    dat_df$win <- with(dat_df, !xor(radiant, radiant_win))

    write.csv(dat_df, file = output_file, row.names = FALSE)
    output_file
}

get_single_match <- function(match_id, output_file) {
    # TODO: expand to other match data: very barebones at the moment
    if (!is.character(match_id)) {
        print("coercing input match_id into a character")
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
                    "gold_per_min", "xp_per_min", "win")]
            })
        
    )
    write.csv(dat_df, file = output_file, row.names = FALSE)
    output_file
}

get_player_heroes <- function(account_id, output_file) {
    base_url <- paste(OPENDOTA_URL, "players/acc_id/heroes", sep = "")
    dat <- rjson::fromJSON(file = gsub("acc_id", account_id, base_url))
    dat_df <- do.call(rbind.data.frame, dat)
    dat_df$date <- Sys.Date()
    write.csv(dat_df, file = output_file, row.names = FALSE)
    output_file
}

get_mmr_data <- function(output_file) {
    dat <- rjson::fromJSON(file = paste(OPENDOTA_URL, "distributions", sep = ""))
    dat_df <- do.call(rbind.data.frame, dat$ranks$rows)
    dat_df$date <- Sys.Date()
    write.csv(dat_df, file = output_file, row.names = FALSE)
    output_file
}

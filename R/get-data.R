library(purrr)
library(rjson)

# TODO: move base_url abstraction outside of functions
get_player_matches <- function(account_id, output_file, recent = FALSE) {
    if (recent) {
        base_url <- "https://api.opendota.com/api/players/acc_id/recentMatches"
    } else {
        base_url <- "https://api.opendota.com/api/players/acc_id/matches"
    }

    dat <- rjson::fromJSON(file = gsub("acc_id", account_id, base_url))
    dat_cleaned <- purrr::map_depth(dat, 2, ~ifelse(is.null(.x), NA, .x))
    dat_df <- do.call(rbind, lapply(dat_cleaned, data.frame))

    dat_df$account_id <- account_id
    dat_df$radiant <- ifelse(dat_df$player_slot < 5, TRUE, FALSE)
    dat_df$win <- with(dat_df, !xor(radiant, radiant_win))

    write.csv(dat_df, file = output_file, row.names = FALSE)
}

get_player_heroes <- function(account_id, output_file) {
    base_url <- "https://api.opendota.com/api/players/acc_id/heroes"
    dat <- rjson::fromJSON(file = gsub("acc_id", account_id, base_url))
    dat_df <- do.call(rbind.data.frame, dat)
    dat_df$date <- Sys.Date()
    write.csv(dat_df, file = output_file, row.names = FALSE)
}

get_mmr_data <- function(output_file) {
    dat <- rjson::fromJSON(file = "https://api.opendota.com/api/distributions") 
    dat_df <- do.call(rbind.data.frame, dat$ranks$rows)
    dat_df$date <- Sys.Date()
    write.csv(dat_df, file = output_file, row.names = FALSE)
}



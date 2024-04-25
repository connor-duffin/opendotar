library(purrr)
library(rjson)

get_player_data <- function(account_id, output_file) {
    dat <- rjson::fromJSON(
        file = gsub("acc_id", account_id,
            "https://api.opendota.com/api/players/acc_id/matches"))
    dat_cleaned <- purrr::map_depth(dat, 2, ~ifelse(is.null(.x), NA, .x))
    dat_df <- do.call(rbind, lapply(dat_cleaned, data.frame))

    dat_df$account_id <- account_id
    dat_df$radiant <- ifelse(dat_df$player_slot < 5, TRUE, FALSE)
    dat_df$win <- with(dat_df, !xor(radiant, radiant_win))

    write.csv(dat_df, file = output_file)
}

get_mmr_data <- function(output_file) {
    dat <- rjson::fromJSON(file = "https://api.opendota.com/api/distributions") 
    dat_df <- do.call(rbind.data.frame, dat$ranks$rows)
    dat_df$date <- Sys.Date()
    write.csv(dat_df, file = output_file, row.names = FALSE)
}

# TODO: check how to manage output file when unit testing
test_that("get_player_data reads in matches from OpenDotA", {
    output_file <- "tmp.csv"
    get_player_matches(86745912, output_file)

    out <- read.csv(output_file)
    expect_in(
        c("account_id",
          "radiant",
          "win",
          "match_id",
          "hero_id",
          "duration",
          "start_time"),
        names(out))
    expect_gt(nrow(out), 20)
    expect_true(sum(out$win) / nrow(out) >= 0.5)
    file.remove(output_file)
})

test_that("get_player_data reads in recent matches from OpenDotA", {
    output_file <- "tmp.csv"
    get_player_matches(86745912, output_file, recent = TRUE)
    out <- read.csv(output_file)

    expect_in(
        c("account_id",
          "radiant",
          "win",
          "match_id",
          "hero_id",
          "duration",
          "start_time"),
        names(out))
    expect_equal(nrow(out), 20)
    expect_true(sum(out$win) / nrow(out) <= 1.)
    file.remove(output_file)
})

test_that("get_single_match reads in match data", {
    output_file <- "tmp.csv"
    get_single_match(7704581651, output_file)
    out <- read.csv(output_file)

    expect_in(names(out),
              c("account_id", "kills", "deaths", "assists",
                "gold_per_min", "xp_per_min", "win"))

    expect_equal(out$account_id,
                 c(101356886, 101986166, 118370366, 114585639,
                   445291085, 231581709, 122817493, 92487440))
    expect_equal(out$win, c(0, 0, 0, 0, 1, 1, 1, 1))
    file.remove(output_file)
})

test_that("get_player_heroes reads in a players hero pool", {
    output_file <- "tmp.csv"
    get_player_heroes(86745912, output_file)
    out <- read.csv(output_file)

    expect_in(c("hero_id", "last_played", "games", "win", "with_games",
                "with_win", "against_games", "against_win", "date"),
              names(out))
    file.remove(output_file)
})

test_that("get_mmr_data reads in data from OpenDotA", {
    output_file <- "tmp.csv"
    get_mmr_data(output_file)

    out <- read.csv(output_file)
    expect_setequal(names(out),
                    c("bin", "bin_name", "count", "cumulative_sum", "date"))
    expect_equal(out$date[1], as.character(Sys.Date()))
    file.remove(output_file)
})

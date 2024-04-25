test_that("get_player_data reads in data from OpenDotA", {
    output_file <- "tmp.csv"
    get_player_data(86745912, output_file)

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
    expect_gt(nrow(out), 0)
    expect_true(sum(out$win) / nrow(out) >= 0.5)
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

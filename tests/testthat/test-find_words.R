test_that("find-words", {

  word_list <- c("repeated measures",
              "within subjects",
              "paired t",
              "paired sample",
              "mixed design",
              "mixed-design",
              "paired-sample",
              "within-subject",
              "repeated-measures",
              "paired-t")

    text <- "something something repeated measures"
    chk <- find_words(text, word_list)
    testthat::expect_true(chk, "test1")

    text <- "something something paired t something"
    chk <- find_words(text, word_list)
    testthat::expect_true(chk, "test2")


    text <- "something something within subjects something"
    chk <- find_words(text, word_list)
    testthat::expect_true(chk, "test3")

    text <- "something something paired samples something"
    chk <- find_words(text, word_list)
    testthat::expect_true(chk, "test4")

    text <- "something mixed design samples something"
    chk <- find_words(text, word_list)
    testthat::expect_true(chk, "test5")


    text <- "something mixed-design sam something"
    chk <- find_words(text, word_list)
    testthat::expect_true(chk, "test6")


    text <- "something within something subjects something tthie"
    chk <- find_words(text, word_list)
    testthat::expect_false(chk, "test7")

})

# test_that("import_file", {

    # filename <- "~/GitHub/ugrad_project/test.txt"
    # text <- import_file(filename)

# })


test_that("import_file", {

    text <- "covariance"
    chk <- find_cov(text)
    testthat::expect_true(chk, "test8")

    text <- "analysis of covariance"
    chk <- find_cov(text)
    testthat::expect_false(chk, "test9")
    
    text <- "cover"
    chk <- find_cov(text)
    testthat::expect_false(chk, "test10")

    text <- "covariate"
    chk <- find_cov(text)
    testthat::expect_false(chk, "test11")

    text <- "ancova"
    chk <- find_cov(text)
    testthat::expect_false(chk, "test12")

    text <- "coverage"
    chk <- find_cov(text)
    testthat::expect_false(chk, "test13")

    text <- "cova"
    chk <- find_cov(text)
    testthat::expect_false(chk, "test14")


})

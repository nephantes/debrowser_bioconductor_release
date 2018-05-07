require(shiny)
library(debrowser)
library(testthat)

test_that("able to create the basic UI", {
    expect_silent( getDataPrep <- getDataPrepPanel() )
    expect_true(exists("getDataPrep"))
    expect_equal(getDataPrep[[1]][[1]],"div")
})

test_that("able to create panel UI", {
    expect_silent( QCPanel <- getQCPanel() )
    expect_true(exists("QCPanel"))
    expect_equal(QCPanel[[1]][[1]], "div")

    expect_silent( downloads <- getDownloadSection(TRUE) )
    expect_true(exists("downloads"))
    expect_equal(downloads[[1]][[1]], "div")

    expect_silent( getMain <- getMainPanel("randstr") )
    expect_true(exists("getMain"))
    expect_equal(getMain[[1]][[1]], "div")

    expect_silent( getStart <- getStartupMsg() )
    expect_true(exists("getStart"))
    expect_equal(getStart[[1]][[1]], "div")

    expect_silent( getAfter <- getAfterLoadMsg() )
    expect_true(exists("getAfter"))
    expect_equal(getAfter[[1]][[1]], "div")

    expect_silent(getGO <- getGoPanel() )
    expect_true(exists("getGO"))
    expect_equal(getGO[[1]][[1]], "div")
})

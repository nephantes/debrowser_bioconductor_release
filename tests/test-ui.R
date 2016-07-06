require(shiny)
library(debrowser)
library(testthat)

test_that("able to create the basic UI", {
    expect_silent( ui <- deUI() )
    expect_true(exists("ui"))
    expect_equal(ui[[3]][[1]][[2]][[1]], "container-fluid")
})

test_that("able to create panel UI", {
    expect_silent( QCPanel <- getQCPanel() )
    expect_true(exists("QCPanel"))
    expect_equal(QCPanel[[1]][[1]], "span")

    expect_silent( leftMenu <- getLeftMenu() )
    expect_true(exists("leftMenu"))
    expect_equal(leftMenu[[1]][[1]], "div")

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

    expect_silent(getGO <- getGoPanel(TRUE) )
    expect_true(exists("getGO"))
    expect_equal(getGO[[1]][[1]], "div")
})

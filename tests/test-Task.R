library(testthat)

test_that("Test creation and run.", {
  x <- 0
  t1 <- Task$new("t1", x <- 1)
  expect_equal(x, 0)
  expect_equal(t1$completed, F)
  t1$run()
  expect_equal(x, 1)
  expect_equal(t1$completed, T)
})

test_that("Dependency checks.", {
  x <- 1
  t1 <- Task$new("t1", x <- x * 2)
  t2 <- Task$new("t2", x <- x * 2)
  t2$dependsOn(t1)
  t2$runAll()
  expect_equal(t1$completed, T)
  expect_equal(t2$completed, T)
  expect_equal(x, 4)
  x <- y <- 1
  t3 <- Task$new("t3", y <- y * 2)
  t2 <- Task$new("t2", z <- x + y)
  t2$dependsOn(t1, t3)
  t2$runAll()
  expect_equal(x, 2)
  expect_equal(y, 2)
  expect_equal(z, 4)
})

test_that("Compound expression test", {
  t1 <- Task$new("t1",
  {
    x <- 2
    print("hello")
  })
  expect_output(t1$runAll(), "hello")
  expect_equal(x, 2)
})

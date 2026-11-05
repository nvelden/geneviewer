test_that("the compile page is opened again when it was closed", {
  first <- compile_page()
  expect_identical(compile_page(), first)

  first$close()
  second <- compile_page()
  expect_false(identical(second, first))
  expect_true(second$is_active())
})

test_that("closing the compile page closes the browser and removes its file", {
  compile_page()
  chrome <- compile_session$chrome
  file <- compile_session$file
  expect_true(file.exists(file))

  close_compile_page()
  # The browser exits a moment after it is told to close
  for (i in seq_len(100)) {
    if (!chrome$is_alive()) break
    Sys.sleep(0.1)
  }
  expect_false(chrome$is_alive())
  expect_false(file.exists(file))
  expect_length(ls(compile_session), 0)
})

test_that("a missing prerequisite skips, or fails where the tests are required", {
  expect_invisible(skip_unless(TRUE, "ready"))
  withr::local_envvar(GENEVIEWER_REQUIRE_TESTS = "")
  expect_condition(skip_unless(FALSE, "not ready"), "not ready", class = "skip")
  withr::local_envvar(GENEVIEWER_REQUIRE_TESTS = "true")
  expect_error(skip_unless(FALSE, "not ready"), "not ready")
})

test_that("an error starting Chrome is kept", {
  error <- simpleError("no such socket")
  withr::local_envvar(GENEVIEWER_REQUIRE_TESTS = "")
  expect_condition(chrome_error(error), "Chrome could not be started: no such socket", class = "skip")
  withr::local_envvar(GENEVIEWER_REQUIRE_TESTS = "true")
  expect_error(chrome_error(error), "no such socket")
})

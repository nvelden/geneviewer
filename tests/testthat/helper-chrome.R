# Opens a file in headless Chrome and returns the session, or skips the test
# when Chrome is missing or does not start (e.g. on a slow CI runner). Chrome
# is closed when the calling test ends.
chrome_page <- function(file, width = 992, height = 1323, env = parent.frame()) {
  skip_on_cran()
  skip_if_not_installed("chromote")
  skip_if(is.null(tryCatch(chromote::find_chrome(), error = function(e) NULL)), "Chrome not found")

  chrome <- tryCatch(chromote::Chromote$new(), error = function(e) NULL)
  skip_if(is.null(chrome), "Chrome could not be started")
  do.call(on.exit, list(substitute(chrome$close(), list(chrome = chrome)), add = TRUE),
          envir = env)

  session <- chrome$new_session(width = width, height = height)
  loaded <- session$Page$loadEventFired(wait_ = FALSE)
  session$Page$navigate(paste0("file://", normalizePath(file)), wait_ = FALSE)
  session$wait_for(loaded)
  session
}

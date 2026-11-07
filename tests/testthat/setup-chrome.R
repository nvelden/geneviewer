# compile_page() (helper-chrome.R) keeps one browser for a test run, not longer
withr::defer(close_compile_page(), teardown_env())

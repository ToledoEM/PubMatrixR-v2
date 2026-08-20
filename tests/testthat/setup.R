# Disable request pacing; fetches are mocked suite-wide.
withr::local_options(PubMatrixR.min_interval = 0, .local_envir = teardown_env())

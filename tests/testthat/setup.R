# Force use of f_glue_pander formatter for all tests
# In setup.R, use triple colon to access internal function
##logger::log_formatter(Tlogger:::f_glue_pander, namespace = "Tdata", index = 1)

Tlogger::setup_namespace_logging(
  "Tdata",
  console_level = "INFO"
)

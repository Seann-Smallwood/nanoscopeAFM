fname = dir(pattern='nid$', recursive = TRUE)
check.NID_file(fname[1]) == 0

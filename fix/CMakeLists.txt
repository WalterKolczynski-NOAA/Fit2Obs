list(APPEND fix_files
  global_hyblev.l65.txt
)

install(PROGRAMS ${fix_files}
        DESTINATION "fix")

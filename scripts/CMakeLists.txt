list(APPEND scripts_files
  excfs_gdas_vrfyfits.sh
)

install(PROGRAMS ${scripts_files}
        DESTINATION "scripts")

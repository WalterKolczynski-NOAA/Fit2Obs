list(APPEND EXEC_NAMES
  cfs_bufrslupao
  cfs_bufrslslev
  cfs_bufrslsfc
  cfs_sfc
  cfs_raob
  cfs_acft
  cfs_acar
  cfs_surf)

foreach(pgmname ${EXEC_NAMES})

  set(exe_name "${pgmname}")
  set(fortran_src "${pgmname}.f")

  add_executable(${exe_name} ${fortran_src})
  target_link_libraries(${exe_name} PRIVATE w3emc::w3emc_4
                                            bufr::bufr_4)

  list(APPEND EXEC_TARGETS ${exe_name})

endforeach()

# Install targets
install(TARGETS ${EXEC_TARGETS} RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})

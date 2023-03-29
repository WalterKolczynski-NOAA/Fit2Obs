list(APPEND EXEC_NAMES
  splitac
  rembfr
  duprep
  combfr)

foreach(pgmname ${EXEC_NAMES})

  set(exe_name "cfs_${pgmname}.x")
  set(fortran_src "${pgmname}.f")

  add_executable(${exe_name} ${fortran_src})
  target_link_libraries(${exe_name} PRIVATE w3emc::w3emc_4
                                            bufr::bufr_4)
  list(APPEND EXEC_TARGETS ${exe_name})

endforeach()

if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel)$")
  set_source_files_properties(convdiag.f PROPERTIES COMPILE_FLAGS "-free")
  set_source_files_properties(read_convdiag2.f PROPERTIES COMPILE_FLAGS "-free")
  set_source_files_properties(read_convdiag3.f PROPERTIES COMPILE_FLAGS "-free")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "^(GNU)$")
  set_source_files_properties(convdiag.f PROPERTIES COMPILE_FLAGS "-ffree-form")
  set_source_files_properties(read_convdiag2.f PROPERTIES COMPILE_FLAGS "-free-form")
  set_source_files_properties(read_convdiag3.f PROPERTIES COMPILE_FLAGS "-free-form")
endif()

add_executable(cfs_post_convdiag.x convdiag.f read_convdiag2.f)
target_link_libraries(cfs_post_convdiag.x PRIVATE w3emc::w3emc_4
                                                  bufr::bufr_4)
list(APPEND EXEC_TARGETS cfs_post_convdiag.x)

add_executable(cfs_post_convnetc.x convdiag.f read_convdiag3.f)
target_link_libraries(cfs_post_convnetc.x PRIVATE w3emc::w3emc_4
                                                  bufr::bufr_4
                                                  NetCDF::NetCDF_Fortran)
list(APPEND EXEC_TARGETS cfs_post_convnetc.x)

# Install targets
install(TARGETS ${EXEC_TARGETS} RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
list(APPEND fortran_src
    gblevn00.f gblevn01.f gblevn02.f gblevn03.f gblevn04.f
    gblevn06.f gblevn08.f gblevn10.f gblevn11.f gblevn12.f
    gblevn35.f gblevn10netc.f gblevn10nems.f
    grdcrd.f hterpt.f hterptz.f moifun.f mpicast.f
    oefg01.f prevents.f sortbufr.f
    selectfile.f
)

if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel)$")
  set_source_files_properties(selectfile.f PROPERTIES COMPILE_FLAGS "-free")
  set_source_files_properties(gblevn10netc.f PROPERTIES COMPILE_FLAGS "-free")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "^(GNU)$")
  set_source_files_properties(selectfile.f PROPERTIES COMPILE_FLAGS "-free-form")
  set_source_files_properties(gblevn10netc.f PROPERTIES COMPILE_FLAGS "-free-form")
endif()

add_executable(cfs_prevmpi.x ${fortran_src})
target_link_libraries(cfs_prevmpi.x PRIVATE bacio::bacio_4
                                            w3emc::w3emc_4
                                            bufr::bufr_4
                                            sp::sp_4
                                            sigio::sigio
                                            sfcio::sfcio
                                            nemsio::nemsio)
target_link_libraries(cfs_prevmpi.x PRIVATE MPI::MPI_Fortran)
target_link_libraries(cfs_prevmpi.x PRIVATE NetCDF::NetCDF_Fortran)
if(OpenMP_Fortran_FOUND)
  target_link_libraries(cfs_prevmpi.x PRIVATE OpenMP::OpenMP_Fortran)
endif()

# Install targets
install(TARGETS cfs_prevmpi.x RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})

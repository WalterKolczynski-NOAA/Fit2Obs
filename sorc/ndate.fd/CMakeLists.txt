add_executable(ndate ndate.f)
target_link_libraries(ndate PRIVATE w3emc::w3emc_4)

# Install targets
install(TARGETS ndate RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})

set(LEVEL1_LIBS utils modules)
if (PSFFT_LIB)
set(LEVEL1_LIBS ${LEVEL1_LIBS} ${PSFFT_LIB} ${FFTWTHREADS_LIB})
endif()

# Simulation
FILE(GLOB sourcefiles "*.f90")

# if (PSFFT_LIB)
# else()
# FILE(GLOB to_remove "dns/rhs_global_incompressible_nbc.f90") # get the complete path
# list(REMOVE_ITEM sourcefiles ${to_remove})
# endif()

add_executable(09_profiling.x ${sourcefiles})
# target_link_libraries(dns.x mappings operators particles filters fdm ibm io thermodynamics ${LEVEL1_LIBS} ${LIBS})
# if (PSFFT_LIB)
# set_source_files_properties(dns/rhs_global_incompressible_nbc.f90 PROPERTIES COMPILE_FLAGS "${PSFFT_COMPILE_FLAGS}")
# set_target_properties(dns.x PROPERTIES LINK_FLAGS "${PSFFT_LINK_FLAGS} ${LINK_FLAGS}")
# endif()
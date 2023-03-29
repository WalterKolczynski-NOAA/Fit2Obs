list(APPEND ush_files
    cfs_bufr_post.sh
    cfs_cdas_fits.sh
    cfs_cdas_horizn.sh
    cfs_cmon.sh
    cfs_combfr.sh
    cfs_horizn.sh
    cfs_prevmpi.sh
)

install(PROGRAMS ${ush_files}
        DESTINATION "ush")
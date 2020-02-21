library("tidyverse")

# this prevents R from grabbing all available cores whenever doing
# any linear algebra with some parallelised BLAS variant in the
# background
if require("RhpcBLASctl") {

    if any( c("parallel",
                "snow",
                "snowFT",
                "snowfall",
                "foreach",
                "future") %in% loadedNamespaces() ) {
        # if any of the packages above is loaded, this suggests
        # internal parallelization in loaded packages, see:
        # https://cran.r-project.org/web/views/HighPerformanceComputing.html

        # parallelized code and BLAS parallelization may not work together
        # well, see e.g.:
        # https://cran.r-project.org/doc/manuals/R-admin.html#BLAS
        blas_set_num_threads(1)
        omp_set_num_threads(1)

    } else {

        # either of these commands sufficed in testing, but better be safe
        blas_set_num_threads(snakemake@threads)
        omp_set_num_threads(snakemake@threads)
    }
} else {
    print( str_c( "If this R rule does any linear algebra, consider adding a recent\n",
                  "version of r-rhpcblasctl to the conda environment yaml. This will\n",
                  "ensure that the CPU usage of the BLAS behind R linear algebra\n",
                  "stays restricted to the threads you specified in the rule definition.\n"
                )
         )
}

load_bioconductor_package <- function(path_to_bioc_pkg, pkg_name) {

    lib <- str_remove(path_to_bioc_pkg, pkg_name)

    # ensure that dependencies of the pkg are also found at same location
    .libPaths( c( lib , .libPaths() ) )

    library(pkg_name, character.only = TRUE)

    print(str_c("loaded package ", pkg_name))

    # ensure that library() calls outside this function don't go looking in the
    # location needed here
    .libPaths( .libPaths()[-1] )
}

get_prefix_col <- function(prefix, col_names) {

    covariate <- snakemake@params[["covariate"]]
    # add standard prefix to covariate
    col <- str_c(prefix, covariate, sep = "_")

    levels <- read_tsv(snakemake@input[["samples"]]) %>%
                dplyr::select( !!covariate ) %>%
                distinct( ) %>%
                pull( !!covariate )

    # possible suffixes, cumulatively added on
    for (level in levels) {
        test_col <- col
        suffixes <- c("", level, ".0")
        found <- FALSE
        for(suffix in suffixes) {
            test_col <- str_c(test_col, suffix)
            # at the shortest hit possible, we break
            if(test_col %in% col_names) {
                return(test_col)
            }
        }
    }

    if(!found) {
        stop(str_c("Invalid covariate '", covariate, "', not found in diffexp table."))
    }

}

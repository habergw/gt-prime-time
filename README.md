Code to replicate tables and figures from manuscript "Is group testing ready for prime-time in disease identification?" by Gregory Haber, Yaakov Malinovsky, and Paul S. Albert.

Table 1 and Figure 1 can be reproduced by running the file `R/create_table.R`.

To recreate Figure 2, first run simulations for each desired `p` by running `R CMD BATCH R/N_sims.R seed p` where `seed` and `p` and replaced by the desired values. The figure can then be recreated by running the file `R/create_plot.R`.

By changing the values in `R/N_sims.R` and varying the `p` input, it is possible to simulate the validation procedure for arbitrary choices of the sensitivity function and prevalence.

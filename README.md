Code to replicate tables and figures from manuscript "Is group testing ready for prime-time in disease identification?" by Gregory Haber, Yaakov Malinovsky, and Paul S. Albert.

Table 1 and Figure 1 can be reproduced by running the file `R/create_table.R`.

To recreate Figure 2, first run simulations for each desired `p` by running `R CMD BATCH '--args seed p' R/N_sims.R` where `seed` and `p` and replaced by the desired values. The figure can then be recreated by running the file `R/create_plot.R`.

Figure 2 is based on the following simulations:
```
R CMD BATCH '--args 1 0.001' R/N_sims.R
R CMD BATCH '--args 2 0.005' R/N_sims.R
R CMD BATCH '--args 3 0.01' R/N_sims.R
R CMD BATCH '--args 4 0.02' R/N_sims.R
R CMD BATCH '--args 5 0.03' R/N_sims.R
R CMD BATCH '--args 6 0.04' R/N_sims.R
R CMD BATCH '--args 7 0.05' R/N_sims.R
R CMD BATCH '--args 8 0.06' R/N_sims.R
R CMD BATCH '--args 9 0.07' R/N_sims.R
R CMD BATCH '--args 10 0.08' R/N_sims.R
R CMD BATCH '--args 11 0.09' R/N_sims.R
R CMD BATCH '--args 12 0.1' R/N_sims.R
```

By changing the values in `R/N_sims.R` and varying the `p` input, it is possible to simulate the validation procedure for arbitrary choices of the sensitivity function and prevalence.

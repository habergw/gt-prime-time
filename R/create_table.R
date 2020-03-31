# Code to replicate manuscript Figure 1 and Table 1
source("functions.R")

p <- seq(.01, .1, .01)
Se_thresh <- .95
Sp_thresh <- .95

k_max <- 25

# Figure 1
Se <- list(f_hwang(.1, 1:k_max, 0.01),
           f_hwang(.1, 1:k_max, 0.05),
           f_hwang(.1, 1:k_max, 0.075),
           f_hwang(.1, 1:k_max, 0.1),
           f_hwang(.1, 1:k_max, .3))

cairo_pdf("Figures/hwang.pdf", width = 7, height = 6)
par(mfcol = c(1, 1),
    mar = c(4.5, 4.5, 1, 1))
plot(1:k_max, Se[[1]], type = "l", ylim = c(0, 1),
     bty = "n",
     ylab= "Se(k)",
     xlab = "k",
     xlim = c(1, 25))
lapply(Se[-1], function(x) lines(1:k_max, x))
text(x = 23.26, y = 1.006, labels = "d = 0.01")
text(x = 23.26, y = 0.900, labels = "d = 0.05")
text(x = 23.45, y = 0.8357, labels = "d = 0.075")
text(x = 23.05, y = 0.7742, labels = "d = 0.1")
text(x = 23.05, y = 0.4763, labels = "d = 0.3")
dev.off()

# Table 1
grid <- expand.grid(1:6, 1:6)

res <- rbindlist(lapply(p, function(p)
            {Se <- list(f_hwang(p, 1:k_max, 0),
                        f_hwang(p, 1:k_max, 0.01),
           f_hwang(p, 1:k_max, 0.05),
           f_hwang(p, 1:k_max, 0.075),
           f_hwang(p, 1:k_max, .1),
           f_hwang(p, 1:k_max, .3))
            rbindlist(Map(function(x, y)
                compare_ests(p, k_max, Se, x, y, Se_thresh, Sp_thresh),
                grid[, 1], grid[, 2]))}))

res <- res[Se_true_ind == 4 & Se_est_ind != 4]
res <- res[, c("p", "k_est", "k_min",
                 "Se_est", "Se_true", "Se_min",
                 "ET_est", "ET_true", "ET_min",
                 "type", "Se_true_ind", "Se_est_ind")]

res <- dcast(res, p +Se_true_ind + Se_est_ind ~ type,
               value.var = names(res)[2:9])

res[, delta := rep(c(0, .01, .05, .1, .3), times = 10)]
ind <- c(1, 20, grep("free", names(res)), grep("const", names(res)))

res <- res[, ind, with = F]

res <- res[p %in% unique(res$p)[c(1, 5, 10)]]
res <- round(res, 3)

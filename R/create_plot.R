# Create manuscript Figure 2 after reading simulated validation data
source("R/functions.R")
files <- list.files("Results/")

sims <- read_sims(files)


lab_list <- list(c(0, 100000, 200000, 300000, 400000),
                 c(0, 200000, 400000, 600000, 800000),
                 c(0, 1000000, 2000000, 3000000, 4000000),
                 c(0, 20000, 60000, 100000, 140000))

labels <- list(c(expression(0), expression("1 x 10"^5), 
                 expression("2 x10"^5), expression("3 x 10"^5), 
                 expression("4 x 10"^5)),
                c(expression(0), expression("2 x 10"^5), 
                  expression("4 x10"^5), expression("6 x 10"^5), 
                  expression("8 x 10"^5)),
                c(expression(0), expression("1 x 10"^6), 
                  expression("2 x10"^6), expression("3 x 10"^6), 
                  expression("4 x 10"^6)),
                c(expression(0), expression("2 x 10"^4), 
                  expression("4 x10"^4), expression("10 x 10"^4), 
                  expression("14 x 10"^4)))

exp_list <- list(expression(bold("Se(k)" == "1 - 0.02 (k - 1)")),
                 expression(bold("Se(k)" == "1 - 0.02 x 2"^{"k / 2"})),
                 expression(bold("Se(k)" == "p" / ("1 - (1 - p)"^{"k"^".1"}))),
                 expression(bold("Se(k)" == "p" / ("1 - (1 - p)"^{"k"^".3"}))))

creat_bp <- function(ind) {
  mod <- letters[ind]
  tmp <- sims[model == mod]

  tmp2 <- tmp[, .(N, T - N, N_S - T - N), by = p]
  mat <- as.matrix(tmp2[, 2:4])

  barplot(t(mat), width = 2, names.arg = as.character(tmp2$p), axes = F,
          legend.text = c("N", expression(T[V]), expression("N"^"*")))
  axis(2, at = lab_list[[ind]], labels = labels[[ind]])
  mtext(exp_list[[ind]], side = 3, adj = 0)
}

cairo_pdf("Figures/N_plot.pdf", width = 10, height = 7)
par(mfrow = c(2, 2),
    mar = c(3, 3, 2, 0))
lapply(1:4, creat_bp)
dev.off()

dat <- dat.bcg

### calculate log risk ratios and corresponding sampling variances (and use
### the 'slab' argument to store study labels as part of the data frame)

dat <- dat[1:5, ]
dat <- dat %>%
  dplyr::rename(database = author) 


dat <- escalc(measure="OR", bi=tpos, ai=tneg, ci=cneg, di=cpos, data=dat, 
              slab = database)
dat$yi <- log(rnorm(5, 1.5, 0.2))
dat$vi <- runif(5, 0.01, 0.015)
### fit random-effects model
res <- rma(log(rnorm(6, 1.5, 0.2)), runif(6, 0.01, 0.015))


lab <- c("Rotterdam", "Utrecht", "Maastricht", "Groningen", "Nijmegen", "STIZON")

### forest plot with extra annotations
res <- rma(log(rnorm(6, 1.5, 0.2)), runif(6, 0.01, 0.015))
forest(res, atransf = exp, at = log(c(.5, 1, 2.5)),
       cex = .75, header = "Parkinsonism", slab = lab, width = 1)

res <- rma(log(rnorm(6, 1.5, 0.2)), runif(6, 0.01, 0.015))
forest(res, atransf = exp, at = log(c(.5, 1, 2.5)),
       cex = .75, header = "Diabetes", slab = lab, width = 1)

res <- rma(log(rnorm(6, 1.5, 0.2)), runif(6, 0.01, 0.015))
forest(res, atransf = exp, at = log(c(.5, 1, 2.5)),
       cex = .75, header = "Obesity", slab = lab, width = 1)

res <- rma(log(rnorm(6, 1.5, 0.2)), runif(6, 0.01, 0.015))
forest(res, atransf = exp, at = log(c(.5, 1, 2.5)),
       cex = .75, header = "Depresion", slab = lab, width = 1)

### add text with Q-value, dfs, p-value, and I^2 statistic
text(-16, -1, pos=4, cex=0.75, bquote(paste("RE Model (Q = ",
                                            .(formatC(res$QE, digits=2, format="f")), ", df = ", .(res$k - res$p),
                                            ", p = ", .(formatC(res$QEp, digits=2, format="f")), "; ", I^2, " = ",
                                            .(formatC(res$I2, digits=1, format="f")), "%)")))
library(ggplot2)
# Define quantile levels
tau <- c(0.2, 0.5, 0.8, 0.9, 0.95, 0.99, 0.999, 0.9999)

n.exp <- 250 # Total number of experiments
ns <- c(1e4, 1e6) # Sample sizes
type <- 1 # type=1 for GP, type=2 for log-normal


# Read in all MSE estimates
MSE.GPDNN <- MSE.gbex <- MSE.QNN <- MSE.evGAM <- array(dim = c(length(ns), length(tau), n.exp))
ind <- 1
for (n in ns) {
  for (i in 1:n.exp) {
    if (type == 1) boo <- try(load(paste0("sim_study_results/GPD/evGAM/n", n, "_exp", i, ".Rdata")), silent = T)
    if (type == 2) boo <- try(load(paste0("sim_study_results/lnorm/evGAM/n", n, "_exp", i, ".Rdata")), silent = T)

    if (class(boo) != "try-error") {
      MSE.evGAM[ind, , i] <- MSE
    }
    if (type == 1) boo <- try(load(paste0("sim_study_results/GPD/gbex/n", n, "_exp", i, ".Rdata")), silent = T)
    if (type == 2) boo <- try(load(paste0("sim_study_results/lnorm/gbex/n", n, "_exp", i, ".Rdata")), silent = T)
    if (class(boo) != "try-error") {
      MSE.gbex[ind, , i] <- MSE
    }
    if (type == 1) boo <- try(load(paste0("sim_study_results/GPD/Q_NN/n", n, "_exp", i, ".Rdata")), silent = T)
    if (type == 2) boo <- try(load(paste0("sim_study_results/lnorm/Q_NN/n", n, "_exp", i, ".Rdata")), silent = T)
    if (class(boo) != "try-error") {
      MSE.QNN[ind, , i] <- MSE
    }
    if (type == 1) boo <- try(load(paste0("sim_study_results/GPD/GPD_NN/n", n, "_exp", i, ".Rdata")), silent = T)
    if (type == 2) boo <- try(load(paste0("sim_study_results/lnorm/GPD_NN/n", n, "_exp", i, ".Rdata")), silent = T)
    if (class(boo) != "try-error") {
      MSE.GPDNN[ind, , i] <- MSE
    }
  }
  ind <- ind + 1
}


# plot n=1e4 results

# Create data frame for plotting
data <- data.frame(
  MSE = (c(c(MSE.evGAM[1, , ]), c(MSE.gbex[1, , ]), c(MSE.QNN[1, , ]), c(MSE.GPDNN[1, , ]))), tau = as.factor(0.95 + (1 - 0.95) * c(rep(tau, n.exp), rep(tau, n.exp), rep(tau, n.exp), rep(tau, n.exp))),
  label = c(rep("GAM", n.exp * length(tau)), rep("boosting", n.exp * length(tau)), rep("Q-MLP", n.exp * length(tau)), rep("GP-MLP", n.exp * length(tau)))
)
p <- ggplot(data, aes(x = tau, y = MSE, fill = label)) +
  xlab(expression(tau)) +
  ylab(expression(RMSNE(tau))) +
  geom_boxplot() +
  scale_y_continuous(trans = "log10") +
  scale_x_discrete(limits = factor(0.95 + (1 - 0.95) * tau[-c(1, 6, 5)]))
p <- p + theme(legend.position = "none") + theme(text = element_text(size = 16), axis.text.x = element_text(size = 16), axis.text.y = element_text(size = 16))
if (type == 1) p <- p + ggtitle(expression("GP: n = " ~ 10^4)) else p <- p + ggtitle(expression("log-normal: n =" ~ 10^4))
p
ggsave(p,
  filename = paste0("Figures/RMSNE_n", ns[1], "_type", type, ".pdf"),
  height = 7.5, width = 6, bg = "transparent"
)



# plot n=1e6 results

data <- data.frame(
  MSE = (c(c(MSE.evGAM[2, , ]), c(MSE.gbex[2, , ]), c(MSE.QNN[2, , ]), c(MSE.GPDNN[2, , ]))), tau = as.factor(0.95 + (1 - 0.95) * c(rep(tau, n.exp), rep(tau, n.exp), rep(tau, n.exp), rep(tau, n.exp))),
  label = c(rep("GAM", n.exp * length(tau)), rep("boosting", n.exp * length(tau)), rep("Q-MLP", n.exp * length(tau)), rep("GP-MLP", n.exp * length(tau)))
)
p <- ggplot(data, aes(x = tau, y = MSE, fill = label)) +
  xlab(expression(tau)) +
  ylab(expression(RMSNE(tau))) +
  geom_boxplot() +
  scale_y_continuous(trans = "log10") +
  scale_x_discrete(limits = factor(0.95 + (1 - 0.95) * tau[-c(1, 6, 5)]))
p <- p + theme(legend.position = "none") + theme(text = element_text(size = 16), axis.text.x = element_text(size = 16), axis.text.y = element_text(size = 16))
if (type == 1) p <- p + ggtitle(expression("GP: n = " ~ 10^6)) else p <- p + ggtitle(expression("log-normal: n =" ~ 10^6))
p

ggsave(p,
  filename = paste0("Figures/RMSNE_n", ns[2], "_type", type, ".pdf"),
  height = 7.5, width = 6, bg = "transparent"
)


# Plot with the legend attached


data <- data.frame(
  MSE = (c(c(MSE.evGAM[2, , ]), c(MSE.gbex[2, , ]), c(MSE.QNN[2, , ]), c(MSE.GPDNN[, , ]))), tau = as.factor(0.95 + (1 - 0.95) * c(rep(tau, n.exp), rep(tau, n.exp), rep(tau, n.exp), rep(tau, n.exp))),
  label = c(rep("GAM", n.exp * length(tau)), rep("boosting", n.exp * length(tau)), rep("Q-MLP", n.exp * length(tau)), rep("GP-MLP", n.exp * length(tau)))
)
p <- ggplot(data, aes(x = tau, y = MSE, fill = label)) +
  xlab(expression(tau)) +
  ylab(expression(RMSNE(tau))) +
  geom_boxplot() +
  scale_y_continuous(trans = "log10") +
  scale_x_discrete(limits = factor(0.95 + (1 - 0.95) * tau[-c(1, 6, 5)]))
p <- p + guides(fill = guide_legend("")) +
  theme(legend.position = "bottom") + theme(text = element_text(size = 16), axis.text.x = element_text(size = 16), axis.text.y = element_text(size = 16))
p

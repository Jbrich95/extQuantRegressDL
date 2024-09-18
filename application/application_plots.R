load("Data/monthly_max_data.Rdata")
source("bGEV_loss_functions.R")

n.boot <- 200


load(paste0("application/Predictions/preds_job_29860036_boot9.Rdata"))

all_preds <- array(dim = c(dim(preds), n.boot))

for (it in 1:n.boot) {
  boo <- try(load(paste0("application/Predictions/preds_job_29860036_boot", it, ".Rdata")))
  if (class(boo) != "try-error") all_preds[, , , it] <- preds

  print(it)
}


# Predicted parameters


t_inds <- c(60, 45, 12, 38)

S <- cbind(c(coords[, 1]), c(coords[, 2]))

library(ggplot2)
library(ggmap)

indbox <- make_bbox(lon = c(min(S[, 1]), max(S[, 1])), lat = c(max(S[, 2]), min(S[, 2])), f = 0)

world <- map_data("world")


ggp <- ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    color = "black", fill = "lightgray", size = 0.1
  ) +
  scale_x_continuous(limits = indbox[c(1, 3)], expand = c(0, 0)) +
  scale_y_continuous(limits = indbox[c(2, 4)], expand = c(0, 0)) +
  coord_fixed(ratio = 1.4)


for (t in t_inds) {
  temp <- apply(all_preds[t, , 1, ], 1, median, na.rm = T)
  cols <- c("#fff7fb", "#ece7f2", "#d0d1e6", "#a6bddb", "#74a9cf", "#3690c0", "#0570b0", "#045a8d", "#023858")
  brks <- seq(floor(min((temp))), ceiling(max((temp))), length = 10)
  brks <- ceiling(brks * 10) / 10


  data <- data.frame(lon = S[, 1], lat = S[, 2], value = temp)
  p0 <- ggp + theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.margin = unit(c(0, 0, -1, -1), "lines")
  ) + geom_tile(data, mapping = aes(x = lon, y = lat, fill = value), width = 0.27, height = 0.27) + xlab("") + ylab("")
  # plot(p0)
  p1 <- p0 + scale_fill_stepsn(breaks = brks, colors = cols, name = expression(alpha), limits = range(brks)) + theme(
    legend.key.size = unit(0.8, "in"),
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 20),
    plot.background = element_rect(fill = "transparent", colour = NA),
    legend.background = element_rect(fill = "transparent", colour = NA),
  )
  # plot(p1)


  p2 <- p1 + geom_path(data = world, aes(x = long, y = lat, group = group), colour = "black")
  p2 <- p2 + theme(rect = element_rect(fill = "transparent"))
  ggsave(p2,
    filename = paste0("application/Figures/loc_t", t, "_median.pdf"),
    height = 6, width = 6, bg = "transparent"
  )


  temp <- apply(all_preds[t, , 2, ], 1, median, na.rm = T)
  brks <- seq(floor(min((temp))), ceiling(max((temp))), length = 10)
  brks <- ceiling(brks * 10) / 10


  data <- data.frame(lon = S[, 1], lat = S[, 2], value = temp)
  p0 <- ggp + theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.margin = unit(c(0, 0, -1, -1), "lines")
  ) + geom_tile(data, mapping = aes(x = lon, y = lat, fill = value), width = 0.27, height = 0.27) + xlab("") + ylab("")
  # plot(p0)
  p1 <- p0 + scale_fill_stepsn(breaks = brks, colors = cols, name = expression(s), limits = range(brks)) + theme(
    legend.key.size = unit(0.8, "in"),
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 20),
    plot.background = element_rect(fill = "transparent", colour = NA),
    legend.background = element_rect(fill = "transparent", colour = NA),
  )
  # plot(p1)


  p2 <- p1 + geom_path(data = world, aes(x = long, y = lat, group = group), colour = "black")
  p2 <- p2 + theme(rect = element_rect(fill = "transparent"))
  ggsave(p2,
    filename = paste0("application/Figures/s_t", t, "_median.pdf"),
    height = 6, width = 6, bg = "transparent"
  )


  temp <- apply(all_preds[t, , 3, ], 1, median, na.rm = T)
  cols <- c("#fff7fb", "#ece7f2", "#d0d1e6", "#a6bddb", "#74a9cf", "#3690c0", "#0570b0", "#045a8d", "#023858")
  brks <- seq(floor(min((temp))), max((temp)), length = 10)
  brks <- ceiling(brks * 100) / 100


  data <- data.frame(lon = S[, 1], lat = S[, 2], value = temp)
  p0 <- ggp + theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.margin = unit(c(0, 0, -1, -1), "lines")
  ) + geom_tile(data, mapping = aes(x = lon, y = lat, fill = value), width = 0.27, height = 0.27) + xlab("") + ylab("")
  # plot(p0)
  p1 <- p0 + scale_fill_stepsn(breaks = brks, colors = cols, name = expression(xi), limits = range(brks)) + theme(
    legend.key.size = unit(0.8, "in"),
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 20),
    plot.background = element_rect(fill = "transparent", colour = NA),
    legend.background = element_rect(fill = "transparent", colour = NA),
  )
  # plot(p1)


  p2 <- p1 + geom_path(data = world, aes(x = long, y = lat, group = group), colour = "black")
  p2 <- p2 + theme(rect = element_rect(fill = "transparent"))
  ggsave(p2,
    filename = paste0("application/Figures/xi_t", t, "_median.pdf"),
    height = 6, width = 6, bg = "transparent"
  )


  temp <- Y[t, ]
  cols <- c("#fff7fb", "#ece7f2", "#d0d1e6", "#a6bddb", "#74a9cf", "#3690c0", "#0570b0", "#045a8d", "#023858")
  brks <- seq(floor(min((temp))), ceiling(max((temp))), length = 10)
  brks <- ceiling(brks * 10) / 10


  data <- data.frame(lon = S[, 1], lat = S[, 2], value = temp)
  p0 <- ggp + theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.margin = unit(c(0, 0, -1, -1), "lines")
  ) + geom_tile(data, mapping = aes(x = lon, y = lat, fill = value), width = 0.27, height = 0.27) + xlab("") + ylab("")
  # plot(p0)
  p1 <- p0 + scale_fill_stepsn(breaks = brks, colors = cols, name = expression(Y), limits = range(brks)) + theme(
    legend.key.size = unit(0.8, "in"),
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 20),
    plot.background = element_rect(fill = "transparent", colour = NA),
    legend.background = element_rect(fill = "transparent", colour = NA),
  )
  # plot(p1)


  p2 <- p1 + geom_path(data = world, aes(x = long, y = lat, group = group), colour = "black")
  p2 <- p2 + theme(rect = element_rect(fill = "transparent"))
  ggsave(p2,
    filename = paste0("application/Figures/obs_t", t, "_median.pdf"),
    height = 6, width = 6, bg = "transparent"
  )
  print(times[t])



  temp <- apply(all_preds[t, , , ], c(1, 3), function(x) {
    qbGEV(0.9,
      q_a = x[1], s_b = x[2], xi = x[3],
      alpha = 0.5, beta = 0.5, p_a = 0.05, p_b = 0.2, c1 = 5, c2 = 5
    )
  })


  temp <- apply(temp, 1, median, na.rm = T)
  cols <- c("#fff7fb", "#ece7f2", "#d0d1e6", "#a6bddb", "#74a9cf", "#3690c0", "#0570b0", "#045a8d", "#023858")
  brks <- seq(floor(min((temp))), max((temp)), length = 10)
  brks <- ceiling(brks * 10) / 10


  data <- data.frame(lon = S[, 1], lat = S[, 2], value = temp)
  p0 <- ggp + theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.margin = unit(c(0, 0, -1, -1), "lines")
  ) + geom_tile(data, mapping = aes(x = lon, y = lat, fill = value), width = 0.27, height = 0.27) + xlab("") + ylab("")
  # plot(p0)
  p1 <- p0 + scale_fill_stepsn(breaks = brks, colors = cols, name = expression(Q[x](tau)), limits = range(brks)) + theme(
    legend.key.size = unit(0.8, "in"),
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 20),
    plot.background = element_rect(fill = "transparent", colour = NA),
    legend.background = element_rect(fill = "transparent", colour = NA),
  )
  # plot(p1)


  p2 <- p1 + geom_path(data = world, aes(x = long, y = lat, group = group), colour = "black")
  p2 <- p2 + theme(rect = element_rect(fill = "transparent"))
  ggsave(p2,
    filename = paste0("application/Figures/q_0.9_t", t, "_median.pdf"),
    height = 6, width = 6, bg = "transparent"
  )

  temp <- apply(all_preds[t, , , ], c(1, 3), function(x) {
    qbGEV(0.99,
      q_a = x[1], s_b = x[2], xi = x[3],
      alpha = 0.5, beta = 0.5, p_a = 0.05, p_b = 0.2, c1 = 5, c2 = 5
    )
  })


  temp <- apply(temp, 1, median, na.rm = T)
  cols <- c("#fff7fb", "#ece7f2", "#d0d1e6", "#a6bddb", "#74a9cf", "#3690c0", "#0570b0", "#045a8d", "#023858")
  brks <- seq(floor(min((temp))), max((temp)), length = 10)
  brks <- ceiling(brks * 10) / 10


  data <- data.frame(lon = S[, 1], lat = S[, 2], value = temp)
  p0 <- ggp + theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.margin = unit(c(0, 0, -1, -1), "lines")
  ) + geom_tile(data, mapping = aes(x = lon, y = lat, fill = value), width = 0.27, height = 0.27) + xlab("") + ylab("")
  # plot(p0)
  p1 <- p0 + scale_fill_stepsn(breaks = brks, colors = cols, name = expression(Q[x](tau)), limits = range(brks)) + theme(
    legend.key.size = unit(0.8, "in"),
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 20),
    plot.background = element_rect(fill = "transparent", colour = NA),
    legend.background = element_rect(fill = "transparent", colour = NA),
  )
  # plot(p1)


  p2 <- p1 + geom_path(data = world, aes(x = long, y = lat, group = group), colour = "black")
  p2 <- p2 + theme(rect = element_rect(fill = "transparent"))
  ggsave(p2,
    filename = paste0("application/Figures/q_0.99_t", t, "_median.pdf"),
    height = 6, width = 6, bg = "transparent"
  )

  temp <- apply(all_preds[t, , , ], c(1, 3), function(x) {
    qbGEV(0.999,
      q_a = x[1], s_b = x[2], xi = x[3],
      alpha = 0.5, beta = 0.5, p_a = 0.05, p_b = 0.2, c1 = 5, c2 = 5
    )
  })


  temp <- apply(temp, 1, median, na.rm = T)
  cols <- c("#fff7fb", "#ece7f2", "#d0d1e6", "#a6bddb", "#74a9cf", "#3690c0", "#0570b0", "#045a8d", "#023858")
  brks <- seq(floor(min((temp))), max((temp)), length = 10)
  brks <- ceiling(brks * 10) / 10


  data <- data.frame(lon = S[, 1], lat = S[, 2], value = temp)
  p0 <- ggp + theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.margin = unit(c(0, 0, -1, -1), "lines")
  ) + geom_tile(data, mapping = aes(x = lon, y = lat, fill = value), width = 0.27, height = 0.27) + xlab("") + ylab("")
  # plot(p0)
  p1 <- p0 + scale_fill_stepsn(breaks = brks, colors = cols, name = expression(Q[x](tau)), limits = range(brks)) + theme(
    legend.key.size = unit(0.8, "in"),
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 20),
    plot.background = element_rect(fill = "transparent", colour = NA),
    legend.background = element_rect(fill = "transparent", colour = NA),
  )
  # plot(p1)


  p2 <- p1 + geom_path(data = world, aes(x = long, y = lat, group = group), colour = "black")
  p2 <- p2 + theme(rect = element_rect(fill = "transparent"))
  ggsave(p2,
    filename = paste0("application/Figures/q_0.999_t", t, "_median.pdf"),
    height = 6, width = 6, bg = "transparent"
  )

  temp <- apply(all_preds[t, , , ], c(1, 3), function(x) {
    qbGEV(0.9999,
      q_a = x[1], s_b = x[2], xi = x[3],
      alpha = 0.5, beta = 0.5, p_a = 0.05, p_b = 0.2, c1 = 5, c2 = 5
    )
  })


  temp <- apply(temp, 1, median, na.rm = T)
  cols <- c("#fff7fb", "#ece7f2", "#d0d1e6", "#a6bddb", "#74a9cf", "#3690c0", "#0570b0", "#045a8d", "#023858")
  brks <- seq(floor(min((temp))), max((temp)), length = 10)
  brks <- ceiling(brks * 10) / 10


  data <- data.frame(lon = S[, 1], lat = S[, 2], value = temp)
  p0 <- ggp + theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.margin = unit(c(0, 0, -1, -1), "lines")
  ) + geom_tile(data, mapping = aes(x = lon, y = lat, fill = value), width = 0.27, height = 0.27) + xlab("") + ylab("")
  # plot(p0)
  p1 <- p0 + scale_fill_stepsn(breaks = brks, colors = cols, name = expression(Q[x](tau)), limits = range(brks)) + theme(
    legend.key.size = unit(0.8, "in"),
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 20),
    plot.background = element_rect(fill = "transparent", colour = NA),
    legend.background = element_rect(fill = "transparent", colour = NA),
  )
  # plot(p1)


  p2 <- p1 + geom_path(data = world, aes(x = long, y = lat, group = group), colour = "black")
  p2 <- p2 + theme(rect = element_rect(fill = "transparent"))
  ggsave(p2,
    filename = paste0("application/Figures/q_0.9999_t", t, "_median.pdf"),
    height = 6, width = 6, bg = "transparent"
  )
}







# Transform to exp margins


dat <- c(Y)
all_exp <- array(dim = c(length(dat), n.boot))
for (it in 1:n.boot) {
  pred_tall <- matrix(ncol = 3, nrow = length(dat))
  for (i in 1:3) pred_tall[, i] <- c(all_preds[, , i, it])

  if (!is.na(pred_tall[1])) {
    all_exp[, it] <- apply(cbind(dat, pred_tall), 1, function(x) {
      (pbGEV(x[1],
        q_a = x[2], s_b = x[3], xi = x[4],
        alpha = 0.5, beta = 0.5, p_a = 0.05, p_b = 0.2, c1 = 5, c2 = 5
      ))
    })
  }
  print(it)
}

all_exp <- qexp(all_exp)

p_min <- 0
n_p <- 25000
ps <- seq(0, length(dat) / (length(dat) + 1), length = n_p)
qs <- array(dim = c(n.boot, length(ps)))
for (i in 1:n.boot) {
  qs[i, ] <- quantile(all_exp[, i], ps, na.rm = T)
}
qs_med <- apply(qs, 2, median, na.rm = T)

qupper <- apply(qs, 2, quantile, prob = 0.975, na.rm = T)
qlower <- apply(qs, 2, quantile, prob = 0.025, na.rm = T)
pdf(file = paste0("application/Figures/bGEV_exp.pdf"), width = 7, height = 7)
plot(qexp(ps), qs_med,
  xlab = "Theoretical", ylab = "Model", main = "", pch = 20,
  ylim = range(0, qupper), xlim = range(0, qupper)
)
abline(a = 0, b = 1, col = "red")
points(qexp(ps), qupper, type = "l", lty = 2, col = "blue", lwd = 2)
points(qexp(ps), qlower, type = "l", lty = 2, col = "blue", lwd = 2)

dev.off()

qq_95 <- data.frame(t_exp = qexp(ps), ylow = qlower, yup = qupper, ymedian = qs_med)
lim0 <- range(qq_95$yup)

# Save the gplot

p <- ggplot(qq_95) +
  geom_point(aes(x = t_exp, y = ymedian), size = 1.2) +
  xlim(0, 15.2) +
  ylim(0, 15.2) +
  geom_line(aes(x = t_exp, y = ylow), linetype = 5, col = "blue") +
  geom_line(aes(x = t_exp, y = yup), linetype = 5, col = "blue") +
  labs(x = "Theoretical", y = "Model") +
  geom_abline(intercept = 0, slope = 1, col = "red") +
  theme(text = element_text(size = 26), axis.text.x = element_text(size = 26), axis.text.y = element_text(size = 26))
ggsave(p, file = paste0("application/Figures/bGEV_exp.pdf"), width = 8, height = 8)

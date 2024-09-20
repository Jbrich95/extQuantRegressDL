rm(list = ls())

library(ggplot2)
library(ggmap)


# Read data and loss functions
load("monthly_max_data.Rdata")
source("bGEV_loss_functions.R")

# Set number of bootstrap samples
n.boot <- 200

#Setting n.boot <- 0 will plot the results for the original data only.

# Read in all predictions
if(n.boot > 0){
all_preds <- array(dim = c(c(dim(Y), 3), n.boot))
for (it in 1:n.boot) {
  load(paste0("Predictions/preds_boot", it, ".Rdata"))
  all_preds[, , , it] <- preds
}
}else if (n.boot==0){
  all_preds <- array(dim = c(c(dim(Y), 3), 1))
  load(paste0("Predictions/preds_boot", 0, ".Rdata"))
  all_preds[, , , 1] <- preds
  
}

# We now plot maps of the median predicted parameters/return levels for a specific month.

t.ind <- 38 # This will plot the maps for July 2013.
print(times[t.ind])

# Colour palette
cols <- c("#fff7fb", "#ece7f2", "#d0d1e6", "#a6bddb", "#74a9cf", "#3690c0", "#0570b0", "#045a8d", "#023858")

# Define the map
S <- cbind(c(coords[, 1]), c(coords[, 2]))
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


# Plot the location parameter alpha(x)

# Get bootstrap median
if(n.boot >0) loc.med <- apply(all_preds[t.ind, , 1, ], 1, median, na.rm = T) else loc.med <- all_preds[t.ind, , 1, ]

# Define the breaks in the colour key
brks <- seq(floor(min((loc.med))), ceiling(max((loc.med))), length = 10)
brks <- ceiling(brks * 10) / 10

# Define data.frame
df <- data.frame(lon = S[, 1], lat = S[, 2], value = loc.med)

# ggplot
p <- ggp + theme(
  axis.line = element_blank(),
  axis.text = element_blank(),
  axis.ticks = element_blank(),
  plot.margin = unit(c(0, 0, -1, -1), "lines")
) + geom_tile(df, mapping = aes(x = lon, y = lat, fill = value), width = 0.27, height = 0.27) +
  xlab("") + ylab("") +
  scale_fill_stepsn(breaks = brks, colors = cols, name = expression(alpha), limits = range(brks)) +
  theme(
    legend.key.size = unit(0.8, "in"),
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 20),
    plot.background = element_rect(fill = "transparent", colour = NA),
    legend.background = element_rect(fill = "transparent", colour = NA),
  ) + geom_path(data = world, aes(x = long, y = lat, group = group), colour = "black") +
  theme(rect = element_rect(fill = "transparent"))

p

# Save
if(n.boot >0){
ggsave(p,
  filename = paste0("Figures/location_median.pdf"),
  height = 6, width = 6, bg = "transparent"
)
}else{
  ggsave(p,
         filename = paste0("Figures/location_original.pdf"),
         height = 6, width = 6, bg = "transparent"
  )
}
# Plot the scale parameter s(x)

# Get bootstrap median
if(n.boot > 0) s.med <- apply(all_preds[t.ind, , 2, ], 1, median, na.rm = T) else s.med <- all_preds[t.ind, , 2, ]

# Define the breaks in the colour key
brks <- seq(floor(min((s.med))), ceiling(max((s.med))), length = 10)
brks <- ceiling(brks * 10) / 10

# Define data.frame

df <- data.frame(lon = S[, 1], lat = S[, 2], value = s.med)

# ggplot
p <- ggp + theme(
  axis.line = element_blank(),
  axis.text = element_blank(),
  axis.ticks = element_blank(),
  plot.margin = unit(c(0, 0, -1, -1), "lines")
) + geom_tile(df, mapping = aes(x = lon, y = lat, fill = value), width = 0.27, height = 0.27) +
  xlab("") + ylab("") +
  scale_fill_stepsn(breaks = brks, colors = cols, name = expression(s), limits = range(brks)) +
  theme(
    legend.key.size = unit(0.8, "in"),
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 20),
    plot.background = element_rect(fill = "transparent", colour = NA),
    legend.background = element_rect(fill = "transparent", colour = NA),
  ) + geom_path(data = world, aes(x = long, y = lat, group = group), colour = "black") +
  theme(rect = element_rect(fill = "transparent"))

p

# Save
if(n.boot > 0){
ggsave(p,
  filename = paste0("Figures/scale_median.pdf"),
  height = 6, width = 6, bg = "transparent"
)
}else{
  ggsave(p,
         filename = paste0("Figures/scale_original.pdf"),
         height = 6, width = 6, bg = "transparent"
  )
  
}

# Plot the shape parameter xi(x)

# Get bootstrap median
if(n.boot >0) xi.med <- apply(all_preds[t.ind, , 3, ], 1, median, na.rm = T) else xi.med <- all_preds[t.ind,,3,]

# Define the breaks in the colour key
brks <- seq(floor(min((xi.med))), max((xi.med)), length = 10)
brks <- ceiling(brks * 100) / 100

# data frame
df <- data.frame(lon = S[, 1], lat = S[, 2], value = xi.med)

# ggplot
p <- ggp + theme(
  axis.line = element_blank(),
  axis.text = element_blank(),
  axis.ticks = element_blank(),
  plot.margin = unit(c(0, 0, -1, -1), "lines")
) + geom_tile(df, mapping = aes(x = lon, y = lat, fill = value), width = 0.27, height = 0.27) +
  xlab("") + ylab("") +
  scale_fill_stepsn(breaks = brks, colors = cols, name = expression(xi), limits = range(brks)) +
  theme(
    legend.key.size = unit(0.8, "in"),
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 20),
    plot.background = element_rect(fill = "transparent", colour = NA),
    legend.background = element_rect(fill = "transparent", colour = NA),
  ) + geom_path(data = world, aes(x = long, y = lat, group = group), colour = "black") +
  theme(rect = element_rect(fill = "transparent"))

p

if(n.boot > 0){
ggsave(p,
  filename = paste0("Figures/xi_median.pdf"),
  height = 6, width = 6, bg = "transparent"
)
}else{
  ggsave(p,
         filename = paste0("Figures/xi_original.pdf"),
         height = 6, width = 6, bg = "transparent"
  ) 
}


# Plot the observation

# Define the breaks in the colour key

brks <- seq(floor(min((Y[t.ind, ]))), ceiling(max((Y[t.ind, ]))), length = 10)
brks <- ceiling(brks * 10) / 10

df <- data.frame(lon = S[, 1], lat = S[, 2], value = Y[t.ind, ])
p <- ggp + theme(
  axis.line = element_blank(),
  axis.text = element_blank(),
  axis.ticks = element_blank(),
  plot.margin = unit(c(0, 0, -1, -1), "lines")
) + geom_tile(df, mapping = aes(x = lon, y = lat, fill = value), width = 0.27, height = 0.27) +
  xlab("") + ylab("") +
  scale_fill_stepsn(breaks = brks, colors = cols, name = expression(Y), limits = range(brks)) +
  theme(
    legend.key.size = unit(0.8, "in"),
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 20),
    plot.background = element_rect(fill = "transparent", colour = NA),
    legend.background = element_rect(fill = "transparent", colour = NA),
  ) + geom_path(data = world, aes(x = long, y = lat, group = group), colour = "black") +
  theme(rect = element_rect(fill = "transparent"))

p
ggsave(p,
  filename = paste0("Figures/ob.pdf"),
  height = 6, width = 6, bg = "transparent"
)


# We now plot the tau  = 0.9 and tau = 0.9999 quantiles
taus <- c(0.9, 0.9999)

for (tau in taus) {
  # evaluate quantile function for bGEV
  if(n.boot > 0){
  q <- apply(all_preds[t.ind, , , ], c(1, 3), function(x) {
    qbGEV(tau,
      q_a = x[1], s_b = x[2], xi = x[3],
      alpha = 0.5, beta = 0.5, p_a = 0.05, p_b = 0.2, c1 = 5, c2 = 5
    )
  })
  }else{
    q <- apply(all_preds[t.ind, , , ], c(1), function(x) {
      qbGEV(tau,
            q_a = x[1], s_b = x[2], xi = x[3],
            alpha = 0.5, beta = 0.5, p_a = 0.05, p_b = 0.2, c1 = 5, c2 = 5
      )
    })
  }
  # Get bootstrap median
 
  if(n.boot>0)  q.med <- apply(q, 1, median, na.rm = T) else q.med <- q

  # Define the breaks in the colour key

  brks <- seq(floor(min((q.med))), max((q.med)), length = 10)
  brks <- ceiling(brks * 10) / 10


  df <- data.frame(lon = S[, 1], lat = S[, 2], value = q.med)

  p <- ggp + theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.margin = unit(c(0, 0, -1, -1), "lines")
  ) + geom_tile(df, mapping = aes(x = lon, y = lat, fill = value), width = 0.27, height = 0.27) +
    xlab("") + ylab("") +
    scale_fill_stepsn(breaks = brks, colors = cols, 
                      name = expression(Q[x](tau)), limits = range(brks)) +
    theme(
      legend.key.size = unit(0.8, "in"),
      legend.text = element_text(size = 15),
      legend.title = element_text(size = 20),
      plot.background = element_rect(fill = "transparent", colour = NA),
      legend.background = element_rect(fill = "transparent", colour = NA),
    ) + geom_path(data = world, aes(x = long, y = lat, group = group), colour = "black") +
    theme(rect = element_rect(fill = "transparent"))

  p
if(n.boot>0){
  ggsave(p,
    filename = paste0("Figures/q_", tau, "_median.pdf"),
    height = 6, width = 6, bg = "transparent"
  )
}else{
  ggsave(p,
         filename = paste0("Figures/q_", tau, "_original.pdf"),
         height = 6, width = 6, bg = "transparent"
  )
}
}




# We now plot the pooled QQ diagnostic in Figure 1.5.


# First, transform the data to unit exponential margins for each bootstrap sample.

# This will take some time!

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


n_p <- 25000 # Number of quantiles to consider


ps <- seq(0, length(dat) / (length(dat) + 1), length = n_p)

# Evaluate quantiles for each bootstrap sample
qs <- array(dim = c(n.boot, length(ps)))
for (i in 1:n.boot) {
  qs[i, ] <- quantile(all_exp[, i], ps, na.rm = T)
}

# Get pointwise medians, 0.025 and 0.975 quantiles
qs_med <- apply(qs, 2, median, na.rm = T)
qupper <- apply(qs, 2, quantile, prob = 0.975, na.rm = T)
qlower <- apply(qs, 2, quantile, prob = 0.025, na.rm = T)

# make data frame for ggplot
df <- data.frame(t_exp = qexp(ps), ylow = qlower, yup = qupper, ymedian = qs_med)
lim0 <- range(df$yup)

# Save the gplot

p <- ggplot(df) +
  geom_point(aes(x = t_exp, y = ymedian), size = 1.2) +
  xlim(0, 15.2) +
  ylim(0, 15.2) +
  geom_line(aes(x = t_exp, y = ylow), linetype = 5, col = "blue") +
  geom_line(aes(x = t_exp, y = yup), linetype = 5, col = "blue") +
  labs(x = "Theoretical", y = "Model") +
  geom_abline(intercept = 0, slope = 1, col = "red") +
  theme(
    text = element_text(size = 26), axis.text.x = element_text(size = 26),
    axis.text.y = element_text(size = 26)
  )
p
ggsave(p, file = paste0("Figures/bGEV_pooledQQ.pdf"), width = 8, height = 8)

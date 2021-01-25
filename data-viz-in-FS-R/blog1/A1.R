# Question 1
age.at.length <- read.csv("age_length_data.csv")
catch.at.length <- read.csv("catch_length.csv")

library(dplyr)

age.at.length <- age.at.length %>% mutate(total = apply(age.at.length[, -1], 1, sum))
age.at.length.in.proportion <- age.at.length[, c(2:14)] / age.at.length$total
catch.at.age <- age.at.length.in.proportion * catch.at.length$Count
catch.at.age <- apply(catch.at.age[, -13], 2, sum, na.rm = T)
age.at.length.in.proportion.t <- t(age.at.length.in.proportion)
colnames(age.at.length.in.proportion.t) <- catch.at.length$Length
row.names(age.at.length.in.proportion.t) <- c(3:15)
image(as.matrix(age.at.length.in.proportion[, -13]),
  xaxt = "n", yaxt = "n",
  xlab = "Length", ylab = "Age"
)

catch.at.age


barplot(catch.at.age,
  names = c(3:14),
  xlab = "Age (unit)", ylab = "Number of fish",
  main = "Catch at age", col = "lightblue", border = NA
)
barplot(cat.at.length$Count,
  names = catch.at.length$Length,
  xlab = "Length (unit)", ylab = "Number of fish", main = "Catch at length",
  col = "lightblue", border = NA
)

library(RColorBrewer)
col1 <- brewer.pal(nrow(age.at.length.in.proportion.t), "Paired")
options(warn = -1)
barplot(age.at.length.in.proportion.t[-13, ],
  col = col1, xlim = c(0, 88), xlab = "Length (unit)",
  ylab = "Frequency", border = NA
)
legend1 <- legend("topright",
  legend = c(3:14),
  title = "Age (unit)", fill = col1, box.col = "white"
)

# Question 2
catch.at.length.3Ps <- read.csv("catch_length_3Ps.csv")
colnames(catch.at.length.3Ps) <- c("length", array(1959:2016))
t1 <- apply(catch.at.length.3Ps[1:53, -1], 2, sum)
t2 <- catch.at.length.3Ps[54:116, -1]
t3 <- apply(catch.at.length.3Ps[117:122, -1], 2, sum)
Y <- rbind(t1, t2, t3)

age.at.length.in.proportion.2 <- data.frame(
  length = c(53:117),
  age.at.length.in.proportion[, -13]
)
X <- age.at.length.in.proportion.2[, -1]

S1 <- list()
S2 <- list()
S3 <- list()

for (i in 1:ncol(Y)) {
  S1[i] <- subset(Y, select = c(i))
  S2[[i]] <- X * S1[[i]]
  S3[[i]] <- apply(S2[[i]], 2, sum, na.rm = TRUE)
}

catch.at.age.ts <- as.data.frame(do.call(cbind, S3))
colnames(catch.at.age.ts) <- c(1959:2016)
row.names(catch.at.age.ts) <- c(3:14)

X <- as.matrix(catch.at.age.ts)
X2 <- X %*% diag(1 / apply(X, 2, sum))
colnames(X2) <- c(1959:2016)

# Proportion at age
barplot(X2,
  col = col1, xlim = c(0, 80), main = "Propotion at age",
  xlab = "Year", ylab = "Frequency", border = NA
)
legend1 <- legend("topright",
  legend = c(3:14), title = "Age",
  fill = col1, box.col = "white"
)


# data preparation for ridge plot for age
{
  library(tidyr)
  ra1 <- data.frame(year = c(1959:2016), t(X2))
  colnames(ra1) <- c("year", 3:14)
  ra2 <- gather(ra1, "age", "proportion", -year)
}
# ridge plot
{
  library(ggplot2)
  library(ggridges)
  pr <- ggplot(r2) +
    theme_ridges(grid = TRUE, center_axis_labels = TRUE, font_size = 10) +
    geom_density_ridges(data = ra2, aes(
      x = as.numeric(age), y = factor(year),
      height = proportion
    ), stat = "identity", scale = 8, alpha = .8, show.legend = TRUE) +
    xlab("Age") +
    ylab("Year") +
    scale_x_continuous(breaks = seq(3, 14, 1)) +
    scale_y_discrete(breaks = seq(1959, 2016, 5))
  pr
}

# data preparation for ridge plot for length
{
  library(dplyr)
  rl1 <- as.data.frame(t(catch.at.length.3Ps[, -1]))
  rl1 <- rl1 %>% mutate(total = apply(rl1, 1, sum))
  rl2 <- data.frame(year = c(1959:2016), rl1 / rl1$total)
  rl2 <- rl2[, -124]
  colnames(rl2) <- c("year", 1:122)
  rl3 <- gather(rl2, "length", "value", -year)
}
# ridge plot
{
  library(ggplot2)
  library(ggridges)
  pr2 <- ggplot(rl3) +
    theme_ridges(grid = T, center_axis_labels = TRUE, font_size = 10) +
    geom_density_ridges(
      data = rl3, aes(
        x = as.numeric(length),
        y = factor(year), height = value
      ), stat = "identity", scale = 8,
      alpha = .8, show.legend = TRUE
    ) +
    xlab("Length") +
    ylab("Year") +
    scale_x_continuous(breaks = seq(1, 122, 10)) +
    scale_y_discrete(breaks = seq(1959, 2016, 5))
  pr2
}

# mean length & age over time series
catch.sum.l <- apply(catch.at.length.3Ps[, -1], 2, sum, na.rm = TRUE)
p1 <- t(catch.at.length.3Ps[, -1]) / catch.sum.l
p2 <- t(p1) * catch.at.length.3Ps[, 1]
p3 <- apply(p2, 2, sum, na.rm = TRUE)
p3 <- data.frame(year = c(1959:2016), mean.length = p3)

catch.sum.a <- apply(catch.at.age.ts, 2, sum, na.rm = TRUE)
p4 <- t(catch.at.age.ts) / catch.sum.a
p5 <- t(p4) * c(3:14)
p6 <- apply(p5, 2, sum, na.rm = TRUE)
p6 <- data.frame(year = c(1959:2016), mean.age = p6)

par(mfrow = c(1, 2))
plot(p6,
  type = "l", main = "Mean length over time", xlab = NA,
  ylab = "mean length (unit)"
)
plot(p3,
  type = "l", main = "Mean age over time", xlab = NA,
  ylab = "mean age (unit)"
)

# spay plot



# Question 3
survey.data <- read.table("survey.dat", header = TRUE)
N <- sum(tapply(survey.data$Nh, survey.data$stratum, unique))

C1 <- list()
C2 <- list()
C3 <- list()
C4 <- list()
C5 <- list()
C6 <- list()

for (i in 1:9) {
  C1[i] <- subset(survey.data, survey.data$stratum == i, select = c("catch"))
  C2[i] <- subset(survey.data, survey.data$stratum == i, select = c("Nh"))
  # compute weighted mean
  C3[[i]] <- mean(C1[[i]]) * mean(C2[[i]])
  C4[[i]] <- mean(C2[[i]])
  # compute var of the estimate
  C6[[i]] <- (C4[[i]] / N)^2 * ((1 / length(C1[[i]])) - (1 / C4[[i]])) * var(C1[[i]])
}

weighted.mean.catch <- sum(unlist(C3)) / N
var.of.mean.estimate <- sum(unlist(C6))
print(weighted.mean.catch)
print(var.of.mean.estimate)

X <- split(survey.data, survey.data$stratum)




Y1 <- var(X$`1`$catch) * ((1 - 5 / 80) / 5) * (80 / 874)^2
Y2 <- var(X$`2`$catch) * ((1 - 7 / 96) / 7) * (96 / 874)^2
Y3 <- var(X$`3`$catch) * ((1 - 7 / 104) / 7) * (104 / 874)^2
Y4 <- var(X$`4`$catch) * ((1 - 8 / 105) / 8) * (105 / 874)^2
Y5 <- var(X$`5`$catch) * ((1 - 7 / 84) / 7) * (84 / 874)^2
Y6 <- var(X$`6`$catch) * ((1 - 6 / 81) / 6) * (81 / 874)^2
Y7 <- var(X$`7`$catch) * ((1 - 13 / 180) / 13) * (180 / 874)^2
Y8 <- var(X$`8`$catch) * ((1 - 3 / 36) / 3) * (36 / 874)^2
Y9 <- var(X$`9`$catch) * ((1 - 13 / 108) / 13) * (108 / 874)^2

Y1 + Y2 + Y3 + Y4 + Y5 + Y6 + Y7 + Y8 + Y9

# C7[[i]] <- ((1/length(C1[[i]]))-(1/C4[[i]]))*var(C1[[i]])
# Nh <- sum(tapply(survey.data$Nh,survey.data$stratum,unique))

# Question 4
F1 <- array()
for (i in 1:9) {
  F1[i] <- length(C1[[i]]) / C4[[i]]
}
barplot(F1,
  main = "Sampling fraction", ylab = "Frequency",
  xlab = "Stratum", names.arg = c(1:9)
)
min(F1)
max(F1)

n.2 <- c(8, 7, 7, 8, 7, 6, 13, 3, 10)
Nh.2 <- c(80, 96, 104, 105, 84, 81, 180, 36, 108)
var.2 <- array()
for (i in 1:9) {
  var.2[i] <- (Nh.2[i] / sum(Nh.2))^2 * (1 / n.2[i] - 1 / Nh.2[i]) * var(C1[[i]])
}
sum(var.2)



# Question 5
O1 <- list()
for (i in 1:9) {
  O1[[i]] <- (C4[[i]] * sqrt(var(C1[[i]])))
}
sum.of.Nh.Sh <- sum(unlist(O1))

n <- array()
for (i in 1:9) {
  n[i] <- 69 * ((C4[[i]] * sqrt(var(C1[[i]]))) / sum.of.Nh.Sh)
}

n
n.op <- c(3, 4, 8, 5, 4, 9, 16, 2, 18)
Nh.op <- c(80, 96, 104, 105, 84, 81, 180, 36, 108)

var.op <- array()
for (i in 1:9) {
  var.op[i] <- (Nh.op[i] / sum(Nh.op))^2 * (1 / n.op[i] - 1 / Nh.op[i]) * var(C1[[i]])
}
sum(var.op)

# Question 6
{
  set.seed(1234)
  Nh <- c(15, 10, 7)
  N <- sum(Nh)
  stratum <- rep(1:3, times = Nh)
  y.pop <- rpois(N, stratum * 5)
  pop <- data.frame(y = y.pop, stratum = stratum)
  pop$istrat <- as.numeric(as.factor(pop$stratum))
  nh <- c(5, 3, 2)
  pop
}

pop.split <- split(pop, list(pop$stratum))

{
  nh <- c(5, 3, 2)
  library(gtools)
  possible.id <- list()
  possible.means <- list()
  n.of.possible.uique.means <- array()
  n.of.possible.means <- array()
}

for (i in 1:3) {
  possible.id[[i]] <- combinations(Nh[i], nh[i])
}

possible.samples.1 <- matrix(pop.split$`1`$y[possible.id[[1]]], ncol = 5)
possible.samples.2 <- matrix(pop.split$`2`$y[possible.id[[2]]], ncol = 3)
possible.samples.3 <- matrix(pop.split$`3`$y[possible.id[[3]]], ncol = 2)

all.possible.samples <- list(
  possible.samples.1,
  possible.samples.2, possible.samples.3
)

for (i in 1:3) {
  possible.means[[i]] <- apply(all.possible.samples[[i]], 1, mean)
  n.of.possible.uique.means[i] <- length(levels(factor(possible.means[[i]])))
  n.of.possible.means[i] <- length(possible.means[[i]])
}

prod(n.of.possible.uique.means)
prod(n.of.possible.means)

possible.R <- list()
for (i in 1:3) {
  possible.R[[i]] <- (possible.means[[i]] * Nh[i]) / 32
}

possible.R.df <- expand.grid(possible.R[[1]], possible.R[[2]], possible.R[[3]])
possible.R.s <- (apply(possible.R.df, 1, sum))

estimate.R <- mean(possible.R.s)
true.R <- mean(pop$y)
estimate.R
true.R

true.var <- var(pop.1$`1`$y) * (15 / 32)^2 *
  ((1 - (5 / 15)) / 5) + var(pop.1$`2`$y) * (10 / 32)^2 * ((1 - (3 / 10)) / 3) +
  var(pop.1$`3`$y) * (7 / 32)^2 * ((1 - (2 / 7)) / 2)
estimate.var <- var(possible.R.s)
true.var
estimate.var
pop.split



hist(possible.R.s)
qqnorm(possible.R.s)

# change the maximum value to 100
{
  pop.split.2 <- pop.split
  pop.split.2$`2`$y <- c(13, 8, 7, 6, 12, 14, 100, 7, 7, 10)

  possible.samples.1.2 <- matrix(pop.split.2$`1`$y[possible.id[[1]]], ncol = 5)
  possible.samples.2.2 <- matrix(pop.split.2$`2`$y[possible.id[[2]]], ncol = 3)
  possible.samples.3.2 <- matrix(pop.split.2$`3`$y[possible.id[[3]]], ncol = 2)

  all.possible.samples.2 <- list(
    possible.samples.1.2,
    possible.samples.2.2, possible.samples.3.2
  )

  possible.means.2 <- list()
  for (i in 1:3) {
    possible.means.2[[i]] <- apply(all.possible.samples.2[[i]], 1, mean)
  }

  possible.R.2 <- list()
  for (i in 1:3) {
    possible.R.2[[i]] <- (possible.means.2[[i]] * Nh[i]) / 32
  }

  possible.R.df.2 <- expand.grid(
    possible.R.2[[1]], possible.R.2[[2]],
    possible.R.2[[3]]
  )
  possible.R.s.2 <- (apply(possible.R.df.2, 1, sum))

  estimate.R.2 <- mean(possible.R.s.2)
  estimate.var.2 <- var(possible.R.s.2)
}

estimate.R.2
estimate.var.2

(estimate.R.2 - estimate.R) / estimate.R * 100
(estimate.var.2 - estimate.var) / estimate.var * 100

var(possible.R.s.2)
var(possible.R.s)





# unique cases
possible.R.unique <- list()
for (i in 1:3) {
  possible.R.unique[[i]] <- (as.numeric(levels(factor(possible.means[[i]]))) *
    Nh[i]) / 32
}

possible.R.unique.df <- expand.grid(
  possible.R.unique[[1]], possible.R.unique[[2]],
  possible.R.unique[[3]]
)
possible.R.unique <- apply(possible.R.unique.df, 1, sum)
hist(possibility.R)
mean(possible.R.unique)
var(possible.R.unique)




# no loop code
possibility.means.1 <- apply(possibility.samples.1, 1, mean)
possibility.means.2 <- apply(possibility.samples.2, 1, mean)
possibility.means.3 <- apply(possibility.samples.3, 1, mean)

n.of.possible.means. <- prod(
  length(possibility.means.1),
  length(possibility.means.2),
  length(possibility.means.3)
)
n.of.possible.uique.means. <- prod(
  length(levels(factor(possibility.means.1))),
  length(levels(factor(possibility.means.2))),
  length(levels(factor(possibility.means.3)))
)


n.of.possible.means
n.of.possible.uique.means.
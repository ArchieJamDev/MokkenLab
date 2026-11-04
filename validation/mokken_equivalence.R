# -----------------------------------------------------------------------------
# MokkenLab
# A jamovi module for Mokken scale analysis and evidence-based methodological
# decision support on nonparametric item response theory.
#
# Copyright (C) 2026 Arquímedes De León Chacón Chacón
#
# This file is part of MokkenLab.
#
# MokkenLab is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License,
# or (at your option) any later version.
#
# MokkenLab is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
# See the GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with MokkenLab.
# If not, see https://www.gnu.org/licenses/.
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# Numerical equivalence with mokken.
# ES: Equivalencia numérica con mokken.
#
# Every number MokkenLab reports must equal the one mokken itself returns for
# the same data and options. This script runs both on the bundled datasets
# and writes validation/mokken_equivalence_results.csv (one row per check,
# with the largest absolute difference). It also checks the confidence
# intervals MokkenLab computes itself (mokken::coefH cannot return them
# numerically, see .mk_ci() in R/shared-helpers.R) against the intervals
# mokken prints. Run from the package root after installing MokkenLab:
#   Rscript validation/mokken_equivalence.R
#
# ES: Cada número que informa MokkenLab debe ser igual al que devuelve
# mokken para los mismos datos y opciones. Este script ejecuta ambos sobre
# los conjuntos incluidos y escribe validation/mokken_equivalence_results.csv
# (una fila por verificación, con la mayor diferencia absoluta). También
# contrasta los intervalos que MokkenLab calcula por su cuenta con los que
# imprime mokken.
# -----------------------------------------------------------------------------

suppressMessages(library(mokken))
suppressMessages(library(MokkenLab))
source(file.path("tests", "testthat", "helper-edge-case-data.R"))

quiet <- function(expr) { out <- NULL; utils::capture.output(out <- suppressWarnings(expr)); out }
results <- list()
record <- function(check, ours, theirs, tol = 1e-8) {
    diff <- max(abs(as.numeric(ours) - as.numeric(theirs)), na.rm = TRUE)
    results[[length(results) + 1]] <<- data.frame(check = check, max_abs_diff = signif(diff, 3),
                                                  tolerance = tol, pass = diff <= tol)
}
parse_ci <- function(x) {
    x <- gsub("\\[|\\]", "", trimws(x))
    as.numeric(trimws(strsplit(x, ",")[[1]]))
}

# DS14, complete cases, Si1 and Si3 reversed (as mokken's own examples).
# ES: DS14, casos completos, Si1 y Si3 invertidos.
ds14 <- utils::read.csv(file.path("data", "mokkenlab_ds14.csv"))
items <- names(ds14)[3:16]
X <- as.matrix(stats::na.omit(ds14[, items]))
X[, c("Si1", "Si3")] <- 4 - X[, c("Si1", "Si3")]

a <- runAnalysis("msa", ds14, vars = items, negItems = c("Si1", "Si3"), typeZ = "WB", zLowerbound = 0.3)
h <- quiet(mokken::coefH(X, nice.output = FALSE, results = FALSE))
record("coefH: scale H", a$results$scale$asDF$est, h$H)
record("coefH: scale SE", a$results$scale$asDF$se, h$se.H)
record("coefH: item Hi", a$results$items$asDF$est, h$Hi)
record("coefH: item SE", a$results$items$asDF$se, h$se.Hi)
z <- quiet(mokken::coefZ(X, lowerbound = 0.3, type.z = "WB"))
record("coefZ (WB, .30): scale z", a$results$scale$asDF$z, z$Z)
record("coefZ (WB, .30): item z", a$results$items$asDF$z, z$Zi)

for (type in c("WB", "RP")) {
    b <- runAnalysis("msa", ds14, vars = items, negItems = c("Si1", "Si3"), typeCI = type)
    nice <- quiet(mokken::coefH(X, ci = 0.95, type.ci = type, results = FALSE))
    printed <- parse_ci(nice$H[1, 3])
    # mokken prints three decimals, so agreement is checked to 5e-4.
    # ES: mokken imprime tres decimales, así que el acuerdo se verifica a
    # 5e-4.
    record(paste0("CI (", type, ") of H vs mokken's printed interval"),
           c(b$results$scale$asDF$lower, b$results$scale$asDF$upper), printed, tol = 5e-4)
    printed_i <- t(sapply(nice$Hi[, 3], parse_ci))
    record(paste0("CI (", type, ") of Hi vs mokken's printed intervals"),
           c(b$results$items$asDF$lower, b$results$items$asDF$upper), c(printed_i[, 1], printed_i[, 2]), tol = 5e-4)
}

rel <- quiet(mokken::check.reliability(X, LCRC = FALSE))
r <- runAnalysis("msa", ds14, vars = items, negItems = c("Si1", "Si3"))
record("check.reliability: MS, alpha, lambda-2", r$results$reliability$asDF$value, c(rel$MS, rel$alpha, rel$lambda.2))
norms <- quiet(mokken::check.norms(rowSums(X), nice.output = FALSE))
n <- runAnalysis("msa", ds14, vars = items, negItems = c("Si1", "Si3"), showNorms = TRUE)
record("check.norms: mean and SD", n$results$normsSummary$asDF$est, c(norms$mean[1], norms$sd[1]))
record("check.norms: percentile ranks", n$results$normsPerc$asDF$est, norms$perc[, 1])

ai <- runAnalysis("aisp", ds14, vars = items, negItems = c("Si1", "Si3"), lowerbound = 0.4)
part <- quiet(mokken::aisp(X, lowerbound = 0.4))
ours <- as.integer(sub("Scale ", "", ai$results$membership$asDF$scale))
ours[is.na(ours)] <- 0L
record("aisp (c = .40): partition", ours, part[, 1], tol = 0)

mono <- quiet(summary(mokken::check.monotonicity(X)))
m <- runAnalysis("msacheck", ds14, vars = items, negItems = c("Si1", "Si3"), checkIIO = TRUE)
record("check.monotonicity: crit", m$results$mono$asDF$crit, mono[, "crit"], tol = 0)
iio <- quiet(summary(mokken::check.iio(X)))
record("check.iio (MIIO): HT", m$results$iioHT$asDF$value, iio$HT)

trog <- utils::read.csv(file.path("data", "mokkenlab_trog.csv"))
ct <- runAnalysis("msacheck", trog, vars = names(trog), checkMono = FALSE, checkIIO = TRUE,
                  iioItemSelection = FALSE, iioOrder = "clusters", iioClusterSize = 4L)
ico <- quiet(summary(mokken::check.iio(as.matrix(trog), item.selection = FALSE, fixed.item.order = rep(1:20, each = 4))))
record("check.iio clustered (TROG): HT, HTB, HTB/HT", ct$results$iioHT$asDF$value, ico$HT[1, ])

au <- utils::read.csv(file.path("data", "mokkenlab_autonomy.csv"))
ml <- runAnalysis("mlmsa", au, subject = "Teacher", vars = names(au)[-1], ci = FALSE)
AS <- as.matrix(cbind(as.integer(factor(au$Teacher)), au[, -1]))
mh <- quiet(mokken::MLcoefH(AS, nice.output = FALSE))
record("MLcoefH: HW, HB, BW", ml$results$scale$asDF$est, unlist(mh$H[1, c("HW", "HB", "HBW")]))
record("MLcoefH: HWi", ml$results$items$asDF$w, mh$Hi$HWi)
icc <- quiet(mokken::ICC(AS))
record("ICC: scale", ml$results$icc$asDF$icc[nrow(ml$results$icc$asDF)], icc$scaleICC$ICC)

out <- do.call(rbind, results)
utils::write.csv(out, file.path("validation", "mokken_equivalence_results.csv"), row.names = FALSE)
print(out, row.names = FALSE)
cat(sprintf("\n%d of %d checks pass.\n", sum(out$pass), nrow(out)))

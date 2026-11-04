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
# Stress tests: small samples, dichotomous items, missing values and the
# bundled datasets end to end. Each run must finish or stop with a MokkenLab
# message, never a raw R error.
# ES: Pruebas de estrés: muestras pequeñas, ítems dicotómicos, valores
# perdidos y los conjuntos incluidos de punta a punta. Cada ejecución debe
# terminar o detenerse con un mensaje de MokkenLab, nunca un error crudo de R.
# -----------------------------------------------------------------------------

finishesOrExplains <- function(expr) {
    result <- tryCatch({ force(expr); "ok" }, error = function(e) conditionMessage(e))
    expect_true(identical(result, "ok") || grepl("MokkenLab|Select|items|cases|subject|score", result), info = result)
}

test_that("small samples finish or explain", {
    for (n in c(40, 15, 6)) {
        data <- edgeScaleData(n = n)
        finishesOrExplains(runAnalysis("msa", data, vars = names(data), showReliability = TRUE, relLCRC = TRUE))
        finishesOrExplains(runAnalysis("aisp", data, vars = names(data)))
        finishesOrExplains(runAnalysis("msacheck", data, vars = names(data), checkIIO = TRUE, checkCA = TRUE))
    }
})

test_that("bundled datasets run end to end", {
    dir <- system.file("extdata", package = "MokkenLab")
    if (!nzchar(dir) || !file.exists(file.path(dir, "mokkenlab_ds14.csv")))
        dir <- file.path(testthat::test_path("..", ".."), "data")
    skip_if_not(file.exists(file.path(dir, "mokkenlab_ds14.csv")))
    ds14 <- utils::read.csv(file.path(dir, "mokkenlab_ds14.csv"))
    items <- names(ds14)[3:16]
    a <- runAnalysis("msa", ds14, vars = items, negItems = c("Si1", "Si3"), naHandling = "twoway", groupVar = "Male")
    expect_true(a$results$scale$isFilled())
    b <- runAnalysis("aisp", ds14, vars = items, negItems = c("Si1", "Si3"), lowerbound = 0.4)
    expect_equal(nrow(b$results$scales$asDF), 2)
    tr <- utils::read.csv(file.path(dir, "mokkenlab_transreas.csv"))
    expect_true(runAnalysis("msa", tr, vars = names(tr)[-1], showBounds = TRUE)$results$bounds$isFilled())
    au <- utils::read.csv(file.path(dir, "mokkenlab_autonomy.csv"))
    expect_equal(nrow(runAnalysis("mlmsa", au, subject = "Teacher", vars = names(au)[-1])$results$scale$asDF), 3)
    trog <- utils::read.csv(file.path(dir, "mokkenlab_trog.csv"))
    c3 <- runAnalysis("msacheck", trog, vars = names(trog)[1:24], checkMono = FALSE, checkIIO = TRUE,
                      iioItemSelection = FALSE, iioOrder = "clusters", iioClusterSize = 4L)
    expect_equal(nrow(c3$results$iioClusters$asDF), 6)
})

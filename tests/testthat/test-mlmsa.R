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
# mlmsa edge-case tests.
# ES: Pruebas de casos límite de mlmsa.
# -----------------------------------------------------------------------------

test_that("mlmsa fills the two-level tables", {
    data <- edgeTwoLevelData()
    a <- runAnalysis("mlmsa", data, subject = "Subject", vars = paste0("r", 1:5), showHij = TRUE,
                     showCov = TRUE, showWeights = TRUE, typeCI = "RP", typeZ = "RP")
    expect_equal(nrow(a$results$scale$asDF), 3)
    expect_equal(nrow(a$results$items$asDF), 5)
    expect_equal(nrow(a$results$pairs$asDF), 10)
    expect_equal(nrow(a$results$covH$asDF), 3)
    expect_equal(nrow(a$results$icc$asDF), 6)
    expect_equal(nrow(a$results$weights$asDF), 10)
})

test_that("mlmsa waits for a subject and rejects a single subject", {
    data <- edgeTwoLevelData()
    a <- runAnalysis("mlmsa", data, vars = paste0("r", 1:5))
    expect_equal(nrow(a$results$scale$asDF), 0)
    data$one <- 1
    expect_error(runAnalysis("mlmsa", data, subject = "one", vars = paste0("r", 1:5)), "fewer than two subjects")
})

test_that("mlmsa reports in Spanish without missing texts", {
    data <- edgeTwoLevelData()
    a <- runAnalysisIn("es", "mlmsa", data, subject = "Subject", vars = paste0("r", 1:5), weighProps = FALSE)
    expect_false(grepl("[missing text", a$results$interpretation$content, fixed = TRUE))
})

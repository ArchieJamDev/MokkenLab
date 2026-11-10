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
# aisp edge-case tests.
# ES: Pruebas de casos límite de aisp.
# -----------------------------------------------------------------------------

test_that("aisp keeps a clean scale together", {
    data <- edgeScaleData()
    a <- runAnalysis("aisp", data, vars = names(data))
    expect_equal(length(unique(a$results$membership$asDF$scale)), 1)
    expect_equal(nrow(a$results$scales$asDF), 1)
})

test_that("aisp separates a two-dimensional structure", {
    data <- edgeTwoDimData()
    a <- runAnalysis("aisp", data, vars = names(data), lowerbound = 0.3)
    expect_gte(nrow(a$results$scales$asDF), 2)
})

test_that("aisp runs a sequence of lower bounds", {
    data <- edgeTwoDimData()
    a <- runAnalysis("aisp", data, vars = names(data), lbMode = "sequence", lbFrom = 0.1, lbTo = 0.5, lbStep = 0.1)
    expect_equal(nrow(a$results$sequenceSummary$asDF), 5)
    expect_error(runAnalysis("aisp", data, vars = names(data), lbMode = "sequence", lbFrom = 0.5, lbTo = 0.1),
                 "From must be smaller")
})

test_that("aisp genetic algorithm is reproducible with the same seed", {
    data <- edgeTwoDimData()
    a <- runAnalysis("aisp", data, vars = names(data), search = "ga", maxgensMode = "manual", maxgens = 200L)
    b <- runAnalysis("aisp", data, vars = names(data), search = "ga", maxgensMode = "manual", maxgens = 200L)
    expect_identical(a$results$membership$asDF$scale, b$results$membership$asDF$scale)
})

test_that("aisp accepts a start set, Hi tests and nested data", {
    data <- edgeTwoLevelData()
    a <- runAnalysisIn("es", "aisp", data, vars = paste0("r", 1:5), clusterVar = "Subject", startSet = c("r1", "r2"),
                       testHi = TRUE)
    expect_true(a$results$membership$isFilled())
    expect_false(grepl("[missing text", a$results$interpretation$content, fixed = TRUE))
})

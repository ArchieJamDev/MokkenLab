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
# msacheck edge-case tests.
# ES: Pruebas de casos límite de msacheck.
# -----------------------------------------------------------------------------

test_that("msacheck runs every check on a clean scale", {
    data <- edgeScaleData()
    a <- runAnalysis("msacheck", data, vars = names(data), checkIIO = TRUE, checkRest = TRUE,
                     checkPmatrix = TRUE, checkCA = TRUE, caWindex = TRUE)
    for (t in c("mono", "iio", "rest", "pmat", "ca", "caW"))
        expect_equal(nrow(a$results[[t]]$asDF), ncol(data), info = t)
    expect_true(a$results$iioHT$isFilled())
    expect_false(grepl("[missing text", a$results$interpretation$content, fixed = TRUE))
})

test_that("msacheck supports every IIO method and fixed orders", {
    data <- edgeScaleData()
    for (m in c("MIIO", "MSCPM", "IT")) {
        a <- runAnalysis("msacheck", data, vars = names(data), checkMono = FALSE, checkIIO = TRUE, iioMethod = m)
        expect_equal(nrow(a$results$iio$asDF), ncol(data), info = m)
    }
    b <- runAnalysis("msacheck", data, vars = names(data), checkMono = FALSE, checkIIO = TRUE,
                     iioItemSelection = FALSE, iioOrder = "clusters", iioClusterSize = 2L)
    expect_equal(nrow(b$results$iioClusters$asDF), 4)
    expect_equal(nrow(b$results$iioHT$asDF), 3)
    c3 <- runAnalysis("msacheck", data, vars = names(data), checkMono = FALSE, checkIIO = TRUE,
                      iioOrder = "custom", iioOrderText = "1, 2, 3")
    expect_equal(c3$results$iio$asDF$item[1], "Not computable")
})

test_that("msacheck reports two-level fit statistics with a cluster variable", {
    data <- edgeTwoLevelData()
    a <- runAnalysis("msacheck", data, vars = paste0("r", 1:5), clusterVar = "Subject", checkIIO = TRUE,
                     iioItemSelection = FALSE)
    expect_equal(nrow(a$results$mono$asDF), 10)
    expect_true("level" %in% names(a$results$mono$asDF))
})

test_that("msacheck rejects fewer than three items", {
    data <- edgeScaleData()
    expect_error(runAnalysis("msacheck", data, vars = c("item1", "item2")), "at least three items")
})

test_that("msacheck honors a fixed minimum group size", {
    data <- edgeScaleData()
    a <- runAnalysis("msacheck", data, vars = names(data), minsizeMode = "manual", minsize = 30L)
    expect_equal(a$results$summary$asDF$value[3], "30")
})

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
# Regression tests for the independent audit of 2026-11-04.
# ES: Pruebas de regresión de la auditoría independiente del 2026-11-04.
#
# Each test pins one confirmed finding, so it cannot come back: conditional
# association with W indices off, item names with spaces in two-level pairs,
# "not evaluable" never read as "no violations", non-computable sections in
# the key findings, the range-preserving interval at H = 1, the single-bound
# AISP plot, and the always-visible reversal row.
# ES: Cada prueba fija un hallazgo confirmado, para que no vuelva.
# -----------------------------------------------------------------------------

test_that("conditional association works with W indices on and off", {
    data <- edgeScaleData()
    for (w in c(FALSE, TRUE)) {
        a <- runAnalysis("msacheck", data, vars = names(data), checkMono = FALSE, checkCA = TRUE, caWindex = w)
        expect_equal(nrow(a$results$ca$asDF), ncol(data), info = w)
        expect_false("Not computable" %in% a$results$ca$asDF$item, info = w)
    }
})

test_that("two-level pairs keep item names that contain spaces", {
    data <- edgeTwoLevelData()
    names(data)[2:3] <- c("item one", "item two")
    a <- runAnalysis("mlmsa", data, subject = "Subject", vars = names(data)[-1], showHij = TRUE)
    pairs <- a$results$pairs$asDF
    expect_equal(nrow(pairs), choose(5, 2))
    expect_equal(pairs$item1[1], "item one")
    expect_equal(pairs$item2[1], "item two")
})

test_that("a missing violation count is never reported as no violations", {
    data <- edgeScaleData()
    a <- runAnalysis("msacheck", data, vars = names(data))
    verdict <- a$.__enclos_env__$private$.verdict
    lang <- MokkenLab:::.mk_lang(a$options)
    expect_equal(verdict(lang, NA_real_, NA_real_), "Not evaluable")
    expect_equal(verdict(lang, 3, NA_real_), "Not evaluable")
    expect_equal(verdict(lang, 0, 0), "No violations")
})

test_that("checks not run and sections not computed reach the key findings", {
    data <- edgeScaleData()
    a <- runAnalysis("msacheck", data, vars = names(data))
    expect_match(a$results$keySummary$content, "Not checked in this run")
    b <- runAnalysis("msa", data, vars = names(data), showBounds = TRUE)
    expect_match(b$results$keySummary$content, "could not be computed")
})

test_that("the range-preserving interval is defined at the boundary", {
    ci <- MokkenLab:::.mk_ci(c(1, 1, 0.5), c(0, 0.02, 0), 0.95, "RP")
    expect_true(all(is.finite(c(ci$lower, ci$upper))))
    expect_equal(ci$upper, c(1, 1, 0.5))
})

test_that("a single lower bound draws the scale membership plot", {
    data <- edgeTwoDimData()
    a <- runAnalysis("aisp", data, vars = names(data))
    expect_false(is.null(a$results$scalePlotImg$state))
    expect_true(renders(a, "scalePlotImg", ".plotScales"))
})

test_that("the reversal row is always shown, also when nothing is reversed", {
    data <- edgeScaleData()
    for (name in c("msa", "aisp", "msacheck")) {
        a <- runAnalysis(name, data, vars = names(data))
        expect_true(any(grepl("^None", a$results$summary$asDF$value)), info = name)
    }
})

test_that("several completed data sets are labeled as not pooled", {
    data <- edgeMissingData()
    a <- runAnalysis("msa", data, vars = names(data), naHandling = "twoway", twowaySets = 2L)
    expect_match(a$results$summary$asDF$value[2], "not pooled")
})

# -----------------------------------------------------------------------------
# Regression tests for the jamovi review of 1.0.0.
# ES: Pruebas de regresión de la revisión de jamovi de la versión 1.0.0.
# -----------------------------------------------------------------------------

test_that("a group too small to analyse marks both group tables", {
    data <- edgeScaleData()
    data$grp <- factor(c("solo", rep(c("a", "b"), length.out = nrow(data) - 1)))
    a <- runAnalysis("msa", data, vars = setdiff(names(data), "grp"), groupVar = "grp")
    expect_equal(a$results$groups$asDF$group[1], "Not computable")
    expect_equal(a$results$groupItems$asDF$item[1], "Not computable")
})

test_that("cached mokken results keep only what the report reads", {
    data <- edgeScaleData()
    a <- runAnalysis("msa", data, vars = names(data), showZ = TRUE, showReliability = TRUE)
    state <- a$results$cacheStore$state
    expect_true(length(state) > 0)
    expect_lt(length(serialize(state, NULL)), 2 * 1024^2)
    coefh <- Filter(function(e) is.list(e$value) && "Hij" %in% names(e$value), state)
    expect_length(coefh, 1)
    expect_setequal(names(coefh[[1]]$value), c("H", "se.H", "Hi", "se.Hi", "Hij"))
})

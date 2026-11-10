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
# msa edge-case tests.
# ES: Pruebas de casos límite de msa.
#
# Category A conditions must reject with an explanation; a valid scale must
# fill every table; every optional section must fill or explain itself. See
# CODE_STYLE.md §19.1.
# ES: Las condiciones de Categoría A deben rechazar con una explicación; una
# escala válida debe llenar cada tabla; cada sección opcional debe llenarse
# o explicarse.
# -----------------------------------------------------------------------------

test_that("msa fills the core tables for a clean scale", {
    data <- edgeScaleData()
    a <- runAnalysis("msa", data, vars = names(data))
    expect_true(a$results$summary$isFilled())
    expect_equal(nrow(a$results$items$asDF), ncol(data))
    h <- a$results$scale$asDF$est
    expect_true(is.finite(h) && h > 0.3)
    expect_true(nzchar(a$results$interpretation$content))
})

test_that("msa fills every optional section", {
    data <- edgeScaleData()
    data$group <- factor(rep(c("A", "B"), length.out = nrow(data)))
    a <- runAnalysis("msa", data, vars = paste0("item", 1:8), groupVar = "group", showHij = TRUE,
                     showErrors = TRUE, errorsOplus = TRUE, showNorms = TRUE, relLCRC = TRUE, relIRC = TRUE,
                     showChoicesGuide = TRUE, typeCI = "RP", typeZ = "RP")
    expect_equal(nrow(a$results$hij$asDF), 8)
    expect_equal(nrow(a$results$groups$asDF), 2)
    expect_equal(nrow(a$results$reliability$asDF), 4)
    expect_equal(nrow(a$results$errors$asDF), 2)
    expect_true(a$results$normsZ$isFilled())
    expect_true(nzchar(a$results$choicesGuide$content))
})

test_that("msa reports in jamovi's language, and follows a change of language", {
    data <- edgeScaleData()
    a <- runAnalysisIn("es", "msa", data, vars = names(data))
    expect_true("Casos usados" %in% a$results$summary$asDF$element)
    expect_false(grepl("[missing text", a$results$interpretation$content, fixed = TRUE))
    # The language is read at every run, never frozen at the first one
    # (ARCHITECTURE.md, "Report language").
    # ES: El idioma se lee en cada ejecución, nunca queda fijo en la primera.
    b <- runAnalysisIn("en", "msa", data, vars = names(data))
    expect_true("Cases used" %in% b$results$summary$asDF$element)
    c <- runAnalysisIn("es", "msa", data, vars = names(data))
    expect_true("Casos usados" %in% c$results$summary$asDF$element)
})

test_that("msa tolerates accented and symbol item names", {
    data <- edgeSpecialNameData()
    a <- runAnalysis("msa", data, vars = names(data), showHij = TRUE)
    expect_true("café ± más" %in% a$results$items$asDF$item)
})

test_that("msa rejects too few items, constant, empty and non-integer items", {
    data <- edgeScaleData()
    expect_error(runAnalysis("msa", data, vars = "item1"), "at least two items")
    expect_error(runAnalysis("msa", edgeConstantItemData(), vars = paste0("item", 1:8)), "zero variance")
    expect_error(runAnalysis("msa", edgeEmptyItemData(), vars = paste0("item", 1:8)), "fewer than two observed")
    expect_error(runAnalysis("msa", edgeNonIntegerData(), vars = paste0("item", 1:8)), "non-integer")
})

test_that("msa rejects a declared range that excludes observed scores", {
    data <- edgeScaleData()
    expect_error(runAnalysis("msa", data, vars = names(data), scoreRange = "manual", scoreMin = 1L, scoreMax = 4L),
                 "declared score range")
})

test_that("msa handles missing values listwise and by two-way imputation", {
    data <- edgeMissingData()
    a <- runAnalysis("msa", data, vars = names(data))
    expect_lt(nrow(data) - sum(stats::complete.cases(data)) + 1, nrow(data))
    b <- runAnalysis("msa", data, vars = names(data), naHandling = "twoway", twowaySets = 3L)
    expect_equal(nrow(b$results$completedSets$asDF), 3)
    expect_match(b$results$summary$asDF$value[2], "two-way")
})

test_that("msa explains bounds for polytomous items instead of failing", {
    data <- edgeScaleData()
    a <- runAnalysis("msa", data, vars = names(data), showBounds = TRUE)
    expect_equal(a$results$bounds$asDF$item1[1], "Not computable")
})

test_that("msa computes bounds for dichotomous items", {
    data <- as.data.frame(lapply(edgeScaleData(), function(x) as.integer(x >= 2)))
    a <- runAnalysis("msa", data, vars = names(data), showBounds = TRUE, boundsUpper = TRUE)
    expect_equal(nrow(a$results$bounds$asDF), choose(ncol(data), 2))
})

test_that("msa reverses negatively worded items", {
    data <- edgeScaleData()
    data$item3 <- 4 - data$item3
    a <- runAnalysis("msa", data, vars = names(data))
    b <- runAnalysis("msa", data, vars = names(data), negItems = "item3")
    expect_gt(b$results$scale$asDF$est, a$results$scale$asDF$est)
})

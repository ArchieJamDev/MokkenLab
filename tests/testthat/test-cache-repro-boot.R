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
# Tests of the statistical cache, the reproducibility record and the
# bootstrap stability of the AISP partition.
# ES: Pruebas de la caché estadística, del registro de reproducibilidad y de
# la estabilidad bootstrap de la partición de AISP.
# -----------------------------------------------------------------------------

# jamovi's cycle on an option change: a new instance, with the state of the
# elements not cleared (here the cache store) restored.
# ES: El ciclo de jamovi ante un cambio de opción: una instancia nueva, con
# el estado de los elementos no borrados (aquí el almacén) restaurado.
rerunWithCache <- function(name, previous, data, ...) {
    cls <- get(paste0(name, "Class"), envir = asNamespace("MokkenLab"))
    opts <- get(paste0(name, "Options"), envir = asNamespace("MokkenLab"))
    analysis <- cls$new(options = opts$new(...), data = data)
    analysis$init()
    analysis$results$cacheStore$setState(previous$results$cacheStore$state)
    analysis$run()
    analysis
}

test_that("a cosmetic change reuses the cached mokken results", {
    data <- edgeScaleData()
    calls <- 0L
    counter <- environment()
    trace("coefH", where = asNamespace("mokken"), print = FALSE,
          tracer = bquote(assign("calls", get("calls", envir = .(counter)) + 1L, envir = .(counter))))
    on.exit(untrace("coefH", where = asNamespace("mokken")), add = TRUE)
    a <- runAnalysis("msa", data, vars = names(data))
    first <- calls
    b <- rerunWithCache("msa", a, data, vars = names(data), plotPointSize = 4, reportLang = "es")
    expect_equal(calls, first)
    expect_equal(a$results$scale$asDF$est, b$results$scale$asDF$est)
    d <- rerunWithCache("msa", b, data, vars = names(data), negItems = "item1")
    expect_gt(calls, first)
})

test_that("every analysis writes a reproducibility record with valid R code", {
    data <- edgeScaleData()
    two <- edgeTwoLevelData()
    runs <- list(runAnalysis("msa", data, vars = names(data), showErrors = TRUE, showNorms = TRUE),
                 runAnalysis("aisp", data, vars = names(data), search = "ga", maxgensMode = "manual", maxgens = 50L),
                 runAnalysis("msacheck", data, vars = names(data), checkIIO = TRUE, checkCA = TRUE,
                             iioOrder = "clusters", iioClusterSize = 2L),
                 runAnalysis("mlmsa", two, subject = "Subject", vars = paste0("r", 1:5), showWeights = TRUE))
    for (a in runs) {
        html <- a$results$reproducibility$content
        expect_match(html, "library(mokken)", fixed = TRUE)
        code <- sub("</pre>.*", "", sub(".*<pre[^>]*>", "", html))
        code <- gsub("&quot;", "\"", gsub("&lt;", "<", gsub("&gt;", ">", gsub("&amp;", "&", code))))
        expect_silent(parse(text = code))
    }
})

test_that("the bootstrap stability of the partition is bounded and reproducible", {
    data <- edgeTwoDimData()
    a <- runAnalysis("aisp", data, vars = names(data), bootStability = TRUE, bootReps = 20L)
    b <- runAnalysis("aisp", data, vars = names(data), bootStability = TRUE, bootReps = 20L)
    boot <- a$results$bootTable$asDF
    expect_equal(nrow(boot), ncol(data))
    values <- unlist(boot[, c("together", "same", "unscalable")])
    expect_true(all(values[!is.na(values)] >= 0 & values[!is.na(values)] <= 1))
    expect_identical(boot, b$results$bootTable$asDF)
    expect_true(renders(a, "bootPlotImg", ".plotBoot"))
})

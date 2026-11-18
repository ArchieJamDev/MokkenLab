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

# -----------------------------------------------------------------------------
# Regression tests for the jamovi audit of 2026-11-13.
# ES: Pruebas de regresión de la auditoría de jamovi del 2026-11-13.
# -----------------------------------------------------------------------------

# The condition private$.checkpoint() raises when jamovi restarts the analysis.
# ES: La condición que lanza private$.checkpoint() cuando jamovi reinicia el
# análisis.
restartSignal <- function() structure(class = c("error", "condition"),
                                      list(message = "restart", call = NULL, code = "restart"))

test_that("the bootstrap calls the checkpoint once per resample", {
    data <- edgeTwoDimData()
    X <- as.matrix(data)
    original <- as.integer(suppressWarnings(mokken::aisp(X, verbose = FALSE))[, 1])
    calls <- 0L
    boot <- MokkenLab:::.mk_aisp_bootstrap(X, list(lowerbound = 0.3), reps = 5L, seed = 1L, level2 = NULL,
                                           original = original, checkpoint = function() calls <<- calls + 1L)
    expect_equal(calls, 5L)
    expect_equal(boot$done + boot$failed, 5L)
})

test_that("jamovi's restart signal stops the bootstrap and is never cached", {
    data <- edgeTwoDimData()
    X <- as.matrix(data)
    original <- as.integer(suppressWarnings(mokken::aisp(X, verbose = FALSE))[, 1])
    store <- new.env()
    store$state <- NULL
    store$setState <- function(value) store$state <- value
    calls <- 0L
    checkpoint <- function() {
        calls <<- calls + 1L
        if (calls == 3L) stop(restartSignal())
    }
    err <- tryCatch(MokkenLab:::.mk_cached_capture(store, "aisp_bootstrap",
        list(X = X, args = list(lowerbound = 0.3), reps = 10L, seed = 1L, level2 = NULL, original = original),
        function(...) MokkenLab:::.mk_aisp_bootstrap(..., checkpoint = checkpoint)),
        error = function(e) e)
    expect_identical(err$code, "restart")
    expect_equal(calls, 3L)
    expect_null(store$state)
})

test_that(".mk_capture() lets the restart signal through and keeps other errors", {
    err <- tryCatch(MokkenLab:::.mk_capture(stop(restartSignal())), error = function(e) e)
    expect_identical(err$code, "restart")
    res <- MokkenLab:::.mk_capture(stop("ordinary failure"))
    expect_null(res$value)
    expect_equal(res$error, "ordinary failure")
})

test_that("Model Assumptions lays out its item rows in .init()", {
    data <- edgeScaleData()
    cls <- MokkenLab:::msacheckClass
    a <- cls$new(options = MokkenLab:::msacheckOptions$new(vars = names(data), checkIIO = TRUE,
                                                          checkRest = TRUE, checkPmatrix = TRUE,
                                                          checkCA = TRUE, caWindex = TRUE),
                 data = data)
    a$init()
    keys <- function(table) as.character(unlist(table$rowKeys))
    for (name in c("mono", "iio", "rest", "pmat", "caW"))
        expect_equal(keys(a$results[[name]]), names(data), info = name)
    a$run()
    for (name in c("mono", "iio", "rest", "pmat", "caW")) {
        expect_equal(keys(a$results[[name]]), names(data), info = name)
        expect_equal(a$results[[name]]$asDF$item, names(data), info = name)
    }
})

test_that("with a cluster variable the two-level rows exist before the results", {
    data <- edgeTwoLevelData()
    items <- paste0("r", 1:5)
    a <- MokkenLab:::msacheckClass$new(options = MokkenLab:::msacheckOptions$new(
        vars = items, clusterVar = "Subject", checkIIO = TRUE, iioItemSelection = FALSE), data = data)
    a$init()
    expected <- c(paste0("L1_", items), paste0("L2_", items))
    for (name in c("mono", "iio"))
        expect_equal(as.character(unlist(a$results[[name]]$rowKeys)), expected, info = name)
    a$run()
    for (name in c("mono", "iio")) {
        expect_equal(as.character(unlist(a$results[[name]]$rowKeys)), expected, info = name)
        expect_equal(a$results[[name]]$asDF$level, rep(c("Within", "Between"), each = 5), info = name)
    }
})

test_that("a check that cannot run replaces the item rows with one row", {
    data <- edgeScaleData()
    a <- runAnalysis("msacheck", data, vars = names(data), checkMono = FALSE, checkIIO = TRUE,
                     iioOrder = "custom", iioOrderText = "1 2")
    expect_equal(a$results$iio$asDF$item, "Not computable")
})

test_that("the W indices table stays visible and says why it is empty", {
    data <- edgeScaleData()
    # The note set on a table (jmvcore keeps them in a private field).
    # ES: La nota de una tabla (jmvcore la guarda en un campo privado).
    note_of <- function(table) {
        notes <- tryCatch(table$.__enclos_env__$private$.notes, error = function(e) NULL)
        paste(unlist(lapply(notes, function(n) if (is.list(n)) n$note else n)), collapse = " ")
    }
    run_with <- function(fake) {
        testthat::local_mocked_bindings(check.ca = fake, .package = "mokken")
        runAnalysis("msacheck", data, vars = names(data), checkMono = FALSE, checkCA = TRUE, caWindex = TRUE)
    }
    # check.ca() fails: the CA table gets its single row, the W table keeps
    # its item rows and a note.
    # ES: check.ca() falla: la tabla CA recibe su fila única y la tabla W
    # conserva sus filas de ítems y una nota.
    a <- run_with(function(...) stop("fake failure"))
    expect_equal(a$results$ca$asDF$item, "Not computable")
    expect_true(a$results$caW$visible)
    expect_equal(a$results$caW$asDF$item, names(data))
    expect_match(note_of(a$results$caW), "could not be computed because conditional association", fixed = TRUE)
    # check.ca() runs but returns no W indices.
    # ES: check.ca() corre pero no devuelve índices W.
    b <- run_with(function(X, ...) list(InScale = list(rep(TRUE, ncol(X)))))
    expect_false("Not computable" %in% b$results$ca$asDF$item)
    expect_true(b$results$caW$visible)
    expect_equal(b$results$caW$asDF$item, names(data))
    expect_match(note_of(b$results$caW), "did not return W indices", fixed = TRUE)
    # No result path hides the table any more.
    # ES: Ningún camino de resultados oculta ya la tabla.
    source_file <- testthat::test_path("..", "..", "R", "msacheck.b.R")
    if (file.exists(source_file))
        expect_false(any(grepl("caW$setVisible(FALSE)", readLines(source_file, warn = FALSE), fixed = TRUE)))
})

test_that("plot checkboxes are greyed out until their analysis is on", {
    root <- testthat::test_path("..", "..", "jamovi")
    skip_if_not(dir.exists(root))
    # The enable: line that follows `name: <option>` in a .u.yaml file
    # (read as text: yaml is not a dependency of the module).
    # ES: La línea enable: que sigue a `name: <opción>` en un .u.yaml.
    enable_of <- function(file, name) {
        lines <- trimws(readLines(file.path(root, file), warn = FALSE))
        at <- match(paste("name:", name), lines)
        sub("^enable:[[:space:]]*", "", lines[at + 1])
    }
    expect_equal(enable_of("msacheck.u.yaml", "monoPlot"), "(checkMono)")
    expect_equal(enable_of("msacheck.u.yaml", "iioPlot"), "(checkIIO)")
    expect_equal(enable_of("msacheck.u.yaml", "restPlot"), "(checkRest)")
    expect_equal(enable_of("msacheck.u.yaml", "pmatPlot"), "(checkPmatrix)")
    expect_equal(enable_of("msa.u.yaml", "groupPlot"), "(groupVar)")
    expect_equal(enable_of("msa.u.yaml", "normsPlot"), "(showNorms)")
})

test_that("headings and appended clauses are translated as whole sentences", {
    data <- edgeMissingData()
    a <- runAnalysisIn("es", "msa", data, vars = names(data), naHandling = "twoway", twowaySets = 2L,
                       showChoicesGuide = TRUE)
    expect_match(a$results$choicesGuide$content, "Valores perdidos: Imputación two-way.", fixed = TRUE)
    expect_match(a$results$summary$asDF$value[2], "sin combinar)", fixed = TRUE)
    b <- runAnalysisIn("es", "aisp", edgeScaleData(), vars = names(edgeScaleData()), testHi = TRUE)
    expect_true(any(grepl("Hi contrastado contra c", b$results$summary$asDF$value, fixed = TRUE)))
    # No catalogue entry is a fragment that starts with a separator.
    # ES: Ninguna entrada del catálogo es un fragmento que empieza con un
    # separador.
    for (s in unlist(MokkenLab:::.mk_text_table(identity), use.names = FALSE))
        expect_false(grepl("^[;,:] ", s), info = s)
})

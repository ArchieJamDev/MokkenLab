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
# Plot tests.
# ES: Pruebas de gráficos.
#
# Every image must receive a state in .run() (CODE_STYLE: plots render from
# image$state, so they survive export) and draw under jamovi's ggtheme with
# every plot setting.
# ES: Cada imagen debe recibir un estado en .run() y dibujarse con el
# ggtheme de jamovi con cada ajuste de gráfico.
# -----------------------------------------------------------------------------

settings <- list(
    list(),
    list(plotValueLabels = TRUE, plotLegend = "right", plotAxisAngle = "a90"),
    list(plotYRange = "data", plotNcol = 1L, plotWidth = 800L, plotHeight = 600L,
         plotPointSize = 0, plotLineWidth = 2)
)

test_that("msa plots draw with every setting", {
    data <- edgeScaleData()
    data$group <- factor(rep(c("A", "B"), length.out = nrow(data)))
    for (s in settings) {
        a <- do.call(runAnalysis, c(list("msa", data, vars = paste0("item", 1:8), groupVar = "group", hijPlot = TRUE,
                                         showErrors = TRUE, errorsOplus = TRUE, errorsPlot = TRUE, showNorms = TRUE,
                                         normsPlot = TRUE), s))
        for (p in list(c("hiPlot", ".plotHi"), c("hijPlotImg", ".plotHij"), c("groupPlotImg", ".plotGroups"),
                       c("errorsPlot", ".plotErrors"), c("normsPlotImg", ".plotNorms")))
            expect_true(renders(a, p[1], p[2]), info = p[1])
    }
})

test_that("aisp, msacheck and mlmsa plots draw with every setting", {
    data <- edgeTwoDimData()
    two <- edgeTwoLevelData()
    for (s in settings) {
        a <- do.call(runAnalysis, c(list("aisp", data, vars = names(data), lbMode = "sequence"), s))
        expect_true(renders(a, "sequencePlot", ".plotSequence"))
        b <- do.call(runAnalysis, c(list("msacheck", data, vars = names(data), checkIIO = TRUE, checkRest = TRUE,
                                         checkPmatrix = TRUE, iioPlot = TRUE, restPlot = TRUE, pmatPlot = TRUE), s))
        for (p in list(c("critPlot", ".plotCrit"), c("monoPlot", ".plotMono"), c("iioPlot", ".plotPairs"),
                       c("restPlot", ".plotPairs"), c("pmatPlot", ".plotPmatrix")))
            expect_true(renders(b, p[1], p[2]), info = p[1])
        m <- do.call(runAnalysis, c(list("mlmsa", two, subject = "Subject", vars = paste0("r", 1:5)), s))
        expect_true(renders(m, "levelPlot", ".plotLevels"))
    }
})

test_that("every curve type of the response function plot draws", {
    data <- edgeScaleData()
    for (curves in c("both", "IRF", "ISRF")) {
        a <- runAnalysis("msacheck", data, vars = names(data), monoCurves = curves, plotCI = FALSE)
        expect_true(renders(a, "monoPlot", ".plotMono"), info = curves)
    }
})

# 1.0.3: with Items to plot empty, the per-item plots draw the first 12 items
# (a whole scale of up to 12 items) and the pair plots the pairs among the
# first six items; chosen items always win.
# ES: 1.0.3: con Ítems a graficar vacío, los gráficos por ítem dibujan los 12
# primeros ítems y los de pares los pares entre los seis primeros; los ítems
# elegidos siempre mandan.
test_that("msacheck plots every item of a short scale when Items to plot is empty", {
    data <- edgeScaleData(items = 8)
    items <- names(data)
    a <- runAnalysis("msacheck", data, vars = items, monoPlot = TRUE, monoCurves = "ISRF",
                     checkPmatrix = TRUE, pmatPlot = TRUE, checkRest = TRUE, restPlot = TRUE)
    expect_setequal(unique(a$results$monoPlot$state$data$item), items)
    expect_setequal(unique(a$results$pmatPlot$state$data$item), items)
    pairs <- unique(as.character(a$results$restPlot$state$data$pair))
    expect_true(all(unique(a$results$restPlot$state$data$item) %in% items[1:6]))
    expect_lte(length(pairs), choose(6, 2))

    long <- edgeScaleData(items = 14)
    a <- runAnalysis("msacheck", long, vars = names(long), monoPlot = TRUE, monoCurves = "IRF")
    expect_setequal(unique(a$results$monoPlot$state$data$item), names(long)[1:12])

    a <- runAnalysis("msacheck", data, vars = items, monoPlot = TRUE, plotItems = items[c(2, 7)])
    expect_setequal(unique(a$results$monoPlot$state$data$item), items[c(2, 7)])
})

test_that("item response function panels put the curve type on its own line", {
    data <- edgeScaleData(items = 5)
    a <- runAnalysis("msacheck", data, vars = names(data), monoPlot = TRUE)
    grDevices::pdf(NULL)
    on.exit(grDevices::dev.off())
    expect_true(renders(a, "monoPlot", ".plotMono"))
    strips <- levels(ggplot2::last_plot()$data$facet)
    expect_true(all(grepl("\n", strips, fixed = TRUE)))
})

test_that("color bar titles sit above the bar", {
    for (pos in c("bottom", "right")) {
        g <- .mk_colourbar(list(plotLegend = pos))$fill
        expect_identical(g$title.position, "top")
    }
})

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

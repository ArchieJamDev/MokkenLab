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
# Shared edge-case data fixtures and helpers.
# ES: Datos compartidos para casos límite y funciones auxiliares.
#
# Every MokkenLab analysis is exercised on pathological input built here: a
# clean unidimensional structure (the positive control), a two-dimensional
# structure, accented and symbol item names, a constant item, an entirely
# missing item, non-integer scores, missing values, too few items and tiny
# samples. Tests assert graceful behavior (an explanatory jmvcore::reject()
# or a filled table), never a numeric result; numeric agreement with mokken
# itself lives in validation/.
#
# ES: Cada análisis de MokkenLab se pone a prueba con entradas patológicas
# construidas aquí: una estructura unidimensional limpia (el control
# positivo), una bidimensional, nombres con acentos y símbolos, un ítem
# constante, uno vacío, puntajes no enteros, valores perdidos, muy pocos
# ítems y muestras diminutas. Las pruebas verifican un comportamiento
# controlado, nunca un resultado numérico; la concordancia numérica con
# mokken vive en validation/.
# -----------------------------------------------------------------------------

# Polytomous items (0-4) generated from one latent trait with increasing
# difficulty: a strong Mokken scale.
# ES: Ítems politómicos (0-4) generados desde un rasgo latente con dificultad
# creciente: una escala de Mokken fuerte.
edgeScaleData <- function(n = 300, items = 8, seed = 20261104, dims = 1) {
    withr::local_seed(seed)
    theta <- matrix(stats::rnorm(n * dims), ncol = dims)
    out <- lapply(seq_len(items), function(j) {
        t <- theta[, (j - 1) %% dims + 1]
        loc <- seq(-1.5, 1.5, length.out = items)[j]
        steps <- loc + c(-1.2, -0.4, 0.4, 1.2)
        rowSums(sapply(steps, function(b) stats::runif(n) < stats::plogis(1.7 * (t - b))))
    })
    data <- as.data.frame(out)
    names(data) <- paste0("item", seq_len(items))
    data
}

edgeTwoDimData <- function() edgeScaleData(items = 10, dims = 2)

edgeSpecialNameData <- function() {
    data <- edgeScaleData()
    names(data)[1:3] <- c("niño_años", "café ± más", "ítem 3 (inv.)")
    data
}

edgeConstantItemData <- function() {
    data <- edgeScaleData()
    data$item1 <- 2
    data
}

edgeEmptyItemData <- function() {
    data <- edgeScaleData()
    data$item1 <- NA_real_
    data
}

edgeNonIntegerData <- function() {
    data <- edgeScaleData()
    data$item2 <- data$item2 + 0.5
    data
}

edgeMissingData <- function(rate = 0.05) {
    data <- edgeScaleData()
    withr::local_seed(7)
    for (v in names(data))
        data[[v]][stats::runif(nrow(data)) < rate] <- NA
    data
}

edgeTwoLevelData <- function(subjects = 15, raters = 12) {
    withr::local_seed(11)
    subject_theta <- stats::rnorm(subjects)
    id <- rep(seq_len(subjects), each = raters)
    theta <- subject_theta[id] + stats::rnorm(length(id), sd = 0.8)
    items <- lapply(1:5, function(j) {
        steps <- seq(-1, 1, length.out = 5)[j] + c(-1, 0, 1)
        rowSums(sapply(steps, function(b) stats::runif(length(id)) < stats::plogis(1.5 * (theta - b))))
    })
    data <- data.frame(Subject = factor(id), as.data.frame(items))
    names(data)[-1] <- paste0("r", 1:5)
    data
}

# Run an analysis through its R6 class (as jamovi does), so private render
# functions can be called on the result.
# ES: Ejecutar un análisis a través de su clase R6 (como jamovi), para poder
# llamar a las funciones privadas de render sobre el resultado.
# Run an analysis with jamovi's language set to `lang`, as jamovi does when
# its interface language is Spanish (jmvcore reads LANGUAGE when no language
# is sent with the options).
# ES: Ejecutar un análisis con el idioma de jamovi fijado en `lang`, como
# hace jamovi cuando su interfaz está en español.
runAnalysisIn <- function(lang, name, data, ...) {
    withr::local_envvar(c(LANGUAGE = lang))
    runAnalysis(name, data, ...)
}

runAnalysis <- function(name, data, ...) {
    cls <- get(paste0(name, "Class"), envir = asNamespace("MokkenLab"))
    opts <- get(paste0(name, "Options"), envir = asNamespace("MokkenLab"))
    analysis <- cls$new(options = opts$new(...), data = data)
    analysis$init()
    analysis$run()
    analysis
}

# Draw an image the way jamovi does (render function with ggtheme and theme)
# into a null device; TRUE when it drew something.
# ES: Dibujar una imagen como lo hace jamovi en un dispositivo nulo; TRUE si
# dibujó algo.
renders <- function(analysis, image, fun) {
    grDevices::pdf(NULL)
    on.exit(grDevices::dev.off())
    render <- analysis$.__enclos_env__$private[[fun]]
    isTRUE(render(analysis$results[[image]], ggtheme = list(ggplot2::theme_bw()),
                  theme = list(color = c("#333333", "#333333"), fill = c("#FFFFFF", "#6B9DE8"))))
}

expectRejects <- function(expr, pattern) {
    expect_error(expr, pattern)
}

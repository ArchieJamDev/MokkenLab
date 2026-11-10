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
# Mokken Scale Analysis.
# ES: Análisis de escalamiento de Mokken.
#
# This file implements the complete workflow for evaluating a set of items as
# a Mokken scale: scalability coefficients (mokken::coefH) with standard
# errors and confidence intervals, tests against a lower bound
# (mokken::coefZ), coefficients by group and across completed data sets,
# reliability (check.reliability), Guttman-error outliers (check.errors),
# correlation bounds for dichotomous items (check.bounds) and norm statistics
# of the total score (check.norms).
#
# ES: Este archivo implementa el flujo completo para evaluar un conjunto de
# ítems como escala de Mokken: coeficientes de escalabilidad (mokken::coefH)
# con errores estándar e intervalos de confianza, pruebas contra una cota
# inferior (mokken::coefZ), coeficientes por grupo y entre conjuntos
# completados, fiabilidad (check.reliability), atípicos por errores de
# Guttman (check.errors), cotas de correlación para ítems dicotómicos
# (check.bounds) y estadísticos normativos de la puntuación total
# (check.norms).
#
# Workflow
#   1. Validate and prepare items (Category A)
#   2. Scalability coefficients and Z tests
#   3. Completed data sets and groups
#   4. Reliability, outliers, bounds, norms (Category B on failure)
#   5. Plots (state only)
#   6. Interpretation, key findings, notes
#
# ES: Flujo
#   1. Validar y preparar los ítems (Categoría A)
#   2. Coeficientes de escalabilidad y pruebas Z
#   3. Conjuntos completados y grupos
#   4. Fiabilidad, atípicos, cotas, normas (Categoría B si fallan)
#   5. Gráficos (solo estado)
#   6. Interpretación, hallazgos clave, notas
#
# Error policy (CODE_STYLE.md §19.1):
#   Category A - fewer than two items, no data, empty, constant or
#     non-integer items, a declared score range that excludes observed
#     scores, coefH() failing -> jmvcore::reject().
#   Category B - any optional section that fails (Z tests, groups,
#     reliability, outliers, bounds, norms) -> its table still renders, with a
#     "Not computable" row and a footnote naming the reason.
#
# ES: Política de errores (CODE_STYLE.md §19.1):
#   Categoría A - menos de dos ítems, sin datos, ítems vacíos, constantes o
#     no enteros, un rango declarado que excluye puntajes observados, o
#     coefH() que falla -> jmvcore::reject().
#   Categoría B - cualquier sección opcional que falle -> su tabla se
#     muestra igual, con una fila "No computable" y una nota al pie con la
#     razón.
# -----------------------------------------------------------------------------

msaClass <- if (requireNamespace("jmvcore", quietly = TRUE)) R6::R6Class(
    "msaClass",
    inherit = msaBase,
    private = list(

        # Lines of the key findings summary of this run.
        # ES: Líneas del resumen de hallazgos de esta ejecución.
        .key = NULL,
        .addKey = function(type, text) {
            if (!is.null(private$.key) && length(text) > 0)
                private$.key[[type]] <- c(private$.key[[type]], text)
        },

        # ---------------------------------------------------------------------
        # Structure known from the options alone, laid out before .run() so
        # the tables do not change shape when the results arrive: one Hij
        # column per selected item (the rows come from `rows: (vars)`).
        # ES: Estructura que se conoce solo con las opciones, dispuesta antes
        # de .run() para que las tablas no cambien de forma al llegar los
        # resultados: una columna Hij por ítem seleccionado (las filas vienen
        # de `rows: (vars)`).
        # ---------------------------------------------------------------------
        .init = function() {
            items <- unlist(self$options$vars)
            if (isTRUE(self$options$showHij))
                for (j in seq_along(items))
                    self$results$hij$addColumn(name = paste0("c", j), title = items[j], type = "number", format = "zto")
        },

        .run = function() {

            # -----------------------------------------------------------------
            # Initial setup.
            # ES: Configuración inicial.
            # -----------------------------------------------------------------
            lang <- .mk_lang(self$options)
            . <- tr <- lang$tr
            txt <- function(section, key) .mk_text(lang, section, key)

            # The introduction is the empty state: the first item assigned
            # replaces it with results, so it never stays above them.
            # ES: La introducción es el estado vacío: el primer ítem asignado
            # la reemplaza por resultados, así que nunca queda sobre ellos.
            if (length(self$options$vars) == 0) {
                self$results$intro$setContent(.mk_html_block(NULL, txt("msa", "intro")))
                return()
            }
            if (isTRUE(self$options$showGuides))
                private$.fillGuides(lang)
            private$.key <- list(findings = character(0), uncertain = character(0), warnings = character(0))

            # -----------------------------------------------------------------
            # Validation and preparation (Category A).
            # ES: Validación y preparación (Categoría A).
            # -----------------------------------------------------------------
            group_name <- self$options$groupVar
            cluster_name <- self$options$clusterVar
            prep <- .mk_prepare_items(self$data, self$options$vars, self$options,
                                      min_items = 2, keep = c(group_name, cluster_name))
            if (!isTRUE(prep$ok))
                .mk_reject_prep(lang, prep)

            X <- as.matrix(prep$data)
            items <- colnames(X)
            level2 <- .mk_level_two(prep, cluster_name)
            level <- as.numeric(self$options$ciWidth) / 100
            warnings <- character(0)

            if (isTRUE(self$options$showChoicesGuide))
                self$results$choicesGuide$setContent(.mk_choices_guide(lang, c(
                    .mk_data_choices(lang, self$options, prep),
                    private$.choices(lang, tr, txt, !is.null(level2)))))

            # -----------------------------------------------------------------
            # Scalability coefficients (Category A if coefH() fails).
            # ES: Coeficientes de escalabilidad (Categoría A si coefH()
            # falla).
            # -----------------------------------------------------------------
            fit <- .mk_cached_capture(self$results$cacheStore, "coefH", list(X, se = TRUE, nice.output = FALSE, results = FALSE,
                                             level.two.var = level2), mokken::coefH,
                                      extract = .mk_keep(c("H", "se.H", "Hi", "se.Hi", "Hij")))
            if (is.null(fit$value))
                jmvcore::reject(sprintf(txt("common", "estimation_failed"), fit$error))
            warnings <- c(warnings, fit$warnings)
            H <- fit$value

            h_scale <- .mk_clean_num(H$H)
            se_scale <- .mk_clean_num(H$se.H)
            hi <- as.numeric(H$Hi)
            se_hi <- as.numeric(H$se.Hi)
            names(hi) <- names(se_hi) <- items

            # Z tests. With nested data mokken only offers the Wald-based and
            # range-preserving tests; the original Z is switched to Wald and
            # the report says so.
            # ES: Pruebas Z. Con datos anidados mokken solo ofrece las
            # pruebas de Wald y preservadora de rango; la Z original se
            # cambia a Wald y el informe lo dice.
            type_z <- self$options$typeZ
            if (!is.null(level2) && identical(type_z, "Z"))
                type_z <- "WB"
            lb <- as.numeric(self$options$zLowerbound)
            zfit <- NULL
            if (isTRUE(self$options$showZ)) {
                # The original Z test only tests against zero (Mokken, 1971).
                # ES: La prueba Z original solo contrasta contra cero.
                z_lb <- if (identical(type_z, "Z")) 0 else lb
                zres <- .mk_cached_capture(self$results$cacheStore, "coefZ", list(X, lowerbound = z_lb, type.z = type_z,
                                                  level.two.var = level2), mokken::coefZ,
                                           extract = .mk_keep(c("Z", "Zi")))
                warnings <- c(warnings, zres$warnings)
                zfit <- zres$value
                if (is.null(zfit))
                    private$.addKey("warnings", sprintf(txt("msa", "key_z_failed"), zres$error))
            }
            z_scale <- if (!is.null(zfit)) .mk_clean_num(zfit$Z) else NA_real_
            z_items <- if (!is.null(zfit)) as.numeric(zfit$Zi) else rep(NA_real_, length(items))
            pz <- function(z) if (is.na(z)) NA_real_ else stats::pnorm(z, lower.tail = FALSE)

            ci_scale <- .mk_ci(h_scale, se_scale, level, self$options$typeCI)
            ci_items <- .mk_ci(hi, se_hi, level, self$options$typeCI)

            # -----------------------------------------------------------------
            # Summary table.
            # ES: Tabla de resumen.
            # -----------------------------------------------------------------
            dichotomous <- private$.fillSummary(lang, prep, items, h_scale, level2, cluster_name, type_z)

            # -----------------------------------------------------------------
            # Scale and item tables.
            # ES: Tablas de la escala y de los ítems.
            # -----------------------------------------------------------------
            .mk_add_row(self$results$scale, 1, list(
                coef = "H", est = h_scale, se = se_scale,
                lower = .mk_clean_num(ci_scale$lower), upper = .mk_clean_num(ci_scale$upper),
                z = z_scale, p = pz(z_scale), strength = .mk_h_label(lang, h_scale)))
            if (isTRUE(self$options$showZ) && !is.null(zfit))
                self$results$scale$setNote("z", private$.zNote(lang, type_z, lb))

            for (i in seq_along(items))
                .mk_add_row(self$results$items, items[i], list(
                    item = items[i], mean = .mk_clean_num(mean(X[, i])), est = .mk_clean_num(hi[i]),
                    se = .mk_clean_num(se_hi[i]), lower = .mk_clean_num(ci_items$lower[i]),
                    upper = .mk_clean_num(ci_items$upper[i]), z = .mk_clean_num(z_items[i]),
                    p = pz(.mk_clean_num(z_items[i]))))
            if (isTRUE(self$options$showZ) && !is.null(zfit))
                self$results$items$setNote("z", private$.zNote(lang, type_z, lb))
            if (isTRUE(self$options$ci))
                self$results$items$setNote("ci", sprintf(txt("msa", "ci_note"),
                    .mk_fmt_num(100 * level, 1),
                    if (identical(self$options$typeCI, "RP")) .("range-preserving [intervals]")
                    else .("Wald-based [inline]")))

            # Item pairs: one column per item, Hij in each cell.
            # ES: Pares de ítems: una columna por ítem, Hij en cada celda.
            negative_pairs <- private$.fillPairs(lang, items, as.matrix(H$Hij))

            # -----------------------------------------------------------------
            # Completed data sets (two-way imputation with several sets).
            # ES: Conjuntos completados (imputación two-way con varios
            # conjuntos).
            # -----------------------------------------------------------------
            set_range <- private$.runCompletedSets(lang, prep, level2)

            # -----------------------------------------------------------------
            # Coefficients by group (Category B).
            # ES: Coeficientes por grupo (Categoría B).
            # -----------------------------------------------------------------
            if (!is.null(group_name) && length(group_name) > 0 && !is.null(prep$extra))
                warnings <- c(warnings, private$.runGroups(prep, X, group_name, level, lang, tr, txt))
            # The plot cannot be declared `visible: (groupPlot && groupVar)`:
            # jmvcore evaluates that in R, where an empty variable option is
            # NULL and `&&` fails ("Could not resolve"), so it is hidden here.
            # ES: El gráfico no puede declararse `visible: (groupPlot &&
            # groupVar)`: jmvcore lo evalúa en R, donde una opción de
            # variable vacía es NULL y `&&` falla, así que se oculta aquí.
            else
                self$results$groupPlotImg$setVisible(FALSE)

            # -----------------------------------------------------------------
            # Reliability, outliers, bounds and norms (Category B).
            # ES: Fiabilidad, atípicos, cotas y normas (Categoría B).
            # -----------------------------------------------------------------
            rel <- NULL
            if (isTRUE(self$options$showReliability))
                rel <- private$.runReliability(X, lang, tr, txt)
            errors <- NULL
            if (isTRUE(self$options$showErrors) || isTRUE(self$results$gplusOV$enabled) ||
                isTRUE(self$results$oplusOV$enabled))
                errors <- private$.runErrors(prep, X, lang, tr, txt)
            if (isTRUE(self$options$showBounds))
                private$.runBounds(X, dichotomous, lang, tr, txt)
            if (isTRUE(self$options$showNorms))
                private$.runNorms(X, lang, tr, txt)

            # -----------------------------------------------------------------
            # Item scalability plot: only what it draws goes into its state.
            # ES: Gráfico de escalabilidad de los ítems: en su estado va solo
            # lo que dibuja.
            # -----------------------------------------------------------------
            if (isTRUE(self$options$hiPlot))
                .mk_set_size(self$results$hiPlot, self$options, 550, max(320, 24 * length(items) + 140))
            if (isTRUE(self$options$hiPlot))
                self$results$hiPlot$setState(list(
                    data = data.frame(item = items, est = as.numeric(hi),
                                      lower = as.numeric(ci_items$lower), upper = as.numeric(ci_items$upper),
                                      stringsAsFactors = FALSE),
                    ci = isTRUE(self$options$ci),
                    lowerbound = if (lb > 0) lb else 0.3,
                    labels = list(x = .("Item scalability (Hi)"),
                                  bound = .("Lower bound"))))

            # -----------------------------------------------------------------
            # Interpretation, key findings and notes.
            # ES: Interpretación, hallazgos clave y notas.
            # -----------------------------------------------------------------
            private$.interpret(lang, tr, txt, h_scale, ci_scale, hi, z_items, type_z, lb, negative_pairs,
                               rel, errors, set_range, prep)

            for (section in .mk_failed_sections(self$results, lang))
                private$.addKey("warnings", sprintf(txt("common", "key_failed"), section))
            if (isTRUE(self$options$showKeySummary))
                self$results$keySummary$setContent(.mk_key_summary(lang, private$.key))

            # Reproducibility record: versions, seeds and the equivalent R
            # code of this run.
            # ES: Registro de reproducibilidad: versiones, semillas y el
            # código R equivalente de esta ejecución.
            if (isTRUE(self$options$showRepro))
                private$.fillRepro(lang, prep, items, level, level2, type_z, lb, group_name, cluster_name)

            .mk_insert_notices(self$results, self$options, .mk_translate_notices(warnings, lang))
        },

        # ---------------------------------------------------------------------
        # Summary table: what was analysed and how. Returns whether every
        # item is dichotomous, which the bounds check needs.
        # ES: Tabla de resumen: qué se analizó y cómo. Devuelve si todos los
        # ítems son dicotómicos, lo que necesita la revisión de cotas.
        # ---------------------------------------------------------------------
        .fillSummary = function(lang, prep, items, h_scale, level2, cluster_name, type_z) {
            . <- tr <- lang$tr
            txt <- function(section, key) .mk_text(lang, section, key)
            categories <- vapply(prep$data, function(x) length(unique(x)), numeric(1))
            dichotomous <- all(categories == 2)
            summary_rows <- list(
                list(.("Items"), as.character(length(items))),
                list(.("Cases used"), .mk_cases_label(tr, prep)),
                list(.("Score range"), sprintf("%s – %s",
                     .mk_fmt_num(prep$range[1], 0), .mk_fmt_num(prep$range[2], 0))),
                list(.("Item type"), if (dichotomous)
                     .("Dichotomous") else if (min(categories) == max(categories))
                     sprintf(.("Polytomous (%d observed categories)"),
                             as.integer(max(categories))) else
                     sprintf(.("Polytomous (%d to %d observed categories)"),
                             as.integer(min(categories)), as.integer(max(categories)))),
                list(.("Scale H"), sprintf("%s (%s)",
                     .mk_fmt_num(h_scale, 3), .mk_h_label(lang, h_scale))))
            summary_rows[[length(summary_rows) + 1]] <- list(
                    .("Negatively worded items (reversed)"),
                    .mk_reversed_label(tr, prep))
            if (!is.null(level2))
                summary_rows[[length(summary_rows) + 1]] <- list(
                    .("Clusters"), sprintf(.("%s (%d clusters; standard errors account for nesting)"),
                        cluster_name, length(unique(level2))))
            if (isTRUE(self$options$showZ) && !identical(type_z, self$options$typeZ))
                summary_rows[[length(summary_rows) + 1]] <- list(
                    .("Z test"), txt("notices", "typez_wb"))
            for (i in seq_along(summary_rows))
                .mk_add_row(self$results$summary, i,
                            list(element = summary_rows[[i]][[1]], value = summary_rows[[i]][[2]]))
            dichotomous
        },

        # ---------------------------------------------------------------------
        # Item pairs: Hij matrix and heat map. Returns the negative pairs.
        # ES: Pares de ítems: matriz Hij y mapa de calor. Devuelve los pares
        # negativos.
        # ---------------------------------------------------------------------
        .fillPairs = function(lang, items, hij) {
            txt <- function(section, key) .mk_text(lang, section, key)
            negative_pairs <- character(0)
            for (i in seq_along(items))
                for (j in seq_along(items))
                    if (i < j && is.finite(hij[i, j]) && hij[i, j] < 0)
                        negative_pairs <- c(negative_pairs, paste(items[i], items[j], sep = " – "))
            if (isTRUE(self$options$hijPlot)) {
                grid <- expand.grid(i = seq_along(items), j = seq_along(items))
                n_items <- length(items)
                .mk_set_size(self$results$hijPlotImg, self$options, max(450, 34 * n_items + 200),
                             max(380, 30 * n_items + 160))
                self$results$hijPlotImg$setState(list(
                    data = data.frame(item1 = items[grid$i], item2 = items[grid$j],
                                      h = ifelse(grid$i == grid$j, NA_real_, hij[cbind(grid$i, grid$j)]),
                                      stringsAsFactors = FALSE),
                    items = items, labels = list(fill = "Hij")))
            }
            if (isTRUE(self$options$showHij)) {
                table <- self$results$hij
                for (i in seq_along(items)) {
                    values <- list(item = items[i])
                    for (j in seq_along(items))
                        # The diagonal is left blank: an item has no pair
                        # with itself (NA would print as "NaN").
                        # ES: La diagonal queda vacía: un ítem no forma par
                        # consigo mismo (NA se imprimiría como "NaN").
                        values[[paste0("c", j)]] <- if (i == j) "" else .mk_clean_num(hij[i, j])
                    .mk_add_row(table, items[i], values)
                }
                table$setNote("neg", txt("msa", "hij_note"))
            }
            negative_pairs
        },

        # ---------------------------------------------------------------------
        # Completed data sets (two-way imputation with several sets).
        # Returns the range of H across sets, or NULL.
        # ES: Conjuntos completados (imputación two-way con varios
        # conjuntos). Devuelve el rango de H entre conjuntos, o NULL.
        # ---------------------------------------------------------------------
        .runCompletedSets = function(lang, prep, level2) {
            txt <- function(section, key) .mk_text(lang, section, key)
            set_range <- NULL
            if (!is.null(prep$completed) && length(prep$completed) > 1) {
                table <- self$results$completedSets
                table$setVisible(TRUE)
                set_h <- numeric(0)
                for (s in seq_along(prep$completed)) {
                    res <- .mk_cached_capture(self$results$cacheStore, "coefH", list(as.matrix(prep$completed[[s]]), se = TRUE,
                                                     nice.output = FALSE, results = FALSE,
                                                     level.two.var = level2), mokken::coefH,
                                              extract = .mk_keep(c("H", "se.H", "Hi")))
                    if (is.null(res$value)) {
                        .mk_add_row(table, s, list(set = s))
                        table$addFootnote(rowKey = s, col = "est", res$error)
                        next
                    }
                    set_h <- c(set_h, .mk_clean_num(res$value$H))
                    .mk_add_row(table, s, list(set = s, est = .mk_clean_num(res$value$H),
                                               se = .mk_clean_num(res$value$se.H),
                                               minHi = .mk_clean_num(min(res$value$Hi))))
                }
                table$setNote("sets", txt("msa", "sets_note"))
                if (length(set_h) > 0)
                    set_range <- range(set_h)
            }
            set_range
        },

        # ---------------------------------------------------------------------
        # Reproducibility record: versions, seeds and the equivalent R code
        # of this run.
        # ES: Registro de reproducibilidad: versiones, semillas y el código R
        # equivalente de esta ejecución.
        # ---------------------------------------------------------------------
        .fillRepro = function(lang, prep, items, level, level2, type_z, lb, group_name, cluster_name) {
            . <- lang$tr
            opts <- self$options
            keep <- c(group_name, cluster_name)
            keep <- keep[!is.null(keep) & nzchar(keep)]
            l2 <- if (!is.null(level2)) paste0("@as.integer(factor(extra[[", encodeString(cluster_name, quote = "\""), "]]))")
            code <- .mk_r_prep_code(prep, opts, items, keep)
            code <- c(code, .mk_r_call("coefH", "X", list(se = TRUE, ci = if (isTRUE(opts$ci)) level,
                type.ci = if (isTRUE(opts$ci)) opts$typeCI, level.two.var = l2)))
            if (isTRUE(opts$showZ))
                code <- c(code, .mk_r_call("coefZ", "X", list(lowerbound = if (identical(type_z, "Z")) 0 else lb,
                                                              type.z = type_z, level.two.var = l2)))
            if (length(group_name) > 0 && !is.null(group_name))
                code <- c(code, .mk_r_call("coefH", "X", list(group.var = paste0("@as.integer(factor(extra[[",
                    encodeString(group_name, quote = "\""), "]]))"))))
            if (isTRUE(opts$showReliability))
                code <- c(code, .mk_r_call("check.reliability", "X", list(MS = isTRUE(opts$relMS),
                    alpha = isTRUE(opts$relAlpha), lambda.2 = isTRUE(opts$relLambda2), LCRC = isTRUE(opts$relLCRC),
                    nclass = if (identical(opts$lcrcClassesMode, "manual")) as.integer(opts$lcrcClasses) else length(items) - 1L,
                    irc = isTRUE(opts$relIRC))))
            if (isTRUE(opts$showErrors))
                code <- c(code, .mk_r_call("check.errors", "X", list(returnGplus = isTRUE(opts$errorsGplus),
                                                                     returnOplus = isTRUE(opts$errorsOplus))))
            if (isTRUE(opts$showBounds))
                code <- c(code, .mk_r_call("check.bounds", "X", list(quant = as.numeric(opts$boundsQuant),
                    lower = isTRUE(opts$boundsLower), upper = isTRUE(opts$boundsUpper))))
            if (isTRUE(opts$showNorms))
                code <- c(code, "check.norms(rowSums(X))")
            seeds <- if (!is.null(prep$completed)) sprintf(.("Two-way imputation seed: %d."),
                                                          as.integer(opts$twowaySeed)) else character(0)
            repro <- .mk_repro_block(lang, code, seeds)
            self$results$reproducibility$setContent(repro$html)
            self$results$reproducibilityCode$setContent(repro$code)
        },

        # ---------------------------------------------------------------------
        # Explanation of the analysis-specific decisions for "About your
        # choices".
        # ES: Explicación de las decisiones propias del análisis para "Acerca
        # de sus elecciones".
        # ---------------------------------------------------------------------
        .choices = function(lang, tr, txt, nested) {
            . <- tr
            out <- list()
            if (isTRUE(self$options$ci))
                out[[length(out) + 1]] <- c(
                    paste0(txt("choices", "h_ci"), ": ", if (identical(self$options$typeCI, "RP"))
                        .("Range-preserving") else .("Wald-based"), "."),
                    txt("choices", paste0("ci_", self$options$typeCI)))
            if (isTRUE(self$options$showZ))
                out[[length(out) + 1]] <- c(
                    paste0(txt("choices", "h_z"), ": ", switch(self$options$typeZ,
                        Z = .("Original Z"), WB = .("Wald-based"),
                        RP = .("Range-preserving [test]")), "."),
                    sprintf(txt("choices", paste0("z_", self$options$typeZ)),
                            .mk_fmt_num(self$options$zLowerbound, 2)))
            if (nested)
                out[[length(out) + 1]] <- c(paste0(txt("choices", "h_cluster"), ": ", self$options$clusterVar, "."),
                                            txt("choices", "cluster_body"))
            out
        },

        # ---------------------------------------------------------------------
        # Teaching blocks before each table (texts.R, section "guide").
        # ES: Bloques didácticos antes de cada tabla (texts.R, sección
        # "guide").
        # ---------------------------------------------------------------------
        .fillGuides = function(lang) {
            guides <- c(scaleGuide = "scale", hijGuide = "hij", reliabilityGuide = "reliability",
                        errorsGuide = "errors", boundsGuide = "bounds", normsGuide = "norms")
            for (name in names(guides))
                self$results[[name]]$setContent(.mk_guide_block(lang, guides[[name]]))
        },

        .zNote = function(lang, type_z, lb) {
            . <- lang$tr
            if (identical(type_z, "Z"))
                return(.mk_text(lang, "msa", "z_note_original"))
            sprintf(.mk_text(lang, "msa", "z_note_bound"), .mk_fmt_num(lb, 2),
                    if (identical(type_z, "RP")) .("range-preserving")
                    else .("Wald-based [inline]"))
        },

        # ---------------------------------------------------------------------
        # Scalability by group: coefH(group.var =). One row per group in the
        # scale table and one column per group in the item table.
        # ES: Escalabilidad por grupo: coefH(group.var =). Una fila por grupo
        # en la tabla de la escala y una columna por grupo en la de ítems.
        # ---------------------------------------------------------------------
        .runGroups = function(prep, X, group_name, level, lang, tr, txt) {
            . <- tr
            table <- self$results$groups
            item_table <- self$results$groupItems

            groups <- factor(as.character(prep$extra[[group_name]]))
            sizes <- table(groups)
            small <- names(sizes)[sizes < 2]
            res <- if (length(small) > 0) list(value = NULL, error = sprintf(txt("msa", "group_small"),
                                                                            paste(small, collapse = ", ")),
                                               warnings = character(0))
                   else .mk_cached_capture(self$results$cacheStore, "coefH", list(X, se = TRUE, nice.output = FALSE, results = FALSE,
                                                  group.var = as.integer(groups)), mokken::coefH,
                           extract = function(x) list(Groups = lapply(x$Groups, .mk_keep(c("H", "se.H", "Hi")))))

            if (is.null(res$value) || is.null(res$value$Groups)) {
                reason <- sprintf(txt("msa", "group_failed"), if (is.null(res$error)) "" else res$error)
                .mk_add_row(table, 1, list(group = txt("common", "not_computable")))
                table$addFootnote(rowKey = 1, col = "group", reason)
                .mk_add_row(item_table, 1, list(item = txt("common", "not_computable")))
                item_table$addFootnote(rowKey = 1, col = "item", reason)
                return(res$warnings)
            }

            labels <- levels(groups)
            for (g in seq_along(labels)) {
                gres <- res$value$Groups[[g]]
                hg <- .mk_clean_num(gres$H)
                seg <- .mk_clean_num(gres$se.H)
                ci <- .mk_ci(hg, seg, level, self$options$typeCI)
                .mk_add_row(table, labels[g], list(group = labels[g], n = as.integer(sizes[[labels[g]]]),
                    est = hg, se = seg, lower = .mk_clean_num(ci$lower), upper = .mk_clean_num(ci$upper)))
                item_table$addColumn(name = paste0("g", g), title = labels[g], type = "number", format = "zto",
                                     superTitle = .("Hi by group"))
            }
            items <- colnames(X)
            for (i in seq_along(items)) {
                values <- list(item = items[i])
                for (g in seq_along(labels))
                    values[[paste0("g", g)]] <- .mk_clean_num(as.numeric(res$value$Groups[[g]]$Hi)[i])
                .mk_add_row(item_table, items[i], values)
            }

            if (isTRUE(self$options$groupPlot)) {
                long <- do.call(rbind, lapply(seq_along(labels), function(g) data.frame(
                    group = labels[g], item = items, est = as.numeric(res$value$Groups[[g]]$Hi),
                    stringsAsFactors = FALSE)))
                .mk_set_size(self$results$groupPlotImg, self$options, 600, max(340, 24 * length(items) + 160))
                self$results$groupPlotImg$setState(list(data = long, items = items, labels = list(
                    x = .("Item scalability (Hi)"), color = .("Group"))))
            }
            hs <- vapply(seq_along(labels), function(g) .mk_clean_num(res$value$Groups[[g]]$H), numeric(1))
            if (all(is.finite(hs)) && length(hs) > 1)
                private$.addKey("findings", sprintf(txt("msa", "key_groups"), length(labels),
                                                    .mk_fmt_num(min(hs), 2), .mk_fmt_num(max(hs), 2)))
            res$warnings
        },

        # ---------------------------------------------------------------------
        # Reliability: check.reliability(). Returns the named coefficients.
        # ES: Fiabilidad: check.reliability(). Devuelve los coeficientes con
        # nombre.
        # ---------------------------------------------------------------------
        .runReliability = function(X, lang, tr, txt) {
            . <- tr
            table <- self$results$reliability
            opts <- self$options
            if (!any(c(opts$relMS, opts$relAlpha, opts$relLambda2, opts$relLCRC, opts$relIRC))) {
                .mk_add_row(table, 1, list(coef = txt("common", "not_computable")))
                table$addFootnote(rowKey = 1, col = "coef", txt("msa", "rel_none"))
                return(NULL)
            }
            nclass <- if (identical(opts$lcrcClassesMode, "manual")) as.integer(opts$lcrcClasses) else ncol(X) - 1
            res <- .mk_cached_capture(self$results$cacheStore, "check.reliability", list(
                X, MS = isTRUE(opts$relMS), alpha = isTRUE(opts$relAlpha), lambda.2 = isTRUE(opts$relLambda2),
                LCRC = isTRUE(opts$relLCRC), nclass = nclass, irc = isTRUE(opts$relIRC)), mokken::check.reliability,
                extract = .mk_keep(c("MS", "alpha", "lambda.2", "LCRC", "irc")))

            if (is.null(res$value)) {
                .mk_add_row(table, 1, list(coef = txt("common", "not_computable")))
                table$addFootnote(rowKey = 1, col = "coef", sprintf(txt("msa", "rel_failed"), res$error))
                return(NULL)
            }
            rel <- res$value
            rows <- list(
                MS = c(isTRUE(opts$relMS), .("Molenaar-Sijtsma (MS)")),
                alpha = c(isTRUE(opts$relAlpha), .("Cronbach's alpha")),
                lambda.2 = c(isTRUE(opts$relLambda2), .("Guttman's lambda-2")),
                LCRC = c(isTRUE(opts$relLCRC), sprintf(.("Latent class reliability (LCRC, %d classes)"), nclass)))
            for (k in names(rows)) {
                if (!identical(rows[[k]][1], "TRUE"))
                    next
                value <- .mk_clean_num(rel[[k]])
                .mk_add_row(table, k, list(coef = rows[[k]][2], value = value))
                if (is.na(value))
                    table$addFootnote(rowKey = k, col = "value", txt("msa", "rel_value_na"))
                else if (identical(k, "LCRC"))
                    table$addFootnote(rowKey = k, col = "coef", sprintf(txt("msa", "lcrc_note"), nclass,
                        if (identical(opts$lcrcClassesMode, "manual")) txt("msa", "lcrc_manual") else txt("msa", "lcrc_auto")))
            }
            if (isTRUE(opts$relIRC) && !is.null(rel$irc)) {
                for (i in seq_len(ncol(X)))
                    .mk_add_row(self$results$irc, colnames(X)[i],
                                list(item = colnames(X)[i], irc = .mk_clean_num(rel$irc[i])))
            }
            rel
        },

        # ---------------------------------------------------------------------
        # Guttman errors (G+) and infrequent scores (O+): check.errors(), with
        # mokken's own fences (U1: Tukey, U2: medcouple-adjusted), and the
        # optional dataset columns.
        # ES: Errores de Guttman (G+) y puntajes infrecuentes (O+):
        # check.errors(), con las vallas del propio mokken (U1: Tukey, U2:
        # ajustada por medcouple), y las columnas opcionales del conjunto.
        # ---------------------------------------------------------------------
        .runErrors = function(prep, X, lang, tr, txt) {
            . <- tr
            table <- self$results$errors
            want_g <- isTRUE(self$options$errorsGplus) || isTRUE(self$results$gplusOV$enabled)
            want_o <- isTRUE(self$options$errorsOplus) || isTRUE(self$results$oplusOV$enabled)
            if (!want_g && !want_o) {
                .mk_add_row(table, 1, list(stat = txt("common", "not_computable")))
                table$addFootnote(rowKey = 1, col = "stat", txt("msa", "errors_none"))
                return(NULL)
            }
            res <- .mk_cached_capture(self$results$cacheStore, "check.errors", list(X, returnGplus = want_g, returnOplus = want_o), mokken::check.errors,
                                      extract = .mk_keep(c("Gplus", "Oplus", "UGplus", "UOplus")))
            if (is.null(res$value)) {
                .mk_add_row(table, 1, list(stat = txt("common", "not_computable")))
                table$addFootnote(rowKey = 1, col = "stat", sprintf(txt("msa", "errors_failed"), res$error))
                return(NULL)
            }
            err <- res$value
            out <- list()
            add <- function(key, label, values, u1, u2) {
                u1 <- .mk_clean_num(u1)
                u2 <- .mk_clean_num(u2)
                if (isTRUE(self$options$showErrors))
                    .mk_add_row(table, key, list(
                        stat = label, mean = .mk_clean_num(mean(values)), median = .mk_clean_num(stats::median(values)),
                        max = .mk_clean_num(max(values)), u1 = u1, n1 = as.integer(sum(values > u1, na.rm = TRUE)),
                        u2 = u2, n2 = as.integer(sum(values > u2, na.rm = TRUE))))
                list(values = values, u1 = u1, u2 = u2, label = label)
            }
            if (!is.null(err$Gplus) && isTRUE(self$options$errorsGplus))
                out$G <- add("G", .("Guttman errors (G+)"), as.numeric(err$Gplus),
                             err$UGplus$U1Gplus, err$UGplus$U2Gplus)
            if (!is.null(err$Oplus) && isTRUE(self$options$errorsOplus))
                out$O <- add("O", .("Infrequent scores (O+)"), as.numeric(err$Oplus),
                             err$UOplus$U1Oplus, err$UOplus$U2Oplus)
            if (isTRUE(self$options$showErrors))
                table$setNote("fences", txt("msa", "errors_note"))

            # Dataset columns, aligned on jamovi's own row numbers.
            # ES: Columnas del conjunto de datos, alineadas con los números de
            # fila de jamovi.
            save <- function(output, values, title) {
                if (isTRUE(output$enabled) && output$isNotFilled()) {
                    output$set(keys = 1, titles = title, descriptions = title, measureTypes = "continuous")
                    output$setRowNums(rownames(prep$data))
                    output$setValues(index = 1, as.numeric(values))
                }
            }
            if (!is.null(err$Gplus))
                save(self$results$gplusOV, err$Gplus, "Gplus")
            if (!is.null(err$Oplus))
                save(self$results$oplusOV, err$Oplus, "Oplus")

            if (isTRUE(self$options$showErrors) && isTRUE(self$options$errorsPlot) && length(out) > 0)
                self$results$errorsPlot$setState(list(
                    series = lapply(out, function(s) s[c("values", "u1", "u2", "label")]),
                    labels = list(x = .("Score"), y = .("Respondents"),
                                  u1 = .("Tukey fence (U1)"),
                                  u2 = .("Skew-adjusted fence (U2)"))))
            out
        },

        # ---------------------------------------------------------------------
        # Relative bounds for the correlations of dichotomous items
        # (check.bounds(), Ellis, 2014). Polytomous items: Category B row.
        # ES: Cotas relativas para las correlaciones de ítems dicotómicos
        # (check.bounds(), Ellis, 2014). Ítems politómicos: fila de Categoría
        # B.
        # ---------------------------------------------------------------------
        .runBounds = function(X, dichotomous, lang, tr, txt) {
            . <- tr
            table <- self$results$bounds
            if (!dichotomous) {
                .mk_add_row(table, 1, list(item1 = txt("common", "not_computable")))
                table$addFootnote(rowKey = 1, col = "item1", txt("msa", "bounds_polytomous"))
                return(invisible(NULL))
            }
            lower <- isTRUE(self$options$boundsLower)
            upper <- isTRUE(self$options$boundsUpper)
            res <- .mk_cached_capture(self$results$cacheStore, "check.bounds", list(X, quant = as.numeric(self$options$boundsQuant),
                                                    lower = lower, upper = upper), mokken::check.bounds,
                                      extract = .mk_keep(c("LowerBounds", "UpperBounds")))
            if (is.null(res$value) || (!lower && !upper)) {
                .mk_add_row(table, 1, list(item1 = txt("common", "not_computable")))
                table$addFootnote(rowKey = 1, col = "item1", if (!lower && !upper) txt("msa", "bounds_none")
                                  else sprintf(txt("msa", "bounds_failed"), res$error))
                return(invisible(NULL))
            }
            r <- stats::cor(X)
            lb <- res$value$LowerBounds
            ub <- res$value$UpperBounds
            items <- colnames(X)
            violations <- 0L
            invalid_upper <- 0L
            for (i in seq_along(items)) for (j in seq_along(items)) {
                if (i >= j) next
                l1 <- if (lower) .mk_clean_num(lb$L1rij[i, j]) else NA_real_
                l2 <- if (lower) .mk_clean_num(lb$L2rij[i, j]) else NA_real_
                u1 <- if (upper) .mk_clean_num(ub$U1rij[i, j]) else NA_real_
                u2 <- if (upper) .mk_clean_num(ub$U2rij[i, j]) else NA_real_
                rij <- .mk_clean_num(r[i, j])
                # mokken 3.1.2 can return upper bounds outside [-1, 1] (seen
                # with the transitive reasoning data). A bound a correlation
                # cannot reach says nothing about this pair, so it is shown
                # but left out of the verdict, and the table says so.
                # ES: mokken 3.1.2 puede devolver cotas superiores fuera de
                # [-1, 1] (visto con los datos de razonamiento transitivo).
                # Una cota que una correlación no puede alcanzar no dice nada
                # del par, así que se muestra pero no entra en el veredicto, y
                # la tabla lo dice.
                u1_use <- if (!is.na(u1) && abs(u1) <= 1) u1 else NA_real_
                u2_use <- if (!is.na(u2) && abs(u2) <= 1) u2 else NA_real_
                if (upper && (is.na(u1_use) && !is.na(u1) || is.na(u2_use) && !is.na(u2)))
                    invalid_upper <- invalid_upper + 1L
                # L1 overestimates the lower bound and L2 underestimates it,
                # so only r < L2 is a clear violation; r < L1 alone is a
                # possible one. Mirror logic for the upper bounds (U1
                # overestimates, U2 underestimates).
                # ES: L1 sobreestima la cota inferior y L2 la subestima, así
                # que solo r < L2 es una violación clara; r < L1 sola es
                # posible. Lógica espejo para las cotas superiores.
                verdict <- .("Within the bounds")
                if (!is.na(l2) && rij < l2 || !is.na(u1_use) && rij > u1_use) {
                    verdict <- .("Violation")
                    violations <- violations + 1L
                } else if (!is.na(l1) && rij < l1 || !is.na(u2_use) && rij > u2_use) {
                    verdict <- .("Possible violation")
                }
                .mk_add_row(table, paste(i, j), list(item1 = items[i], item2 = items[j], r = rij,
                                                     l1 = l1, l2 = l2, u1 = u1, u2 = u2, verdict = verdict))
            }
            table$setNote("bounds", txt("msa", "bounds_note"))
            if (invalid_upper > 0)
                table$setNote("invalid", sprintf(txt("msa", "bounds_invalid_upper"), invalid_upper))
            if (violations > 0)
                private$.addKey("warnings", sprintf(txt("msa", "key_bounds"), violations))
            invisible(NULL)
        },

        # ---------------------------------------------------------------------
        # Norm statistics of the total score with standard errors
        # (check.norms(), Oosterhuis et al., 2017).
        # ES: Estadísticos normativos de la puntuación total con errores
        # estándar (check.norms(), Oosterhuis et al., 2017).
        # ---------------------------------------------------------------------
        .runNorms = function(X, lang, tr, txt) {
            . <- tr
            y <- rowSums(X)
            res <- .mk_cached_capture(self$results$cacheStore, "check.norms", list(y, nice.output = FALSE), mokken::check.norms,
                                      extract = .mk_keep(c("mean", "sd", "z", "sta9", "perc")))
            table <- self$results$normsSummary
            if (is.null(res$value)) {
                .mk_add_row(table, 1, list(stat = txt("common", "not_computable")))
                table$addFootnote(rowKey = 1, col = "stat", sprintf(txt("msa", "norms_failed"), res$error))
                return(invisible(NULL))
            }
            nr <- res$value
            row4 <- function(m, i) list(est = .mk_clean_num(m[i, 1]), se = .mk_clean_num(m[i, 2]),
                                        lower = .mk_clean_num(m[i, 3]), upper = .mk_clean_num(m[i, 4]))
            .mk_add_row(table, "mean", c(list(stat = .("Mean")), row4(nr$mean, 1)))
            .mk_add_row(table, "sd", c(list(stat = .("Standard deviation")), row4(nr$sd, 1)))
            table$setNote("total", sprintf(txt("msa", "norms_note"), ncol(X)))

            scores <- function(m) suppressWarnings(as.integer(rownames(m)))
            if (isTRUE(self$options$normsZ))
                for (i in seq_len(nrow(nr$z)))
                    .mk_add_row(self$results$normsZ, i, c(list(score = scores(nr$z)[i]), row4(nr$z, i)))
            if (isTRUE(self$options$normsStanines))
                for (i in seq_len(nrow(nr$sta9)))
                    .mk_add_row(self$results$normsSta9, i, c(list(
                        bound = sprintf(.("Stanine %s"), rownames(nr$sta9)[i])), row4(nr$sta9, i)))
            if (isTRUE(self$options$normsPlot))
                self$results$normsPlotImg$setState(list(
                    data = data.frame(score = scores(nr$perc), est = nr$perc[, 1], lower = nr$perc[, 3],
                                      upper = nr$perc[, 4]),
                    labels = list(x = .("Total score"),
                                  y = .("Percentile rank"))))
            if (isTRUE(self$options$normsPercentiles)) {
                for (i in seq_len(nrow(nr$perc)))
                    .mk_add_row(self$results$normsPerc, i, c(list(score = scores(nr$perc)[i]), row4(nr$perc, i)))
                if (any(nr$perc[, 3] < 0 | nr$perc[, 4] > 100, na.rm = TRUE))
                    self$results$normsPerc$setNote("range", txt("msa", "norms_perc_range"))
            }
            invisible(NULL)
        },

        # ---------------------------------------------------------------------
        # Applied interpretation: what happened, why, what it means, what to
        # do next (texts.R, section "msa").
        # ES: Interpretación aplicada: qué ocurrió, por qué, qué significa,
        # qué hacer ahora (texts.R, sección "msa").
        # ---------------------------------------------------------------------
        .interpret = function(lang, tr, txt, h_scale, ci_scale, hi, z_items, type_z, lb, negative_pairs,
                              rel, errors, set_range, prep) {
            parts <- character(0)
            label <- .mk_h_label(lang, h_scale)

            if (is.na(h_scale)) {
                parts <- c(parts, txt("msa", "interp_h_na"))
            } else if (h_scale < 0.3) {
                parts <- c(parts, sprintf(txt("msa", "interp_h_low"), .mk_fmt_num(h_scale, 3)))
                private$.addKey("findings", sprintf(txt("msa", "key_h"), .mk_fmt_num(h_scale, 2), label))
            } else {
                parts <- c(parts, sprintf(txt("msa", "interp_h_ok"), .mk_fmt_num(h_scale, 3), label))
                private$.addKey("findings", sprintf(txt("msa", "key_h"), .mk_fmt_num(h_scale, 2), label))
            }
            if (isTRUE(self$options$ci) && !is.na(h_scale) && is.finite(ci_scale$lower) &&
                ci_scale$lower < 0.3 && h_scale >= 0.3) {
                parts <- c(parts, sprintf(txt("msa", "interp_ci_low"), .mk_fmt_num(ci_scale$lower, 3)))
                private$.addKey("uncertain", txt("msa", "key_ci_low"))
            }

            low_items <- names(hi)[is.finite(hi) & hi < 0.3]
            if (length(low_items) > 0) {
                parts <- c(parts, sprintf(txt("msa", "interp_low_items"), length(low_items),
                                          paste(low_items, collapse = ", ")))
                private$.addKey("warnings", sprintf(txt("msa", "key_low_items"), length(low_items),
                                                    paste(low_items, collapse = ", ")))
            } else if (any(is.finite(hi))) {
                parts <- c(parts, txt("msa", "interp_items_ok"))
            }

            if (isTRUE(self$options$showZ) && !identical(type_z, "Z") && lb > 0) {
                nonsig <- names(hi)[is.finite(z_items) & stats::pnorm(z_items, lower.tail = FALSE) >= 0.05]
                if (length(nonsig) > 0) {
                    parts <- c(parts, sprintf(txt("msa", "interp_z_nonsig"), .mk_fmt_num(lb, 2),
                                              length(nonsig), paste(nonsig, collapse = ", ")))
                    private$.addKey("uncertain", sprintf(txt("msa", "key_z_nonsig"), length(nonsig), .mk_fmt_num(lb, 2)))
                }
            }

            if (length(negative_pairs) > 0) {
                parts <- c(parts, sprintf(txt("msa", "interp_neg_pairs"), length(negative_pairs),
                                          paste(utils::head(negative_pairs, 10), collapse = "; ")))
                private$.addKey("warnings", sprintf(txt("msa", "key_neg_pairs"), length(negative_pairs)))
            }

            if (!is.null(set_range))
                parts <- c(parts, sprintf(txt("msa", "interp_sets"), .mk_fmt_num(set_range[1], 3),
                                          .mk_fmt_num(set_range[2], 3)))

            if (!is.null(rel)) {
                ms <- .mk_clean_num(rel$MS)
                if (!is.na(ms)) {
                    parts <- c(parts, sprintf(txt("msa", "interp_rel"), .mk_fmt_num(ms, 3)))
                    private$.addKey("findings", sprintf(txt("msa", "key_rel"), .mk_fmt_num(ms, 2)))
                }
            }

            if (!is.null(errors) && !is.null(errors$G)) {
                n_out <- sum(errors$G$values > errors$G$u2, na.rm = TRUE)
                if (n_out > 0) {
                    parts <- c(parts, sprintf(txt("msa", "interp_errors"), as.integer(n_out),
                                              length(errors$G$values)))
                    private$.addKey("warnings", sprintf(txt("msa", "key_errors"), as.integer(n_out)))
                } else {
                    n_u1 <- sum(errors$G$values > errors$G$u1, na.rm = TRUE)
                    parts <- c(parts, sprintf(txt("msa", "interp_errors_none"), as.integer(n_u1)))
                }
            }

            next_key <- if (!is.na(h_scale) && h_scale >= 0.3 && length(low_items) == 0) "interp_next_ok" else "interp_next_aisp"
            parts <- c(parts, txt("msa", next_key))
            if (!is.null(prep$completed))
                parts <- c(parts, txt("msa", "interp_imputed"))

            self$results$interpretation$setContent(.mk_interpretation(lang, parts))
        },


        # ---------------------------------------------------------------------
        # Item scalability: each Hi with its confidence interval, sorted, the
        # lower bound dashed and the zone below it shaded. Standard ggplot,
        # so jamovi's ggtheme applies.
        # ES: Escalabilidad de los ítems: cada Hi con su intervalo de
        # confianza, ordenados, con la cota inferior en línea discontinua y
        # la zona por debajo sombreada.
        # ---------------------------------------------------------------------
        .plotHi = function(image, ggtheme, theme, ...) {
            state <- image$state
            if (is.null(state) || is.null(state$data) || nrow(state$data) == 0)
                return(FALSE)
            df <- state$data
            df$item <- factor(df$item, levels = df$item[order(df$est)])
            style <- .mk_plot_style(self$options)
            colors <- .mk_plot_colors(theme)
            plot <- ggplot2::ggplot(df, ggplot2::aes(x = item, y = est)) +
                ggplot2::annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = state$lowerbound,
                                  alpha = 0.07, fill = colors$alert) +
                ggplot2::geom_hline(yintercept = state$lowerbound, linetype = "dashed", color = colors$alert,
                                    linewidth = style$line)
            if (isTRUE(state$ci))
                plot <- plot + ggplot2::geom_errorbar(ggplot2::aes(ymin = lower, ymax = upper),
                                                      width = 0.25, color = colors$line, linewidth = style$line)
            plot <- plot + ggplot2::geom_point(size = style$point * 1.4, color = colors$line)
            if (style$labels)
                plot <- plot + ggplot2::geom_text(ggplot2::aes(label = formatC(est, format = "f", digits = 2)),
                                                  vjust = -0.9, size = 3)
            plot <- plot + ggplot2::coord_flip(ylim = if (style$full) c(min(0, min(df$lower, na.rm = TRUE)), 1) else NULL) +
                ggplot2::labs(x = NULL, y = state$labels$x) +
                ggtheme + .mk_plot_theme(self$options) + ggplot2::theme(legend.position = "none")
            print(plot)
            TRUE
        },

        # ---------------------------------------------------------------------
        # Item-pair heat map: Hij in every cell, diverging around 0 so that
        # negative pairs stand out, with .30 marked by the cell text color.
        # ES: Mapa de calor de pares: Hij en cada celda, divergente alrededor
        # de 0 para que los pares negativos resalten.
        # ---------------------------------------------------------------------
        .plotHij = function(image, ggtheme, theme, ...) {
            state <- image$state
            if (is.null(state) || is.null(state$data) || nrow(state$data) == 0)
                return(FALSE)
            df <- state$data
            df$item1 <- factor(df$item1, levels = rev(state$items))
            df$item2 <- factor(df$item2, levels = state$items)
            style <- .mk_plot_style(self$options)
            high <- "#2C7BB6"
            low <- "#D7191C"
            plot <- ggplot2::ggplot(df, ggplot2::aes(x = item2, y = item1, fill = h)) +
                ggplot2::geom_tile(color = "white", linewidth = 0.4)
            if (style$labels || length(state$items) <= 15)
                plot <- plot + ggplot2::geom_text(ggplot2::aes(label = ifelse(is.na(h), "", formatC(h, format = "f", digits = 2))),
                                                  size = 2.8)
            plot <- plot +
                ggplot2::labs(x = NULL, y = NULL, fill = state$labels$fill) +
                ggtheme + .mk_plot_theme(self$options) +
                ggplot2::scale_fill_gradient2(low = low, mid = "white", high = high, midpoint = 0,
                                              limits = c(-1, 1), breaks = c(-1, -0.5, 0, 0.5, 1),
                                              na.value = "grey92")
            # A horizontal color bar needs room for its five labels.
            # ES: Una barra de color horizontal necesita espacio para sus
            # cinco etiquetas.
            if (self$options$plotLegend %in% c("bottom", "top"))
                plot <- plot + ggplot2::theme(legend.key.width = grid::unit(1.6, "cm"))
            print(plot)
            TRUE
        },

        # ---------------------------------------------------------------------
        # Item scalability by group: one point per group for each item, so
        # items that scale differently across groups are easy to spot.
        # ES: Escalabilidad por grupo: un punto por grupo para cada ítem, para
        # detectar los ítems que escalan distinto entre grupos.
        # ---------------------------------------------------------------------
        .plotGroups = function(image, ggtheme, theme, ...) {
            state <- image$state
            if (is.null(state) || is.null(state$data) || nrow(state$data) == 0)
                return(FALSE)
            df <- state$data
            df$item <- factor(df$item, levels = rev(state$items))
            style <- .mk_plot_style(self$options)
            plot <- ggplot2::ggplot(df, ggplot2::aes(x = item, y = est, color = group, group = item)) +
                ggplot2::geom_hline(yintercept = 0.3, linetype = "dashed", color = "grey50") +
                ggplot2::geom_line(color = "grey70", linewidth = style$line) +
                ggplot2::geom_point(size = style$point * 1.4)
            if (style$labels)
                plot <- plot + ggplot2::geom_text(ggplot2::aes(label = formatC(est, format = "f", digits = 2)),
                                                  vjust = -0.9, size = 2.6, show.legend = FALSE)
            plot <- plot + ggplot2::coord_flip(ylim = if (style$full) c(min(0, min(df$est, na.rm = TRUE)), 1) else NULL) +
                ggplot2::labs(x = NULL, y = state$labels$x, color = state$labels$color) +
                ggtheme + .mk_plot_theme(self$options)
            print(plot)
            TRUE
        },

        # ---------------------------------------------------------------------
        # Percentile rank of every total score with its 95% band (Oosterhuis
        # et al., 2017): where the band is wide, the norm is imprecise.
        # ES: Rango percentil de cada puntuación total con su banda al 95%:
        # donde la banda es ancha, la norma es imprecisa.
        # ---------------------------------------------------------------------
        .plotNorms = function(image, ggtheme, theme, ...) {
            state <- image$state
            if (is.null(state) || is.null(state$data) || nrow(state$data) == 0)
                return(FALSE)
            df <- state$data
            style <- .mk_plot_style(self$options)
            colors <- .mk_plot_colors(theme)
            plot <- ggplot2::ggplot(df, ggplot2::aes(x = score, y = est)) +
                ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper), fill = colors$fill, alpha = 0.3) +
                ggplot2::geom_line(color = colors$line, linewidth = style$line) +
                ggplot2::geom_point(color = colors$line, size = style$point)
            if (style$labels)
                plot <- plot + ggplot2::geom_text(ggplot2::aes(label = formatC(est, format = "f", digits = 0)),
                                                  vjust = -0.9, size = 2.6)
            # Wald intervals of percentile ranks can pass 0 or 100 at the
            # extremes; the band is drawn within the possible range.
            # ES: Los intervalos de Wald de los rangos percentiles pueden
            # pasar de 0 o 100 en los extremos; la banda se dibuja dentro del
            # rango posible.
            df$lower <- pmax(df$lower, 0)
            df$upper <- pmin(df$upper, 100)
            plot$data <- df
            if (style$full)
                plot <- plot + ggplot2::coord_cartesian(ylim = c(0, 100))
            plot <- plot + ggplot2::labs(x = state$labels$x, y = state$labels$y) +
                ggtheme + .mk_plot_theme(self$options)
            print(plot)
            TRUE
        },

        # ---------------------------------------------------------------------
        # Distribution of G+ (and O+): one histogram per statistic with
        # mokken's two fences.
        # ES: Distribución de G+ (y O+): un histograma por estadístico con
        # las dos vallas de mokken.
        # ---------------------------------------------------------------------
        .plotErrors = function(image, ggtheme, theme, ...) {
            state <- image$state
            if (is.null(state) || length(state$series) == 0)
                return(FALSE)
            df <- do.call(rbind, lapply(state$series, function(s)
                data.frame(stat = s$label, value = s$values, stringsAsFactors = FALSE)))
            fences <- do.call(rbind, lapply(state$series, function(s)
                data.frame(stat = s$label, fence = c(state$labels$u1, state$labels$u2),
                           value = c(s$u1, s$u2), stringsAsFactors = FALSE)))
            fences <- fences[is.finite(fences$value), , drop = FALSE]
            style <- .mk_plot_style(self$options, length(state$series))
            colors <- .mk_plot_colors(theme)
            plot <- ggplot2::ggplot(df, ggplot2::aes(x = value)) +
                ggplot2::geom_histogram(bins = 30, fill = colors$fill, color = colors$line, linewidth = 0.2) +
                ggplot2::geom_vline(data = fences, ggplot2::aes(xintercept = value, linetype = fence),
                                    color = colors$alert, linewidth = style$line) +
                ggplot2::facet_wrap(~ stat, scales = "free", ncol = style$ncol) +
                ggplot2::labs(x = state$labels$x, y = state$labels$y, linetype = NULL) +
                ggtheme + .mk_plot_theme(self$options)
            print(plot)
            TRUE
        }
    )
)

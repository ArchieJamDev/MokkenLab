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
# Automated Item Selection.
# ES: Selección automática de ítems.
#
# This file implements mokken::aisp(): Mokken's automated item selection
# procedure (a bottom-up, sequential clustering of items into scales) or the
# genetic algorithm of Straat et al. (2013), at one lower bound c or across a
# sequence of lower bounds, with the scalability of every scale it finds
# (mokken::coefH on each scale's items).
#
# ES: Este archivo implementa mokken::aisp(): el procedimiento de selección
# automática de Mokken (un agrupamiento secuencial ascendente de ítems en
# escalas) o el algoritmo genético de Straat et al. (2013), con una cota
# inferior c o una secuencia de cotas, con la escalabilidad de cada escala
# encontrada (mokken::coefH sobre los ítems de cada escala).
#
# Workflow
#   1. Validate and prepare items (Category A)
#   2. Build the lower bounds and the start set
#   3. Run aisp() (reproducible seed for the genetic algorithm)
#   4. Membership and scalability of each scale, or the sequence table
#   5. Plot state, interpretation, key findings, notes
#
# ES: Flujo
#   1. Validar y preparar los ítems (Categoría A)
#   2. Construir las cotas inferiores y el conjunto inicial
#   3. Ejecutar aisp() (semilla reproducible para el algoritmo genético)
#   4. Pertenencia y escalabilidad de cada escala, o la tabla de secuencia
#   5. Estado del gráfico, interpretación, hallazgos, notas
#
# Error policy (CODE_STYLE.md §19.1):
#   Category A - data problems (see .mk_prepare_items), an empty or inverted
#     sequence of lower bounds, aisp() failing -> jmvcore::reject().
#   Category B - coefH() failing for one scale -> its row renders with a
#     footnote naming the reason.
#
# ES: Política de errores (CODE_STYLE.md §19.1):
#   Categoría A - problemas de datos, una secuencia de cotas vacía o
#     invertida, aisp() que falla -> jmvcore::reject().
#   Categoría B - coefH() que falla para una escala -> su fila se muestra con
#     una nota al pie que nombra la razón.
#
# Not exposed: aisp(search = "extended") and aisp(verbose =). The "extended"
# branch of mokken 3.1.2 is an unfinished stub that never receives the items
# and fails on any data; verbose only prints progress to the R console.
#
# ES: No expuestos: aisp(search = "extended") y aisp(verbose =). La rama
# "extended" de mokken 3.1.2 es un esbozo inacabado que nunca recibe los
# ítems y falla con cualquier dato; verbose solo imprime el progreso en la
# consola de R.
# -----------------------------------------------------------------------------

aispClass <- if (requireNamespace("jmvcore", quietly = TRUE)) R6::R6Class(
    "aispClass",
    inherit = aispBase,
    private = list(

        .key = NULL,
        .addKey = function(type, text) {
            if (!is.null(private$.key) && length(text) > 0)
                private$.key[[type]] <- c(private$.key[[type]], text)
        },

        # ---------------------------------------------------------------------
        # Structure known from the options alone: one column per lower bound
        # of the sequence (the rows come from `rows: (vars)`).
        # ES: Estructura que se conoce solo con las opciones: una columna por
        # cota de la secuencia (las filas vienen de `rows: (vars)`).
        # ---------------------------------------------------------------------
        .init = function() {
            lbs <- private$.lowerBounds()
            if (identical(self$options$lbMode, "sequence") && length(lbs) > 0)
                for (k in seq_along(lbs))
                    self$results$sequence$addColumn(name = paste0("c", k), title = paste0("c = ", .mk_fmt_num(lbs[k], 2)),
                                                    type = "integer")
        },

        # The lower bounds the options ask for; empty when the sequence is
        # invalid (.run() rejects it then).
        # ES: Las cotas que piden las opciones; vacío si la secuencia no es
        # válida (.run() la rechaza entonces).
        .lowerBounds = function() {
            if (!identical(self$options$lbMode, "sequence"))
                return(as.numeric(self$options$lowerbound))
            from <- as.numeric(self$options$lbFrom)
            to <- as.numeric(self$options$lbTo)
            step <- as.numeric(self$options$lbStep)
            if (!(from < to) || !(step > 0))
                return(numeric(0))
            round(seq(from, to, by = step), 4)
        },

        .run = function() {

            lang <- .mk_lang(self$options)
            . <- tr <- lang$tr
            txt <- function(section, key) .mk_text(lang, section, key)

            # The introduction is the empty state: the first item assigned
            # replaces it with results, so it never stays above them.
            # ES: La introducción es el estado vacío: el primer ítem asignado
            # la reemplaza por resultados, así que nunca queda sobre ellos.
            if (length(self$options$vars) == 0) {
                self$results$intro$setContent(.mk_html_block(NULL, txt("aisp", "intro")))
                return()
            }
            if (isTRUE(self$options$showGuides)) {
                self$results$aispGuide$setContent(.mk_guide_block(lang, "aisp"))
                self$results$sequenceGuide$setContent(.mk_guide_block(lang, "sequence"))
            }
            private$.key <- list(findings = character(0), uncertain = character(0), warnings = character(0))

            # -----------------------------------------------------------------
            # Validation and preparation (Category A).
            # ES: Validación y preparación (Categoría A).
            # -----------------------------------------------------------------
            cluster_name <- self$options$clusterVar
            prep <- .mk_prepare_items(self$data, self$options$vars, self$options,
                                      min_items = 2, keep = cluster_name)
            if (!isTRUE(prep$ok))
                .mk_reject_prep(lang, prep)
            X <- as.matrix(prep$data)
            items <- colnames(X)
            level2 <- .mk_level_two(prep, cluster_name)

            sequence <- identical(self$options$lbMode, "sequence")
            if (sequence) {
                if (!(as.numeric(self$options$lbFrom) < as.numeric(self$options$lbTo)))
                    jmvcore::reject(txt("aisp", "bad_sequence"))
            }
            lbs <- private$.lowerBounds()

            start <- match(intersect(as.character(unlist(self$options$startSet)), items), items)
            ga <- identical(self$options$search, "ga")

            # With nested data, or when Hi is tested against c, mokken
            # replaces the original Z test by the Wald-based one; the module
            # does it explicitly so the report states the test actually used.
            # ES: Con datos anidados, o al contrastar Hi contra c, mokken
            # reemplaza la Z original por la de Wald; el módulo lo hace
            # explícitamente para que el informe diga la prueba realmente
            # usada.
            type_z <- self$options$typeZ
            z_changed <- identical(type_z, "Z") && (!is.null(level2) || isTRUE(self$options$testHi))
            if (z_changed)
                type_z <- "WB"

            if (isTRUE(self$options$showChoicesGuide))
                self$results$choicesGuide$setContent(.mk_choices_guide(lang, c(
                    .mk_data_choices(lang, self$options, prep),
                    private$.choices(lang, tr, txt, ga, type_z, length(start) > 0))))

            # -----------------------------------------------------------------
            # Item selection (Category A on failure). aisp() has no seed
            # argument; the genetic algorithm draws from R's generator, so
            # the seed is set here and the previous random state restored.
            # ES: Selección de ítems (Categoría A si falla). aisp() no tiene
            # argumento de semilla; el algoritmo genético usa el generador de
            # R, así que aquí se fija la semilla y luego se restaura el
            # estado aleatorio previo.
            # -----------------------------------------------------------------
            args <- list(X = X, lowerbound = lbs, search = self$options$search,
                         alpha = as.numeric(self$options$alpha), StartSet = if (length(start) > 0) start else FALSE,
                         popsize = as.integer(self$options$popsize),
                         pxover = as.numeric(self$options$pxover), pmutation = as.numeric(self$options$pmutation),
                         type.z = type_z, test.Hi = isTRUE(self$options$testHi), level.two.var = level2)
            if (identical(self$options$maxgensMode, "manual"))
                args$maxgens <- as.integer(self$options$maxgens)

            had_seed <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
            old_seed <- if (had_seed) get(".Random.seed", envir = globalenv()) else NULL
            set.seed(as.integer(self$options$gaSeed))
            fit <- .mk_cached_capture(self$results$cacheStore, "aisp", args, mokken::aisp, key_extra = as.integer(self$options$gaSeed))
            if (had_seed) assign(".Random.seed", old_seed, envir = globalenv())
            else if (exists(".Random.seed", envir = globalenv(), inherits = FALSE))
                rm(".Random.seed", envir = globalenv())

            if (is.null(fit$value))
                jmvcore::reject(sprintf(txt("aisp", "failed"), fit$error))
            warnings <- fit$warnings
            partition <- as.matrix(fit$value)
            rownames(partition) <- items

            # -----------------------------------------------------------------
            # Summary.
            # ES: Resumen.
            # -----------------------------------------------------------------
            search_label <- if (ga) .("Genetic algorithm") else
                .("Mokken's automated item selection procedure")
            z_label <- switch(type_z, Z = .("Original Z"), WB = .("Wald-based"),
                              RP = .("Range-preserving [test]"))
            rows <- list(
                list(.("Items"), as.character(length(items))),
                list(.("Cases used"), .mk_cases_label(tr, prep)),
                list(.("Search"), search_label),
                list(.("Lower bound c"), if (sequence)
                     sprintf(.("%s to %s in steps of %s (%d values)"),
                             .mk_fmt_num(min(lbs), 2), .mk_fmt_num(max(lbs), 2),
                             .mk_fmt_num(as.numeric(self$options$lbStep), 2), length(lbs))
                     else .mk_fmt_num(lbs, 2)),
                list(.("Tests during selection"), sprintf(
                    .("%s, alpha = %s%s"), z_label, .mk_fmt_num(self$options$alpha, 3),
                    if (isTRUE(self$options$testHi)) .("; Hi tested against c") else "")))
            if (ga)
                rows[[length(rows) + 1]] <- list(.("Genetic algorithm"), sprintf(
                    .("population %d, %s generations, crossover %s, mutation %s, seed %d"),
                    as.integer(self$options$popsize),
                    if (identical(self$options$maxgensMode, "manual")) as.character(as.integer(self$options$maxgens))
                    else .("default"),
                    .mk_fmt_num(self$options$pxover, 2), .mk_fmt_num(self$options$pmutation, 2),
                    as.integer(self$options$gaSeed)))
            if (length(start) > 0)
                rows[[length(rows) + 1]] <- list(.("Start set"), paste(items[start], collapse = ", "))
            if (!is.null(level2))
                rows[[length(rows) + 1]] <- list(.("Clusters"),
                    sprintf(.("%s (%d clusters)"), cluster_name, length(unique(level2))))
            rows[[length(rows) + 1]] <- list(.("Negatively worded items (reversed)"),
                                                 .mk_reversed_label(tr, prep))
            for (i in seq_along(rows))
                .mk_add_row(self$results$summary, i, list(element = rows[[i]][[1]], value = rows[[i]][[2]]))
            if (z_changed)
                self$results$summary$setNote("z", txt("aisp", "z_changed"))
            if (ga && length(start) > 0)
                self$results$summary$setNote("start", txt("aisp", "start_ga"))

            # -----------------------------------------------------------------
            # One lower bound: membership and the scalability of each scale.
            # A sequence: one column per lower bound.
            # ES: Una cota: pertenencia y escalabilidad de cada escala. Una
            # secuencia: una columna por cota.
            # -----------------------------------------------------------------
            if (!sequence) {
                private$.fillSingle(X, partition[, 1], level2, lang, tr, txt)
                if (isTRUE(self$options$bootStability))
                    warnings <- c(warnings, private$.runBoot(X, args, level2, as.integer(partition[, 1]), lang, tr, txt))
            } else {
                private$.fillSequence(partition, lbs, lang, tr, txt)
            }

            for (section in .mk_failed_sections(self$results, lang))
                private$.addKey("warnings", sprintf(txt("common", "key_failed"), section))
            if (isTRUE(self$options$showKeySummary))
                self$results$keySummary$setContent(.mk_key_summary(lang, private$.key))

            # Reproducibility record.
            # ES: Registro de reproducibilidad.
            if (isTRUE(self$options$showRepro))
                private$.fillRepro(lang, prep, items, cluster_name, level2, ga, lbs, start, type_z, sequence)

            .mk_insert_notices(self$results, self$options, .mk_translate_notices(warnings, lang))
        },

        # ---------------------------------------------------------------------
        # Reproducibility record: versions, seeds and the equivalent R code
        # of this run.
        # ES: Registro de reproducibilidad: versiones, semillas y el código R
        # equivalente de esta ejecución.
        # ---------------------------------------------------------------------
        .fillRepro = function(lang, prep, items, cluster_name, level2, ga, lbs, start, type_z, sequence) {
            . <- lang$tr
            txt <- function(section, key) .mk_text(lang, section, key)
            opts <- self$options
            keep <- if (!is.null(cluster_name)) cluster_name else character(0)
            code <- .mk_r_prep_code(prep, opts, items, keep)
            if (ga)
                code <- c(code, paste0("set.seed(", as.integer(opts$gaSeed), ")"))
            code <- c(code, .mk_r_call("aisp", "X", list(lowerbound = lbs, search = opts$search,
                alpha = as.numeric(opts$alpha), StartSet = if (length(start) > 0) start,
                popsize = if (ga) as.integer(opts$popsize),
                maxgens = if (ga && identical(opts$maxgensMode, "manual")) as.integer(opts$maxgens),
                pxover = if (ga) as.numeric(opts$pxover), pmutation = if (ga) as.numeric(opts$pmutation),
                type.z = type_z, test.Hi = isTRUE(opts$testHi),
                level.two.var = if (!is.null(level2)) paste0("@as.integer(factor(extra[[",
                    encodeString(cluster_name, quote = "\""), "]]))"))))
            if (!sequence && isTRUE(opts$bootStability))
                code <- c(code, sprintf(paste0("# Bootstrap stability: the same aisp() call on %d resamples of the rows ",
                                               "(of whole clusters, with nested data) after set.seed(%d)"),
                                        as.integer(opts$bootReps), as.integer(opts$bootSeed)))
            seeds <- c(if (!is.null(prep$completed)) sprintf(.("Two-way imputation seed: %d."),
                                                             as.integer(opts$twowaySeed)),
                       if (ga) sprintf(.("Genetic algorithm seed: %d."),
                                       as.integer(opts$gaSeed)),
                       if (!sequence && isTRUE(opts$bootStability))
                           sprintf(.("Bootstrap seed: %d."), as.integer(opts$bootSeed)))
            repro <- .mk_repro_block(lang, code, seeds)
            self$results$reproducibility$setContent(repro$html)
            self$results$reproducibilityCode$setContent(repro$code)
        },

        .choices = function(lang, tr, txt, ga, type_z, has_start) {
            . <- tr
            out <- list(
                c(paste0(txt("choices", "h_search"), ": ", if (ga) .("Genetic algorithm")
                         else .("Mokken's procedure"), "."),
                  txt("choices", if (ga) "search_ga" else "search_normal")),
                c(paste0(txt("choices", "h_lb"), ": ", if (identical(self$options$lbMode, "sequence"))
                         .("Sequence") else .mk_fmt_num(self$options$lowerbound, 2), "."),
                  txt("choices", if (identical(self$options$lbMode, "sequence")) "lb_sequence" else "lb_single")),
                c(paste0(txt("choices", "h_z"), ": ", switch(type_z, Z = .("Original Z"),
                         WB = .("Wald-based"), RP = .("Range-preserving [test]")), "."),
                  txt("choices", if (isTRUE(self$options$testHi)) "testhi_true" else "testhi_false")))
            if (has_start)
                out[[length(out) + 1]] <- c(paste0(txt("choices", "h_start"), "."), txt("choices", "start_body"))
            out
        },

        # ---------------------------------------------------------------------
        # One lower bound: item membership (0 = unscalable) and the
        # scalability of every scale with at least two items.
        # ES: Una cota: pertenencia de los ítems (0 = no escalable) y la
        # escalabilidad de cada escala con al menos dos ítems.
        # ---------------------------------------------------------------------
        .fillSingle = function(X, scale_of, level2, lang, tr, txt) {
            . <- tr
            items <- colnames(X)
            scale_of <- as.integer(scale_of)
            names(scale_of) <- items
            scale_ids <- sort(unique(scale_of[scale_of > 0]))
            unscalable <- items[scale_of == 0]
            hi_in_scale <- stats::setNames(rep(NA_real_, length(items)), items)
            se_in_scale <- stats::setNames(rep(NA_real_, length(items)), items)
            label_of <- function(s) if (s == 0) .("Unscalable") else sprintf(.("Scale %d"), s)
            label_of_vec <- function(v) vapply(v, label_of, character(1))

            table <- self$results$scales
            scale_lines <- character(0)
            for (s in scale_ids) {
                members <- items[scale_of == s]
                if (!isTRUE(self$options$scaleCoefs)) next
                res <- .mk_cached_capture(self$results$cacheStore, "coefH", list(X[, members, drop = FALSE], se = TRUE, nice.output = FALSE,
                                                 results = FALSE, level.two.var = level2), mokken::coefH,
                                          extract = .mk_keep(c("H", "se.H", "Hi", "se.Hi")))
                if (is.null(res$value)) {
                    .mk_add_row(table, s, list(scale = label_of(s), nItems = length(members),
                                               items = paste(members, collapse = ", ")))
                    table$addFootnote(rowKey = s, col = "h", sprintf(txt("aisp", "coef_failed"), res$error))
                    next
                }
                h <- .mk_clean_num(res$value$H)
                se <- .mk_clean_num(res$value$se.H)
                ci <- .mk_ci(h, se, 0.95, "WB")
                hi <- as.numeric(res$value$Hi)
                hi_in_scale[members] <- hi
                se_in_scale[members] <- as.numeric(res$value$se.Hi)
                .mk_add_row(table, s, list(scale = label_of(s), nItems = length(members),
                    items = paste(members, collapse = ", "), h = h, se = se,
                    lower = .mk_clean_num(ci$lower), upper = .mk_clean_num(ci$upper),
                    minHi = .mk_clean_num(min(hi)), strength = .mk_h_label(lang, h)))
                scale_lines <- c(scale_lines, sprintf(txt("aisp", "interp_scale"), label_of(s), length(members),
                                                      .mk_fmt_num(h, 3), .mk_h_label(lang, h)))
            }
            if (isTRUE(self$options$scaleCoefs) && length(scale_ids) > 0)
                table$setNote("ci", txt("aisp", "scales_note"))

            membership <- self$results$membership
            for (item in items)
                .mk_add_row(membership, item, list(item = item, scale = label_of(scale_of[[item]]),
                                                   hi = .mk_clean_num(hi_in_scale[[item]])))
            membership$setNote("order", txt("aisp", "membership_note"))

            # Plot state: Hi of every scaled item within its own scale (95%
            # Wald interval), the lower bound c marked, unscalable items
            # listed in the caption.
            # ES: Estado del gráfico: Hi de cada ítem escalado dentro de su
            # escala (intervalo de Wald al 95%), la cota c marcada y los ítems
            # no escalables en la leyenda inferior.
            if (isTRUE(self$options$scalePlot) && length(scale_ids) > 0 && isTRUE(self$options$scaleCoefs)) {
                scaled <- items[scale_of > 0]
                ci <- .mk_ci(hi_in_scale[scaled], se_in_scale[scaled], 0.95, "WB")
                .mk_set_size(self$results$scalePlotImg, self$options, 600, max(340, 26 * length(scaled) + 160))
                self$results$scalePlotImg$setState(list(
                    data = data.frame(item = scaled, scale = label_of_vec(scale_of[scaled]),
                                      est = as.numeric(hi_in_scale[scaled]), lower = as.numeric(ci$lower),
                                      upper = as.numeric(ci$upper), stringsAsFactors = FALSE),
                    lowerbound = as.numeric(self$options$lowerbound),
                    caption = if (length(unscalable) > 0)
                        sprintf(.("Unscalable: %s"), paste(unscalable, collapse = ", ")) else NULL,
                    labels = list(x = .("Item scalability within its scale (Hi)"),
                                  color = .("Scale"), bound = .("Lower bound c"))))
            }

            # Interpretation.
            # ES: Interpretación.
            parts <- character(0)
            if (length(scale_ids) == 0) {
                parts <- c(parts, sprintf(txt("aisp", "interp_none"), .mk_fmt_num(self$options$lowerbound, 2)))
                private$.addKey("warnings", txt("aisp", "key_none"))
            } else if (length(scale_ids) == 1 && length(unscalable) == 0) {
                parts <- c(parts, sprintf(txt("aisp", "interp_one_all"), .mk_fmt_num(self$options$lowerbound, 2)))
                private$.addKey("findings", txt("aisp", "key_one_all"))
            } else {
                parts <- c(parts, sprintf(txt("aisp", "interp_many"), length(scale_ids),
                                          .mk_fmt_num(self$options$lowerbound, 2)))
                private$.addKey("findings", sprintf(txt("aisp", "key_many"), length(scale_ids)))
            }
            parts <- c(parts, scale_lines)
            if (length(unscalable) > 0) {
                parts <- c(parts, sprintf(txt("aisp", "interp_unscalable"), length(unscalable),
                                          paste(unscalable, collapse = ", ")))
                private$.addKey("warnings", sprintf(txt("aisp", "key_unscalable"), length(unscalable),
                                                    paste(unscalable, collapse = ", ")))
            }
            parts <- c(parts, txt("aisp", "interp_caution"), txt("aisp", "interp_next"))
            self$results$interpretation$setContent(.mk_interpretation(lang, parts))
        },

        # ---------------------------------------------------------------------
        # Sequence of lower bounds: membership by c, a summary of each
        # partition, the heat-map state, and the reading of the pattern
        # (Hemker et al., 1995).
        # ES: Secuencia de cotas: pertenencia por c, un resumen de cada
        # partición, el estado del mapa de calor y la lectura del patrón
        # (Hemker et al., 1995).
        # ---------------------------------------------------------------------
        .fillSequence = function(partition, lbs, lang, tr, txt) {
            . <- tr
            items <- rownames(partition)
            table <- self$results$sequence
            for (item in items) {
                values <- list(item = item)
                for (k in seq_along(lbs))
                    values[[paste0("c", k)]] <- as.integer(partition[item, k])
                .mk_add_row(table, item, values)
            }
            table$setNote("zero", txt("aisp", "sequence_note"))

            n_scales <- integer(length(lbs))
            summary <- self$results$sequenceSummary
            for (k in seq_along(lbs)) {
                col <- as.integer(partition[, k])
                n_scales[k] <- length(unique(col[col > 0]))
                largest <- if (any(col > 0)) max(table(col[col > 0])) else 0L
                .mk_add_row(summary, k, list(lb = lbs[k], nScales = n_scales[k],
                                             nUnscalable = as.integer(sum(col == 0)), largest = as.integer(largest)))
            }

            if (isTRUE(self$options$sequencePlot)) {
                long <- data.frame(item = rep(items, times = length(lbs)),
                                   lb = rep(lbs, each = length(items)),
                                   scale = as.integer(as.vector(partition)), stringsAsFactors = FALSE)
                .mk_set_size(self$results$sequencePlot, self$options, max(500, 45 * length(lbs) + 220),
                             max(360, 24 * length(items) + 160))
                self$results$sequencePlot$setState(list(
                    data = long, items = items,
                    labels = list(x = .("Lower bound c"), fill = .("Scale"),
                                  unscalable = .("Unscalable"))))
            }

            # Reading the pattern: one scale whose items drop out as c rises
            # suggests one attribute; one scale splitting into several as c
            # rises suggests several related attributes (Hemker et al., 1995).
            # ES: Lectura del patrón: una escala cuyos ítems salen al subir c
            # sugiere un atributo; una escala que se divide en varias al
            # subir c sugiere varios atributos relacionados.
            parts <- c(sprintf(txt("aisp", "interp_seq_intro"), length(lbs), .mk_fmt_num(min(lbs), 2),
                               .mk_fmt_num(max(lbs), 2)))
            split_at <- which(n_scales > 1)
            if (length(split_at) > 0) {
                first <- lbs[split_at[1]]
                parts <- c(parts, sprintf(txt("aisp", "interp_seq_split"), .mk_fmt_num(first, 2),
                                          max(n_scales)))
                private$.addKey("findings", sprintf(txt("aisp", "key_seq_split"), .mk_fmt_num(first, 2), max(n_scales)))
            } else {
                parts <- c(parts, txt("aisp", "interp_seq_one"))
                private$.addKey("findings", txt("aisp", "key_seq_one"))
            }
            at30 <- which(abs(lbs - 0.3) < 1e-8)
            if (length(at30) == 1) {
                col <- as.integer(partition[, at30])
                parts <- c(parts, sprintf(txt("aisp", "interp_seq_30"), length(unique(col[col > 0])),
                                          as.integer(sum(col == 0))))
            }
            parts <- c(parts, txt("aisp", "interp_seq_next"))
            self$results$interpretation$setContent(.mk_interpretation(lang, parts))
        },


        # ---------------------------------------------------------------------
        # Bootstrap stability of the partition: the same selection on
        # resamples (cached, so cosmetic changes do not repeat it).
        # ES: Estabilidad bootstrap de la partición: la misma selección en
        # remuestras (en caché, para que los cambios cosméticos no la
        # repitan).
        # ---------------------------------------------------------------------
        .runBoot = function(X, args, level2, original, lang, tr, txt) {
            . <- tr
            if (isTRUE(self$options$showGuides))
                self$results$bootGuide$setContent(.mk_guide_block(lang, "boot"))
            table <- self$results$bootTable
            reps <- as.integer(self$options$bootReps)
            seed <- as.integer(self$options$bootSeed)
            boot_args <- args[setdiff(names(args), c("X", "level.two.var"))]
            boot_args$lowerbound <- boot_args$lowerbound[1]
            res <- .mk_cached_capture(self$results$cacheStore, "aisp_bootstrap",
                list(X = X, args = boot_args, reps = reps, seed = seed, level2 = level2, original = original),
                .mk_aisp_bootstrap)
            boot <- res$value
            if (is.null(boot) || is.null(boot$done) || boot$done == 0) {
                .mk_add_row(table, 1, list(item = txt("common", "not_computable")))
                table$addFootnote(rowKey = 1, col = "item", sprintf(txt("aisp", "boot_failed"),
                    if (!is.null(res$error)) res$error else ""))
                return(res$warnings)
            }
            items <- colnames(X)
            label_of <- function(s) if (s == 0) .("Unscalable") else sprintf(.("Scale %d"), s)
            for (i in seq_along(items))
                .mk_add_row(table, items[i], list(item = items[i], scale = label_of(original[i]),
                    together = .mk_clean_num(boot$together[i]), same = .mk_clean_num(boot$same[i]),
                    unscalable = .mk_clean_num(boot$unscalable[i])))
            table$setNote("boot", sprintf(txt("aisp", "boot_note"), boot$done, seed,
                                          .mk_fmt_num(boot$exact, 2)))
            if (boot$failed > 0)
                table$setNote("failed", sprintf(txt("aisp", "boot_failed_some"), boot$failed, boot$reps))

            weak <- items[is.finite(boot$same) & boot$same < 0.5 & original > 0]
            private$.addKey(if (length(weak) > 0) "uncertain" else "findings", if (length(weak) > 0)
                sprintf(txt("aisp", "key_boot_weak"), length(weak), paste(weak, collapse = ", "))
                else sprintf(txt("aisp", "key_boot_ok"), .mk_fmt_num(boot$exact, 2)))

            if (isTRUE(self$options$bootPlot)) {
                n <- length(items)
                .mk_set_size(self$results$bootPlotImg, self$options, max(450, 34 * n + 200), max(380, 30 * n + 160))
                grid <- expand.grid(i = seq_len(n), j = seq_len(n))
                order_items <- items[order(original == 0, original)]
                self$results$bootPlotImg$setState(list(
                    data = data.frame(item1 = items[grid$i], item2 = items[grid$j],
                                      p = ifelse(grid$i == grid$j, NA_real_, boot$co[cbind(grid$i, grid$j)]),
                                      stringsAsFactors = FALSE),
                    items = order_items, labels = list(fill = .("Same scale"))))
            }
            res$warnings
        },

        # ---------------------------------------------------------------------
        # Bootstrap co-membership: the proportion of resamples in which each
        # pair of items falls in the same scale, items ordered by the
        # partition found, so stable scales show as dark blocks.
        # ES: Co-pertenencia bootstrap: la proporción de remuestras en que
        # cada par de ítems cae en la misma escala, ítems ordenados por la
        # partición encontrada, para que las escalas estables se vean como
        # bloques oscuros.
        # ---------------------------------------------------------------------
        .plotBoot = function(image, ggtheme, theme, ...) {
            state <- image$state
            if (is.null(state) || is.null(state$data) || nrow(state$data) == 0)
                return(FALSE)
            df <- state$data
            df$item1 <- factor(df$item1, levels = rev(state$items))
            df$item2 <- factor(df$item2, levels = state$items)
            style <- .mk_plot_style(self$options)
            high <- "#2C7BB6"
            plot <- ggplot2::ggplot(df, ggplot2::aes(x = item2, y = item1, fill = p)) +
                ggplot2::geom_tile(color = "white", linewidth = 0.4)
            if (style$labels || length(state$items) <= 15)
                plot <- plot + ggplot2::geom_text(ggplot2::aes(label = ifelse(is.na(p), "", formatC(p, format = "f", digits = 2))),
                                                  size = 2.8)
            plot <- plot +
                ggplot2::labs(x = NULL, y = NULL, fill = state$labels$fill) +
                ggtheme + .mk_plot_theme(self$options) +
                ggplot2::scale_fill_gradient(low = "white", high = high, limits = c(0, 1), na.value = "grey92")
            if (self$options$plotLegend %in% c("bottom", "top"))
                plot <- plot + ggplot2::theme(legend.key.width = grid::unit(1.6, "cm"))
            print(plot)
            TRUE
        },

        # ---------------------------------------------------------------------
        # Single lower bound: Hi of each item within its scale with its
        # interval, items grouped by scale, the lower bound c dashed.
        # ES: Una sola cota: Hi de cada ítem dentro de su escala con su
        # intervalo, ítems agrupados por escala, la cota c en línea
        # discontinua.
        # ---------------------------------------------------------------------
        .plotScales = function(image, ggtheme, theme, ...) {
            state <- image$state
            if (is.null(state) || is.null(state$data) || nrow(state$data) == 0)
                return(FALSE)
            df <- state$data
            order_index <- order(df$scale, df$est)
            df$item <- factor(df$item, levels = df$item[order_index])
            style <- .mk_plot_style(self$options)
            plot <- ggplot2::ggplot(df, ggplot2::aes(x = item, y = est, color = scale)) +
                ggplot2::geom_hline(yintercept = state$lowerbound, linetype = "dashed", color = "grey45",
                                    linewidth = style$line) +
                ggplot2::geom_errorbar(ggplot2::aes(ymin = lower, ymax = upper), width = 0.25, linewidth = style$line) +
                ggplot2::geom_point(size = style$point * 1.4)
            if (style$labels)
                plot <- plot + ggplot2::geom_text(ggplot2::aes(label = formatC(est, format = "f", digits = 2)),
                                                  vjust = -0.9, size = 2.8, show.legend = FALSE)
            plot <- plot +
                ggplot2::coord_flip(ylim = if (style$full) c(min(0, min(df$lower, na.rm = TRUE)), 1) else NULL) +
                ggplot2::labs(x = NULL, y = state$labels$x, color = state$labels$color, caption = state$caption) +
                ggtheme + .mk_plot_theme(self$options)
            print(plot)
            TRUE
        },

        # ---------------------------------------------------------------------
        # Heat map: items by lower bound, colored by scale, unscalable grey.
        # ES: Mapa de calor: ítems por cota inferior, coloreados por escala,
        # los no escalables en gris.
        # ---------------------------------------------------------------------
        .plotSequence = function(image, ggtheme, theme, ...) {
            state <- image$state
            if (is.null(state) || is.null(state$data) || nrow(state$data) == 0)
                return(FALSE)
            df <- state$data
            df$scale_label <- ifelse(df$scale == 0, state$labels$unscalable, as.character(df$scale))
            levels <- c(as.character(sort(unique(df$scale[df$scale > 0]))),
                        if (any(df$scale == 0)) state$labels$unscalable)
            df$scale_label <- factor(df$scale_label, levels = levels)
            df$item <- factor(df$item, levels = rev(state$items))
            df$lb_label <- factor(formatC(df$lb, format = "f", digits = 2),
                                  levels = formatC(sort(unique(df$lb)), format = "f", digits = 2))
            style <- .mk_plot_style(self$options)
            plot <- ggplot2::ggplot(df, ggplot2::aes(x = lb_label, y = item, fill = scale_label)) +
                ggplot2::geom_tile(color = "grey80", linewidth = 0.4)
            # Unscalable cells are white with a dash, so no palette color
            # (jamovi's own includes a grey) can be mistaken for them.
            # ES: Las celdas no escalables son blancas con un guion, para que
            # ningún color de paleta (la de jamovi incluye un gris) se confunda
            # con ellas.
            if (style$labels || length(state$items) <= 30)
                plot <- plot + ggplot2::geom_text(ggplot2::aes(label = ifelse(scale == 0, "–", scale)), size = 3)
            plot <- plot +
                ggplot2::labs(x = state$labels$x, y = NULL, fill = state$labels$fill) +
                ggtheme + .mk_plot_theme(self$options)
            n_scales <- sum(levels != state$labels$unscalable)
            values <- .mk_global_palette(self$options, n_scales)
            if (any(df$scale == 0))
                values <- c(values, "white")
            plot <- plot + ggplot2::scale_fill_manual(values = values, drop = FALSE)
            print(plot)
            TRUE
        }
    )
)

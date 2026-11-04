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

        .run = function() {

            lang <- .mk_normalize_lang(self$options$reportLang)
            tr <- function(en, es) .mk_tr(lang, en, es)
            txt <- function(section, key) .mk_text(lang, section, key)

            private$.setTitles(tr)
            self$results$intro$setContent(.mk_html_block(NULL, txt("aisp", "intro")))

            if (length(self$options$vars) == 0)
                return()
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
                from <- as.numeric(self$options$lbFrom)
                to <- as.numeric(self$options$lbTo)
                step <- as.numeric(self$options$lbStep)
                if (!(from < to))
                    jmvcore::reject(txt("aisp", "bad_sequence"))
                lbs <- round(seq(from, to, by = step), 4)
            } else {
                lbs <- as.numeric(self$options$lowerbound)
            }

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
            fit <- .mk_capture(do.call(mokken::aisp, args))
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
            search_label <- if (ga) tr("Genetic algorithm", "Algoritmo genético") else
                tr("Mokken's automated item selection procedure", "Procedimiento de selección automática de Mokken")
            z_label <- switch(type_z, Z = tr("Original Z", "Z original"), WB = tr("Wald-based", "De Wald"),
                              RP = tr("Range-preserving", "Preservadora de rango"))
            rows <- list(
                list(tr("Items", "Ítems"), as.character(length(items))),
                list(tr("Cases used", "Casos usados"), .mk_cases_label(tr, prep)),
                list(tr("Search", "Búsqueda"), search_label),
                list(tr("Lower bound c", "Cota inferior c"), if (sequence)
                     sprintf(tr("%s to %s in steps of %s (%d values)", "%s a %s en pasos de %s (%d valores)"),
                             .mk_fmt_num(min(lbs), 2), .mk_fmt_num(max(lbs), 2),
                             .mk_fmt_num(as.numeric(self$options$lbStep), 2), length(lbs))
                     else .mk_fmt_num(lbs, 2)),
                list(tr("Tests during selection", "Pruebas durante la selección"), sprintf(
                    tr("%s, alpha = %s%s", "%s, alfa = %s%s"), z_label, .mk_fmt_num(self$options$alpha, 3),
                    if (isTRUE(self$options$testHi)) tr("; Hi tested against c", "; Hi contrastado contra c") else "")))
            if (ga)
                rows[[length(rows) + 1]] <- list(tr("Genetic algorithm", "Algoritmo genético"), sprintf(
                    tr("population %d, %s generations, crossover %s, mutation %s, seed %d",
                       "población %d, %s generaciones, cruce %s, mutación %s, semilla %d"),
                    as.integer(self$options$popsize),
                    if (identical(self$options$maxgensMode, "manual")) as.character(as.integer(self$options$maxgens))
                    else tr("default", "por defecto"),
                    .mk_fmt_num(self$options$pxover, 2), .mk_fmt_num(self$options$pmutation, 2),
                    as.integer(self$options$gaSeed)))
            if (length(start) > 0)
                rows[[length(rows) + 1]] <- list(tr("Start set", "Conjunto inicial"), paste(items[start], collapse = ", "))
            if (!is.null(level2))
                rows[[length(rows) + 1]] <- list(tr("Clusters", "Conglomerados"),
                    sprintf(tr("%s (%d clusters)", "%s (%d conglomerados)"), cluster_name, length(unique(level2))))
            if (length(prep$neg) > 0)
                rows[[length(rows) + 1]] <- list(tr("Negatively worded items (reversed)", "Ítems redactados en negativo (invertidos)"),
                                                 paste(prep$neg, collapse = ", "))
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
            } else {
                private$.fillSequence(partition, lbs, lang, tr, txt)
            }

            if (isTRUE(self$options$showKeySummary))
                self$results$keySummary$setContent(.mk_key_summary(lang, private$.key))

            notes <- .mk_translate_notices(warnings, lang)
            if (length(notes) > 0) {
                self$results$notes$setContent(.mk_html_block(
                    txt("common", "warnings_title"), .mk_html_list(notes), raw = TRUE))
            } else {
                self$results$notes$setVisible(FALSE)
            }
        },

        .choices = function(lang, tr, txt, ga, type_z, has_start) {
            out <- list(
                c(paste0(txt("choices", "h_search"), ": ", if (ga) tr("Genetic algorithm", "Algoritmo genético")
                         else tr("Mokken's procedure", "Procedimiento de Mokken"), "."),
                  txt("choices", if (ga) "search_ga" else "search_normal")),
                c(paste0(txt("choices", "h_lb"), ": ", if (identical(self$options$lbMode, "sequence"))
                         tr("Sequence", "Secuencia") else .mk_fmt_num(self$options$lowerbound, 2), "."),
                  txt("choices", if (identical(self$options$lbMode, "sequence")) "lb_sequence" else "lb_single")),
                c(paste0(txt("choices", "h_z"), ": ", switch(type_z, Z = tr("Original Z", "Z original"),
                         WB = tr("Wald-based", "De Wald"), RP = tr("Range-preserving", "Preservadora de rango")), "."),
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
            items <- colnames(X)
            scale_of <- as.integer(scale_of)
            names(scale_of) <- items
            scale_ids <- sort(unique(scale_of[scale_of > 0]))
            unscalable <- items[scale_of == 0]
            hi_in_scale <- stats::setNames(rep(NA_real_, length(items)), items)
            label_of <- function(s) if (s == 0) tr("Unscalable", "No escalable") else sprintf(tr("Scale %d", "Escala %d"), s)

            table <- self$results$scales
            scale_lines <- character(0)
            for (s in scale_ids) {
                members <- items[scale_of == s]
                if (!isTRUE(self$options$scaleCoefs)) next
                res <- .mk_capture(mokken::coefH(X[, members, drop = FALSE], se = TRUE, nice.output = FALSE,
                                                 results = FALSE, level.two.var = level2))
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
            items <- rownames(partition)
            table <- self$results$sequence
            for (k in seq_along(lbs))
                table$addColumn(name = paste0("c", k), title = paste0("c = ", .mk_fmt_num(lbs[k], 2)), type = "integer")
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
                    labels = list(x = tr("Lower bound c", "Cota inferior c"), fill = tr("Scale", "Escala"),
                                  unscalable = tr("Unscalable", "No escalable"))))
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

        .setTitles = function(tr) {
            titles <- list(
                c("intro", "Introduction", "Introducción"),
                c("keySummary", "Key findings", "Hallazgos clave"),
                c("choicesGuide", "About your choices", "Acerca de sus elecciones"),
                c("aispGuide", "How the automated item selection works", "Cómo funciona la selección automática de ítems"),
                c("summary", "Analysis summary", "Resumen del análisis"),
                c("membership", "Scale membership", "Pertenencia a las escalas"),
                c("scales", "Scalability of each scale", "Escalabilidad de cada escala"),
                c("sequenceGuide", "How to read the sequence of lower bounds", "Cómo leer la secuencia de cotas inferiores"),
                c("sequence", "Scale membership across lower bounds", "Pertenencia a las escalas según la cota inferior"),
                c("sequenceSummary", "Partition at each lower bound", "Partición en cada cota inferior"),
                c("sequencePlot", "Scale membership across lower bounds", "Pertenencia a las escalas según la cota inferior"),
                c("interpretation", "Applied interpretation", "Interpretación aplicada"),
                c("notes", "Notes", "Notas"))
            for (t in titles)
                tryCatch(self$results[[t[1]]]$setTitle(tr(t[2], t[3])), error = function(e) NULL)
            cols <- list(
                list("summary", "element", "Element", "Elemento"), list("summary", "value", "Value", "Valor"),
                list("membership", "item", "Item", "Ítem"), list("membership", "scale", "Scale", "Escala"),
                list("membership", "hi", "Hi in its scale", "Hi en su escala"),
                list("scales", "scale", "Scale", "Escala"), list("scales", "nItems", "Items", "Ítems"),
                list("scales", "items", "Item list", "Lista de ítems"), list("scales", "se", "SE", "EE"),
                list("scales", "lower", "Lower", "Inferior"), list("scales", "upper", "Upper", "Superior"),
                list("scales", "minHi", "Lowest Hi", "Hi más bajo"), list("scales", "strength", "Strength", "Fuerza"),
                list("sequence", "item", "Item", "Ítem"),
                list("sequenceSummary", "lb", "Lower bound", "Cota inferior"),
                list("sequenceSummary", "nScales", "Scales", "Escalas"),
                list("sequenceSummary", "nUnscalable", "Unscalable items", "Ítems no escalables"),
                list("sequenceSummary", "largest", "Items in the largest scale", "Ítems en la escala mayor"))
            for (cdef in cols)
                tryCatch(self$results[[cdef[[1]]]]$getColumn(cdef[[2]])$setTitle(tr(cdef[[3]], cdef[[4]])),
                         error = function(e) NULL)
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
            palette <- .mk_plot_series_palette(self$options$plotPalette)
            n_scales <- sum(levels != state$labels$unscalable)
            values <- if (!is.null(palette)) rep_len(palette, n_scales) else
                tryCatch(jmvcore::colorPalette(n_scales, "jmv", "fill"),
                         error = function(e) grDevices::hcl.colors(n_scales))
            if (any(df$scale == 0))
                values <- c(values, "white")
            plot <- plot + ggplot2::scale_fill_manual(values = values, drop = FALSE)
            print(plot)
            TRUE
        }
    )
)

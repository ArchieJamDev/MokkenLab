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
# Model Assumptions.
# ES: Supuestos del modelo.
#
# This file checks the assumptions of Mokken's two models:
#   - monotone homogeneity model (MHM): unidimensionality, local
#     independence, monotonicity -> check.monotonicity(), check.ca();
#   - double monotonicity model (DMM): MHM plus nonintersecting item step
#     response functions -> check.iio() (invariant item ordering),
#     check.restscore() and check.pmatrix().
# The plot methods of mokken are rebuilt in ggplot from the same matrices, so
# they follow jamovi's theme and survive export (state holds only what is
# drawn).
#
# ES: Este archivo verifica los supuestos de los dos modelos de Mokken:
#   - modelo de homogeneidad monótona (MHM): unidimensionalidad,
#     independencia local, monotonicidad -> check.monotonicity(),
#     check.ca();
#   - modelo de doble monotonicidad (DMM): MHM más funciones de respuesta de
#     los pasos de ítem que no se cruzan -> check.iio() (orden invariante),
#     check.restscore() y check.pmatrix().
# Los métodos plot de mokken se reconstruyen en ggplot a partir de las mismas
# matrices, para que sigan el tema de jamovi y sobrevivan a la exportación
# (el estado guarda solo lo que se dibuja).
#
# Workflow
#   1. Validate and prepare items (Category A)
#   2. Monotonicity, invariant item ordering, rest-score, P-matrix,
#      conditional association: each optional, each Category B on failure
#   3. Plot states
#   4. Interpretation, key findings, notes
#
# ES: Flujo
#   1. Validar y preparar los ítems (Categoría A)
#   2. Monotonicidad, orden invariante, rest-score, P-matrix, asociación
#      condicional: cada uno opcional y de Categoría B si falla
#   3. Estados de los gráficos
#   4. Interpretación, hallazgos, notas
#
# Not exposed: verbose (console progress only) and the color/ask arguments
# of the plot methods (colors follow jamovi's theme and global palette;
# jamovi shows every panel at once instead of asking).
#
# ES: No expuestos: verbose (solo progreso en consola) y los argumentos
# color/ask de los métodos plot (los colores siguen el tema y la paleta
# global de jamovi; jamovi muestra todos los paneles a la vez en lugar de
# preguntar).
# -----------------------------------------------------------------------------

msacheckClass <- if (requireNamespace("jmvcore", quietly = TRUE)) R6::R6Class(
    "msacheckClass",
    inherit = msacheckBase,
    private = list(

        .key = NULL,
        # Crit of every item in every check of this run, for the violation
        # profile plot.
        # ES: Crit de cada ítem en cada verificación de esta ejecución, para
        # el gráfico de perfil de violaciones.
        .crit = NULL,
        .addKey = function(type, text) {
            if (!is.null(private$.key) && length(text) > 0)
                private$.key[[type]] <- c(private$.key[[type]], text)
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
                self$results$intro$setContent(.mk_html_block(NULL, txt("check", "intro")))
                return()
            }
            if (isTRUE(self$options$showGuides))
                for (g in c("mono", "iio", "rest", "pmat", "ca"))
                    self$results[[paste0(g, "Guide")]]$setContent(.mk_guide_block(lang, g))
            private$.key <- list(findings = character(0), uncertain = character(0), warnings = character(0))
            private$.crit <- list()

            # -----------------------------------------------------------------
            # Validation and preparation (Category A).
            # ES: Validación y preparación (Categoría A).
            # -----------------------------------------------------------------
            cluster_name <- self$options$clusterVar
            prep <- .mk_prepare_items(self$data, self$options$vars, self$options,
                                      min_items = 3, keep = cluster_name)
            if (!isTRUE(prep$ok))
                .mk_reject_prep(lang, prep)
            X <- as.matrix(prep$data)
            items <- colnames(X)
            level2 <- .mk_level_two(prep, cluster_name)
            minsize <- if (identical(self$options$minsizeMode, "manual")) as.integer(self$options$minsize) else NULL

            plot_items <- intersect(as.character(unlist(self$options$plotItems)), items)
            if (length(plot_items) == 0)
                plot_items <- utils::head(items, 6)

            if (isTRUE(self$options$showChoicesGuide))
                self$results$choicesGuide$setContent(.mk_choices_guide(lang, c(
                    .mk_data_choices(lang, self$options, prep), private$.choices(lang, tr, txt))))

            # -----------------------------------------------------------------
            # Summary.
            # ES: Resumen.
            # -----------------------------------------------------------------
            rows <- list(
                list(.("Items"), as.character(length(items))),
                list(.("Cases used"), .mk_cases_label(tr, prep)),
                list(.("Minimum rest-score group size"),
                     if (is.null(minsize)) sprintf(.("%d (mokken default for N = %d)"),
                                                   as.integer(.mk_default_minsize(nrow(X))), nrow(X))
                     else as.character(minsize)))
            rows[[length(rows) + 1]] <- list(.("Negatively worded items (reversed)"),
                                                 .mk_reversed_label(tr, prep))
            if (!is.null(level2))
                rows[[length(rows) + 1]] <- list(.("Clusters"), sprintf(
                    .("%s (%d clusters; two-level fit statistics for monotonicity and invariant item ordering)"),
                    cluster_name, length(unique(level2))))
            for (i in seq_along(rows))
                .mk_add_row(self$results$summary, i, list(element = rows[[i]][[1]], value = rows[[i]][[2]]))

            warnings <- character(0)
            parts <- character(0)

            # -----------------------------------------------------------------
            # Each check, optional and Category B on failure.
            # ES: Cada verificación, opcional y de Categoría B si falla.
            # -----------------------------------------------------------------
            if (isTRUE(self$options$checkMono)) {
                out <- private$.runMono(X, level2, minsize, plot_items, lang, tr, txt)
                warnings <- c(warnings, out$warnings)
                parts <- c(parts, out$text)
            }
            if (isTRUE(self$options$checkIIO)) {
                out <- private$.runIIO(X, level2, minsize, plot_items, lang, tr, txt)
                warnings <- c(warnings, out$warnings)
                parts <- c(parts, out$text)
            }
            if (isTRUE(self$options$checkRest)) {
                out <- private$.runRest(X, minsize, plot_items, lang, tr, txt)
                warnings <- c(warnings, out$warnings)
                parts <- c(parts, out$text)
            }
            if (isTRUE(self$options$checkPmatrix)) {
                out <- private$.runPmatrix(X, plot_items, lang, tr, txt)
                warnings <- c(warnings, out$warnings)
                parts <- c(parts, out$text)
            }
            if (isTRUE(self$options$checkCA)) {
                out <- private$.runCA(X, lang, tr, txt)
                warnings <- c(warnings, out$warnings)
                parts <- c(parts, out$text)
            }

            # Violation profile: crit of every item in every check run.
            # ES: Perfil de violaciones: crit de cada ítem en cada
            # verificación ejecutada.
            if (isTRUE(self$options$critPlot) && length(private$.crit) > 0) {
                crit_df <- do.call(rbind, private$.crit)
                n_checks <- length(unique(crit_df$check))
                style <- .mk_plot_style(self$options, n_checks)
                .mk_set_size(self$results$critPlot, self$options, max(500, 260 * min(style$ncol, n_checks) + 80),
                             max(360, (14 * length(items) + 120) * ceiling(n_checks / style$ncol)))
                self$results$critPlot$setState(list(data = crit_df, items = items, labels = list(
                    x = "Crit", none = .("No serious violation"),
                    doubtful = .("Doubtful"), serious = .("Serious"),
                    zone = .("Zone"), na = .("Not evaluable"),
                    na_short = .("not evaluable"))))
            }

            if (length(parts) == 0)
                parts <- txt("check", "interp_nothing")
            parts <- c(parts, txt("check", "interp_next"))
            self$results$interpretation$setContent(.mk_interpretation(lang, parts))

            # Checks not run are named, so the summary never implies they
            # were passed.
            # ES: Las verificaciones no ejecutadas se nombran, para que el
            # resumen nunca dé a entender que se superaron.
            not_run <- c(mono = self$options$checkMono, iio = self$options$checkIIO, rest = self$options$checkRest,
                         pmat = self$options$checkPmatrix, ca = self$options$checkCA)
            not_run <- names(not_run)[!vapply(not_run, isTRUE, logical(1))]
            if (length(not_run) > 0)
                private$.addKey("uncertain", sprintf(txt("check", "key_not_run"), paste(vapply(not_run, function(k)
                    txt("check", paste0("section_", k)), character(1)), collapse = ", ")))
            for (section in .mk_failed_sections(self$results, lang))
                private$.addKey("warnings", sprintf(txt("common", "key_failed"), section))
            if (isTRUE(self$options$showKeySummary))
                self$results$keySummary$setContent(.mk_key_summary(lang, private$.key))

            # Reproducibility record.
            # ES: Registro de reproducibilidad.
            if (isTRUE(self$options$showRepro))
                private$.fillRepro(lang, prep, items, cluster_name, level2, minsize)

            .mk_insert_notices(self$results, self$options, .mk_translate_notices(warnings, lang))
        },

        # ---------------------------------------------------------------------
        # Reproducibility record: versions, seeds and the equivalent R code
        # of this run.
        # ES: Registro de reproducibilidad: versiones, semillas y el código R
        # equivalente de esta ejecución.
        # ---------------------------------------------------------------------
        .fillRepro = function(lang, prep, items, cluster_name, level2, minsize) {
            . <- lang$tr
            txt <- function(section, key) .mk_text(lang, section, key)
            opts <- self$options
            keep <- if (!is.null(cluster_name)) cluster_name else character(0)
            l2 <- if (!is.null(level2)) paste0("@as.integer(factor(extra[[", encodeString(cluster_name, quote = "\""), "]]))")
            code <- .mk_r_prep_code(prep, opts, items, keep)
            if (isTRUE(opts$checkMono))
                code <- c(code, .mk_r_call("check.monotonicity", "X", list(minvi = as.numeric(opts$monoMinvi),
                                                                           minsize = minsize, level.two.var = l2)))
            if (isTRUE(opts$checkIIO)) {
                order <- private$.iioOrder(length(items), txt)$order
                code <- c(code, .mk_r_call("check.iio", "X", list(method = opts$iioMethod,
                    minvi = if (identical(opts$iioMinviMode, "manual")) as.numeric(opts$iioMinvi), minsize = minsize,
                    alpha = as.numeric(opts$iioAlpha), item.selection = isTRUE(opts$iioItemSelection),
                    fixed.item.order = order, level.two.var = l2)))
            }
            if (isTRUE(opts$checkRest))
                code <- c(code, .mk_r_call("check.restscore", "X", list(minvi = as.numeric(opts$restMinvi), minsize = minsize)))
            if (isTRUE(opts$checkPmatrix))
                code <- c(code, .mk_r_call("check.pmatrix", "X", list(minvi = as.numeric(opts$pmatMinvi))))
            if (isTRUE(opts$checkCA))
                code <- c(code, .mk_r_call("check.ca", "X", list(Windex = isTRUE(opts$caWindex),
                    MINSIZE = as.integer(opts$caMinsize), NWEIGHTOPTION = opts$caNweight,
                    COVWEIGHTOPTION = opts$caCovweight, MINGROUP = as.integer(opts$caMingroup))))
            seeds <- if (!is.null(prep$completed)) sprintf(.("Two-way imputation seed: %d."),
                                                          as.integer(opts$twowaySeed)) else character(0)
            repro <- .mk_repro_block(lang, code, seeds)
            self$results$reproducibility$setContent(repro$html)
            self$results$reproducibilityCode$setContent(repro$code)
        },

        .choices = function(lang, tr, txt) {
            . <- tr
            out <- list(c(paste0(txt("choices", "h_minsize"), ": ", if (identical(self$options$minsizeMode, "manual"))
                                     as.character(self$options$minsize) else .("Automatic"), "."),
                          txt("choices", "minsize_body")),
                        c(paste0(txt("choices", "h_minvi"), "."), txt("choices", "minvi_body")))
            if (isTRUE(self$options$checkIIO)) {
                out[[length(out) + 1]] <- c(paste0(txt("choices", "h_iio_method"), ": ", self$options$iioMethod, "."),
                                            txt("choices", paste0("iio_", self$options$iioMethod)))
                out[[length(out) + 1]] <- c(paste0(txt("choices", "h_iio_order"), "."),
                                            txt("choices", paste0("order_", self$options$iioOrder)))
            }
            if (isTRUE(self$options$checkCA))
                out[[length(out) + 1]] <- c(paste0(txt("choices", "h_ca"), "."), txt("choices", "ca_body"))
            out
        },

        # ---------------------------------------------------------------------
        # Shared row filler for the violation summaries of mokken (columns
        # ItemH, #ac, #vi, #vi/#ac, maxvi, sum, sum/#ac, z/tmax, #z/tsig,
        # crit), with the verdict of Molenaar and Sijtsma (2000): crit < 40
        # no serious violation, 40-80 doubtful, > 80 serious.
        # ES: Relleno de filas común para los resúmenes de violaciones de
        # mokken, con el veredicto de Molenaar y Sijtsma (2000): crit < 40
        # sin violación seria, 40-80 dudosa, > 80 seria.
        # ---------------------------------------------------------------------
        .verdict = function(lang, vi, crit) {
            # A missing count or crit means the check could not be evaluated
            # for this item; it is never reported as an absence of violations.
            # ES: Un conteo o un crit ausente significa que la verificación no
            # pudo evaluarse para este ítem; nunca se informa como ausencia de
            # violaciones.
            if (is.na(vi)) return(.mk_text(lang, "check", "verdict_na"))
            if (vi == 0) return(.mk_text(lang, "check", "verdict_none"))
            if (is.na(crit)) return(.mk_text(lang, "check", "verdict_na"))
            if (crit < 40) return(.mk_text(lang, "check", "verdict_minor"))
            if (crit <= 80) return(.mk_text(lang, "check", "verdict_doubtful"))
            .mk_text(lang, "check", "verdict_serious")
        },

        .fillViolations = function(table, summ, lang, level_label = NULL, prefix = "", check = NULL) {
            summ <- as.matrix(summ)
            cols <- colnames(summ)
            pick <- function(i, name) {
                j <- which(cols == name)
                if (length(j) == 0) NA_real_ else .mk_clean_num(summ[i, j[1]])
            }
            # The test statistic depends on the method: z (monotonicity,
            # rest-score, P-matrix), t (MIIO) or x (IT). Its columns are found
            # by suffix and the header shows the statistic actually used.
            # ES: El estadístico depende del método: z, t (MIIO) o x (IT). Sus
            # columnas se buscan por sufijo y el encabezado muestra el
            # estadístico realmente usado.
            stat_max <- setdiff(grep("max$", cols, value = TRUE), "maxvi")[1]
            stat_sig <- grep("sig$", cols, value = TRUE)[1]
            if (!is.na(stat_max)) {
                tryCatch(table$getColumn("zmax")$setTitle(stat_max), error = function(e) NULL)
                tryCatch(table$getColumn("zsig")$setTitle(stat_sig), error = function(e) NULL)
            }
            serious <- character(0)
            doubtful <- character(0)
            not_evaluable <- character(0)
            for (i in seq_len(nrow(summ))) {
                item <- rownames(summ)[i]
                vi <- pick(i, "#vi")
                crit <- pick(i, "crit")
                values <- list(item = item, hi = pick(i, "ItemH"), ac = as.integer(pick(i, "#ac")),
                               vi = as.integer(vi), viac = pick(i, "#vi/#ac"), maxvi = pick(i, "maxvi"),
                               sum = pick(i, "sum"), sumac = pick(i, "sum/#ac"),
                               zmax = pick(i, stat_max), zsig = as.integer(pick(i, stat_sig)),
                               crit = as.integer(crit), verdict = private$.verdict(lang, vi, crit))
                if (!is.null(level_label))
                    values$level <- level_label
                .mk_add_row(table, paste0(prefix, item), values)
                if (!is.null(check))
                    private$.crit[[length(private$.crit) + 1]] <- data.frame(
                        check = if (is.null(level_label)) check else paste0(check, " (", level_label, ")"),
                        item = item, crit = if (!is.na(vi) && vi == 0 && is.na(crit)) 0 else crit,
                        stringsAsFactors = FALSE)
                if (is.na(vi) || (vi > 0 && is.na(crit))) not_evaluable <- c(not_evaluable, item)
                if (!is.na(crit) && !is.na(vi) && vi > 0 && crit > 80) serious <- c(serious, item)
                else if (!is.na(crit) && !is.na(vi) && vi > 0 && crit >= 40) doubtful <- c(doubtful, item)
            }
            list(serious = unique(serious), doubtful = unique(doubtful), not_evaluable = unique(not_evaluable))
        },

        .failRow = function(table, lang, message) {
            .mk_add_row(table, 1, list(item = .mk_text(lang, "common", "not_computable")))
            table$addFootnote(rowKey = 1, col = "item", message)
        },

        .sectionText = function(lang, txt, section, res) {
            out <- character(0)
            if (length(res$not_evaluable) > 0) {
                out <- c(out, sprintf(txt("check", "not_evaluable"), txt("check", paste0("section_", section)),
                                      length(res$not_evaluable), paste(res$not_evaluable, collapse = ", ")))
                private$.addKey("warnings", sprintf(txt("check", "key_not_evaluable"),
                                                    txt("check", paste0("section_", section)), length(res$not_evaluable)))
            }
            if (length(res$serious) == 0 && length(res$doubtful) == 0 && length(res$not_evaluable) == 0) {
                out <- txt("check", paste0(section, "_ok"))
                private$.addKey("findings", txt("check", paste0("key_", section, "_ok")))
            } else {
                if (length(res$serious) > 0) {
                    out <- c(out, sprintf(txt("check", paste0(section, "_serious")), length(res$serious),
                                          paste(res$serious, collapse = ", ")))
                    private$.addKey("warnings", sprintf(txt("check", paste0("key_", section, "_serious")),
                                                        length(res$serious), paste(res$serious, collapse = ", ")))
                }
                if (length(res$doubtful) > 0) {
                    out <- c(out, sprintf(txt("check", "doubtful"), txt("check", paste0("section_", section)),
                                          length(res$doubtful),
                                          paste(res$doubtful, collapse = ", ")))
                    private$.addKey("uncertain", sprintf(txt("check", paste0("key_", section, "_doubtful")),
                                                         length(res$doubtful)))
                }
            }
            out
        },

        # ---------------------------------------------------------------------
        # Monotonicity.
        # ES: Monotonicidad.
        # ---------------------------------------------------------------------
        .runMono = function(X, level2, minsize, plot_items, lang, tr, txt) {
            . <- tr
            table <- self$results$mono
            args <- list(X = X, minvi = as.numeric(self$options$monoMinvi), level.two.var = level2)
            if (!is.null(minsize)) args$minsize <- minsize
            fit <- .mk_cached_capture(self$results$cacheStore, "check.monotonicity", args, mokken::check.monotonicity,
                extract = function(v) {
                    summ <- .mk_capture(summary(v))$value
                    nested <- is.list(summ) && !is.matrix(summ)
                    results <- if (nested) v[[1]]$results else v$results
                    list(summary = summ, curves = if (!is.null(results)) lapply(results, function(r) r[[2]]))
                })
            if (is.null(fit$value)) {
                private$.failRow(table, lang, sprintf(txt("check", "failed"), fit$error))
                return(list(warnings = fit$warnings, text = sprintf(txt("check", "mono_failed"), fit$error)))
            }
            summ <- fit$value$summary
            if (is.list(summ) && !is.matrix(summ)) {
                res1 <- private$.fillViolations(table, summ[[1]], lang, .("Within"), "L1_",
                                                .("Monotonicity"))
                res2 <- private$.fillViolations(table, summ[[2]], lang, .("Between"), "L2_",
                                                .("Monotonicity"))
                res <- list(serious = union(res1$serious, res2$serious), doubtful = union(res1$doubtful, res2$doubtful),
                            not_evaluable = union(res1$not_evaluable, res2$not_evaluable))
                table$setNote("levels", txt("check", "levels_note"))
            } else {
                res <- private$.fillViolations(table, summ, lang, check = .("Monotonicity"))
            }
            results <- fit$value$curves
            table$setNote("crit", txt("check", "crit_note"))

            if (isTRUE(self$options$monoPlot) && !is.null(results)) {
                curves <- list()
                for (item in plot_items) {
                    k <- match(item, colnames(X))
                    m <- results[[k]]
                    curves[[item]] <- private$.curveData(m, item, self$options$monoCurves, tr)
                }
                df <- do.call(rbind, curves)
                if (!is.null(df) && nrow(df) > 0) {
                    panels <- length(plot_items) * (if (identical(self$options$monoCurves, "both")) 2 else 1)
                    private$.sizeImage(self$results$monoPlot, length(plot_items),
                                       if (identical(self$options$monoCurves, "both")) 2 else 1)
                    self$results$monoPlot$setState(list(
                        data = df, ci = isTRUE(self$options$plotCI),
                        labels = list(x = .("Rest score (group midpoint)"),
                                      y = .("Response function"),
                                      curve = .("Curve"))))
                }
            }
            list(warnings = fit$warnings, text = private$.sectionText(lang, txt, "mono", res))
        },

        # ---------------------------------------------------------------------
        # Long data for the item response function plot: the IRF (mean item
        # score) and each ISRF (P(X >= k)) by rest-score group, with normal
        # confidence bands from the group frequencies.
        # ES: Datos largos para el gráfico de funciones de respuesta: la IRF
        # (media del ítem) y cada ISRF (P(X >= k)) por grupo de rest-score,
        # con bandas normales a partir de las frecuencias del grupo.
        # ---------------------------------------------------------------------
        .curveData = function(m, item, curves, tr) {
            . <- tr
            cols <- colnames(m)
            n <- m[, "N"]
            # The x axis is the midpoint of each rest-score group on a
            # numeric scale: items form different groups, so a shared
            # categorical axis would misplace them.
            # ES: El eje x es el punto medio de cada grupo de rest-score en
            # una escala numérica: los ítems forman grupos distintos, así que
            # un eje categórico común los ubicaría mal.
            group <- (m[, 2] + m[, 3]) / 2
            z <- stats::qnorm(1 - as.numeric(self$options$plotAlpha) / 2)
            out <- list()
            fcols <- grep("^F ", cols)
            if (curves %in% c("both", "IRF") && "Mean" %in% cols) {
                scores <- as.numeric(sub("^F ", "", cols[fcols]))
                freq <- m[, fcols, drop = FALSE]
                mean_k <- m[, "Mean"]
                sd_k <- sqrt(pmax(rowSums(freq * outer(rep(1, nrow(freq)), scores)^2) / n - mean_k^2, 0))
                se <- sd_k / sqrt(n)
                out[[1]] <- data.frame(item = item, panel = .("IRF (mean item score)"),
                                       curve = "IRF", group = group, est = mean_k, lower = mean_k - z * se,
                                       upper = mean_k + z * se, ymin = min(scores), ymax = max(scores),
                                       stringsAsFactors = FALSE)
            }
            if (curves %in% c("both", "ISRF")) {
                for (j in grep("^P\\(X >=", cols)) {
                    p <- m[, j]
                    se <- sqrt(p * (1 - p) / n)
                    out[[length(out) + 1]] <- data.frame(
                        item = item, panel = .("ISRFs P(X ≥ k)"),
                        curve = sub("P\\(X >=", "X ≥ ", sub("\\)$", "", cols[j])), group = group, est = p,
                        lower = pmax(p - z * se, 0), upper = pmin(p + z * se, 1), ymin = 0, ymax = 1,
                        stringsAsFactors = FALSE)
                }
            }
            do.call(rbind, out)
        },

        .sizeImage = function(image, n_panels, n_rows = 1) {
            style <- .mk_plot_style(self$options, n_panels)
            size <- .mk_panel_size(n_panels, style$ncol, n_rows)
            .mk_set_size(image, self$options, size[1], size[2])
        },

        # ---------------------------------------------------------------------
        # Item order for check.iio(fixed.item.order =): NULL (estimated), the
        # list order, consecutive clusters of k items, or typed codes. Returns
        # list(order =, error =).
        # ES: Orden de ítems para check.iio(fixed.item.order =): NULL
        # (estimado), el orden de la lista, conglomerados consecutivos de k
        # ítems, o códigos escritos. Devuelve list(order =, error =).
        # ---------------------------------------------------------------------
        .iioOrder = function(n_items, txt) {
            mode <- self$options$iioOrder
            if (identical(mode, "listed"))
                return(list(order = seq_len(n_items)))
            if (identical(mode, "clusters")) {
                k <- as.integer(self$options$iioClusterSize)
                return(list(order = rep(seq_len(ceiling(n_items / k)), each = k)[seq_len(n_items)]))
            }
            if (identical(mode, "custom")) {
                codes <- suppressWarnings(as.integer(strsplit(trimws(self$options$iioOrderText), "[,; ]+")[[1]]))
                if (length(codes) != n_items || any(is.na(codes)))
                    return(list(error = sprintf(txt("check", "order_bad"), n_items)))
                return(list(order = codes))
            }
            list(order = NULL)
        },

        # ---------------------------------------------------------------------
        # Invariant item ordering.
        # ES: Orden invariante de los ítems.
        # ---------------------------------------------------------------------
        .runIIO = function(X, level2, minsize, plot_items, lang, tr, txt) {
            . <- tr
            table <- self$results$iio
            order <- private$.iioOrder(ncol(X), txt)
            if (!is.null(order$error)) {
                private$.failRow(table, lang, order$error)
                return(list(warnings = character(0), text = order$error))
            }
            args <- list(X = X, method = self$options$iioMethod, alpha = as.numeric(self$options$iioAlpha),
                         item.selection = isTRUE(self$options$iioItemSelection),
                         fixed.item.order = order$order, level.two.var = level2)
            if (identical(self$options$iioMinviMode, "manual")) args$minvi <- as.numeric(self$options$iioMinvi)
            if (!is.null(minsize)) args$minsize <- minsize
            fit <- .mk_cached_capture(self$results$cacheStore, "check.iio", args, mokken::check.iio,
                extract = function(obj) {
                    clustered <- !is.null(obj$Items) && !is.null(obj$Clusters)
                    nested <- !is.null(obj$Level.1)
                    base <- if (clustered) obj$Items else if (nested) obj$Level.1 else obj
                    list(summary = .mk_capture(summary(obj))$value, clustered = clustered, nested = nested,
                         removed = base$items.removed,
                         pairs = lapply(base$results, function(p) list(p[[1]], p[[2]])))
                })
            if (is.null(fit$value)) {
                private$.failRow(table, lang, sprintf(txt("check", "failed"), fit$error))
                return(list(warnings = fit$warnings, text = sprintf(txt("check", "iio_failed"), fit$error)))
            }
            summ <- fit$value$summary
            clustered <- fit$value$clustered
            nested <- fit$value$nested

            # Removal step of each item in the backward selection.
            # ES: Paso de eliminación de cada ítem en la selección hacia
            # atrás.
            removed <- fit$value$removed
            removed_at <- stats::setNames(rep("", ncol(X)), colnames(X))
            if (length(removed) > 0)
                removed_at[names(removed)] <- sprintf(.("Step %d"), seq_along(removed))
            means <- colMeans(X)

            fill <- function(item_summary, level_label = NULL, prefix = "") {
                res <- private$.fillViolations(table, item_summary, lang, level_label, prefix,
                                               .("Invariant item ordering"))
                for (item in rownames(item_summary))
                    table$setRow(rowKey = paste0(prefix, item), values = list(
                        mean = .mk_clean_num(means[[item]]), removed = removed_at[[item]]))
                res
            }
            if (nested) {
                r1 <- fill(summ$item.summary[[1]], .("Within"), "L1_")
                r2 <- fill(summ$item.summary[[2]], .("Between"), "L2_")
                res <- list(serious = union(r1$serious, r2$serious), doubtful = union(r1$doubtful, r2$doubtful),
                            not_evaluable = union(r1$not_evaluable, r2$not_evaluable))
                table$setNote("levels", txt("check", "levels_note"))
            } else if (clustered) {
                item_summ <- summ$item.summary$Items
                res <- fill(item_summ)
                ctab <- self$results$iioClusters
                ctab$setVisible(TRUE)
                cs <- as.matrix(summ$item.summary$Clusters)
                for (i in seq_len(nrow(cs))) {
                    vi <- .mk_clean_num(cs[i, "#vi"]); crit <- .mk_clean_num(cs[i, "crit"])
                    zc <- setdiff(grep("max$", colnames(cs), value = TRUE), "maxvi")[1]
                    zs <- grep("sig$", colnames(cs), value = TRUE)[1]
                    code <- suppressWarnings(as.integer(rownames(cs)[i]))
                    members <- if (!is.na(code) && !is.null(order$order)) colnames(X)[order$order == code] else character(0)
                    label <- if (length(members) > 0) sprintf("%s (%s)", rownames(cs)[i], paste(members, collapse = ", "))
                             else rownames(cs)[i]
                    .mk_add_row(ctab, i, list(cluster = label, ac = as.integer(cs[i, "#ac"]),
                        vi = as.integer(vi), viac = .mk_clean_num(cs[i, "#vi/#ac"]), maxvi = .mk_clean_num(cs[i, "maxvi"]),
                        sum = .mk_clean_num(cs[i, "sum"]), sumac = .mk_clean_num(cs[i, "sum/#ac"]),
                        zmax = .mk_clean_num(cs[i, zc]), zsig = as.integer(cs[i, zs]), crit = as.integer(crit),
                        verdict = private$.verdict(lang, vi, crit)))
                }
                ctab$setNote("clusters", txt("check", "clusters_note"))
            } else {
                res <- fill(summ$item.summary)
            }
            table$setNote("crit", txt("check", "crit_note"))
            if (!is.null(order$order))
                table$setNote("order", sprintf(txt("check", "order_note"), paste(order$order, collapse = ", ")))

            # HT (and, for clusters, HTB and their ratio).
            # ES: HT (y, con conglomerados, HTB y su razón).
            ht_table <- self$results$iioHT
            ht <- summ$HT
            ht_value <- NA_real_
            if (is.matrix(ht) && ncol(ht) >= 3) {
                ht_value <- .mk_clean_num(ht[1, 1])
                .mk_add_row(ht_table, "HT", list(stat = "HT", value = ht_value, meaning = private$.htLabel(lang, ht_value)))
                .mk_add_row(ht_table, "HTB", list(stat = "HTB", value = .mk_clean_num(ht[1, 2]),
                                                  meaning = txt("check", "htb_meaning")))
                .mk_add_row(ht_table, "ratio", list(stat = "HTB/HT", value = .mk_clean_num(ht[1, 3]),
                                                    meaning = txt("check", "htratio_meaning")))
            } else {
                ht_value <- .mk_clean_num(ht)
                .mk_add_row(ht_table, "HT", list(stat = "HT", value = ht_value, meaning = private$.htLabel(lang, ht_value)))
            }
            ht_table$setNote("ht", txt("check", "ht_note"))

            # Pair plot from the pairwise results.
            # ES: Gráfico de pares a partir de los resultados por pares.
            if (isTRUE(self$options$iioPlot)) {
                pairs <- fit$value$pairs
                df <- private$.pairData(pairs, plot_items, function(p) unlist(p[[1]]), tr)
                if (!is.null(df) && nrow(df) > 0) {
                    private$.sizeImage(self$results$iioPlot, length(unique(df$pair)))
                    self$results$iioPlot$setState(list(data = df, ci = isTRUE(self$options$plotCI),
                        labels = list(x = .("Rest score (group midpoint)"),
                                      y = .("Mean item score"), item = .("Item"))))
                }
            }

            text <- private$.sectionText(lang, txt, "iio", res)
            if (length(removed) > 0) {
                text <- c(text, sprintf(txt("check", "iio_removed"), length(removed), paste(names(removed), collapse = ", ")))
                private$.addKey("warnings", sprintf(txt("check", "key_iio_removed"), length(removed)))
            }
            if (!is.na(ht_value)) {
                text <- c(text, sprintf(txt("check", "iio_ht"), .mk_fmt_num(ht_value, 3), private$.htLabel(lang, ht_value)))
                private$.addKey(if (ht_value >= 0.3) "findings" else "uncertain",
                                sprintf(txt("check", "key_ht"), .mk_fmt_num(ht_value, 2), private$.htLabel(lang, ht_value)))
            }
            list(warnings = fit$warnings, text = text)
        },

        .htLabel = function(lang, ht) {
            if (is.na(ht)) return(.mk_text(lang, "common", "not_computable"))
            key <- if (ht < 0.3) "ht_low" else if (ht < 0.4) "ht_weak" else if (ht < 0.5) "ht_medium" else "ht_high"
            .mk_text(lang, "check", key)
        },

        # ---------------------------------------------------------------------
        # Long data for the pair plots (invariant item ordering and rest-score
        # method): the mean score of both items by rest-score group, for
        # every pair of plotted items. Standard errors come from SD columns
        # or from the cumulative probabilities P(X >= k); without either
        # (method IT) the band is omitted.
        # ES: Datos largos para los gráficos de pares: la media de ambos
        # ítems por grupo de rest-score, para cada par de ítems graficados.
        # Los errores estándar salen de las columnas SD o de las
        # probabilidades acumuladas P(X >= k); sin ninguna (método IT) se
        # omite la banda.
        # ---------------------------------------------------------------------
        .pairData = function(pairs, plot_items, pair_names, tr, matrix_of = function(p) p[[2]]) {
            z <- stats::qnorm(1 - as.numeric(self$options$plotAlpha) / 2)
            out <- list()
            for (p in pairs) {
                nm <- pair_names(p)
                if (length(nm) < 2 || !all(nm[1:2] %in% plot_items)) next
                m <- matrix_of(p)
                cols <- colnames(m)
                # mokken names the columns after the items' positions in the
                # data (E(X3), SD(X3), P(X3>=1), ...), so they are located by
                # position, not by a fixed name.
                # ES: mokken nombra las columnas según la posición de los
                # ítems en los datos (E(X3), SD(X3), P(X3>=1), ...), así que
                # se ubican por posición, no por un nombre fijo.
                e_cols <- grep("^E\\(X[0-9]+\\)$", cols)
                if (length(e_cols) < 2 || !("N" %in% cols)) next
                idx <- sub("^E\\(X([0-9]+)\\)$", "\\1", cols[e_cols[1:2]])
                lo <- if ("Lo" %in% cols) m[, "Lo"] else m[, 2]
                hi <- if ("Hi" %in% cols) m[, "Hi"] else m[, 3]
                group <- (lo + hi) / 2
                n <- m[, "N"]
                se_of <- function(w) {
                    sd_col <- paste0("SD(X", idx[w], ")")
                    if (sd_col %in% cols) return(m[, sd_col] / sqrt(n))
                    pc <- grep(paste0("^P\\(X", idx[w], ">="), cols)
                    if (length(pc) == 0) return(rep(NA_real_, nrow(m)))
                    cum <- cbind(1, m[, pc, drop = FALSE], 0)
                    probs <- cum[, -ncol(cum), drop = FALSE] - cum[, -1, drop = FALSE]
                    scores <- 0:(ncol(probs) - 1)
                    mu <- as.numeric(probs %*% scores)
                    sqrt(pmax(as.numeric(probs %*% scores^2) - mu^2, 0)) / sqrt(n)
                }
                pair <- paste(nm[1], "–", nm[2])
                for (w in 1:2) {
                    est <- m[, e_cols[w]]
                    se <- se_of(w)
                    out[[length(out) + 1]] <- data.frame(pair = pair, item = nm[w], group = group, est = est,
                                                         lower = est - z * se, upper = est + z * se,
                                                         stringsAsFactors = FALSE)
                }
            }
            if (length(out) == 0) return(NULL)
            do.call(rbind, out)
        },

        # ---------------------------------------------------------------------
        # Rest-score method.
        # ES: Método rest-score.
        # ---------------------------------------------------------------------
        .runRest = function(X, minsize, plot_items, lang, tr, txt) {
            . <- tr
            table <- self$results$rest
            args <- list(X = X, minvi = as.numeric(self$options$restMinvi))
            if (!is.null(minsize)) args$minsize <- minsize
            fit <- .mk_cached_capture(self$results$cacheStore, "check.restscore", args, mokken::check.restscore,
                extract = function(v) list(summary = .mk_capture(summary(v))$value,
                    results = lapply(v$results, function(p) list(p[[1]][c("FIRST.ITEM", "SECOND.ITEM")],
                                                                  SUMMARY.MATRIX = p$SUMMARY.MATRIX))))
            if (is.null(fit$value)) {
                private$.failRow(table, lang, sprintf(txt("check", "failed"), fit$error))
                return(list(warnings = fit$warnings, text = sprintf(txt("check", "rest_failed"), fit$error)))
            }
            res <- private$.fillViolations(table, fit$value$summary, lang,
                                           check = .("Rest-score"))
            table$setNote("crit", txt("check", "crit_note"))
            if (isTRUE(self$options$restPlot)) {
                df <- private$.pairData(fit$value$results, plot_items,
                                        function(p) c(p[[1]]$FIRST.ITEM, p[[1]]$SECOND.ITEM), tr,
                                        matrix_of = function(p) p$SUMMARY.MATRIX)
                if (!is.null(df) && nrow(df) > 0) {
                    private$.sizeImage(self$results$restPlot, length(unique(df$pair)))
                    self$results$restPlot$setState(list(data = df, ci = isTRUE(self$options$plotCI),
                        labels = list(x = .("Rest score (group midpoint)"),
                                      y = .("Mean item score"), item = .("Item"))))
                }
            }
            list(warnings = fit$warnings, text = private$.sectionText(lang, txt, "rest", res))
        },

        # ---------------------------------------------------------------------
        # P-matrix method.
        # ES: Método P-matrix.
        # ---------------------------------------------------------------------
        .runPmatrix = function(X, plot_items, lang, tr, txt) {
            . <- tr
            table <- self$results$pmat
            fit <- .mk_cached_capture(self$results$cacheStore, "check.pmatrix", list(X, minvi = as.numeric(self$options$pmatMinvi)), mokken::check.pmatrix,
                extract = function(v) list(summary = .mk_capture(summary(v))$value, N = v$N, I.item = v$I.item,
                                           I.step = v$I.step, results = v$results[c("Ppp", "Pmm")]))
            if (is.null(fit$value)) {
                private$.failRow(table, lang, sprintf(txt("check", "failed"), fit$error))
                return(list(warnings = fit$warnings, text = sprintf(txt("check", "pmat_failed"), fit$error)))
            }
            res <- private$.fillViolations(table, fit$value$summary, lang,
                                           check = "P-matrix")
            table$setNote("crit", txt("check", "crit_note"))

            if (isTRUE(self$options$pmatPlot)) {
                pm <- fit$value
                n <- pm$N
                z <- stats::qnorm(1 - as.numeric(self$options$plotAlpha) / 2)
                which <- switch(self$options$pmatType, both = c("Ppp", "Pmm"), ppp = "Ppp", pmm = "Pmm")
                out <- list()
                for (item in plot_items) {
                    k <- match(item, colnames(X))
                    rows_k <- which(pm$I.item == k)
                    for (mat in which) {
                        P <- pm$results[[mat]]
                        for (r in rows_k) {
                            vals <- P[r, ]
                            keep <- !is.na(vals)
                            if (!any(keep)) next
                            se <- sqrt(vals[keep] * (1 - vals[keep]) / n)
                            out[[length(out) + 1]] <- data.frame(
                                item = item, matrix = if (mat == "Ppp") "P(++)" else "P(--)",
                                step = sub(paste0("^X", k), item, pm$I.step[r]),
                                x = which(keep), est = vals[keep], lower = pmax(vals[keep] - z * se, 0),
                                upper = pmin(vals[keep] + z * se, 1), stringsAsFactors = FALSE)
                        }
                    }
                }
                if (length(out) > 0) {
                    df <- do.call(rbind, out)
                    private$.sizeImage(self$results$pmatPlot, length(plot_items), length(which))
                    self$results$pmatPlot$setState(list(data = df, ci = isTRUE(self$options$plotCI),
                        labels = list(x = .("Other item steps, from most to least popular"),
                                      y = .("Joint proportion"),
                                      step = .("Item step"))))
                }
            }
            list(warnings = fit$warnings, text = private$.sectionText(lang, txt, "pmat", res))
        },

        # ---------------------------------------------------------------------
        # Conditional association (Straat et al., 2016): the iterative
        # removal of the worst locally dependent item, and optionally the W
        # indices of the first iteration.
        # ES: Asociación condicional (Straat et al., 2016): la eliminación
        # iterativa del ítem con peor dependencia local y, opcionalmente, los
        # índices W de la primera iteración.
        # ---------------------------------------------------------------------
        .runCA = function(X, lang, tr, txt) {
            . <- tr
            table <- self$results$ca
            fit <- .mk_cached_capture(self$results$cacheStore, "check.ca", list(X, Windex = isTRUE(self$options$caWindex),
                MINSIZE = as.integer(self$options$caMinsize), NWEIGHTOPTION = self$options$caNweight,
                COVWEIGHTOPTION = self$options$caCovweight, MINGROUP = as.integer(self$options$caMingroup)), mokken::check.ca,
                extract = function(v) {
                    # check.ca() returns list(InScale =, Index =, Flagged =)
                    # when Windex = TRUE, but the bare list of InScale
                    # iterations when Windex = FALSE. Both are valid;
                    # normalize before keeping the pieces.
                    # ES: check.ca() devuelve list(InScale =, Index =,
                    # Flagged =) con Windex = TRUE, pero solo la lista de
                    # iteraciones de InScale con Windex = FALSE. Ambas son
                    # válidas; se normalizan antes de guardar las piezas.
                    if (is.null(v$InScale) && is.list(v) && length(v) > 0 && all(vapply(v, is.logical, logical(1))))
                        v <- list(InScale = v)
                    .mk_keep(c("InScale", "Index", "Flagged"))(v)
                })
            if (is.null(fit$value) || is.null(fit$value$InScale)) {
                private$.failRow(table, lang, sprintf(txt("check", "failed"),
                                                      if (is.null(fit$error)) "" else fit$error))
                return(list(warnings = fit$warnings, text = sprintf(txt("check", "ca_failed"),
                                                                    if (is.null(fit$error)) "" else fit$error)))
            }
            inscale <- fit$value$InScale
            items <- colnames(X)
            steps <- length(inscale)
            for (s in seq_len(steps))
                table$addColumn(name = paste0("s", s), title = sprintf(.("Step %d"), s), type = "text")
            table$addColumn(name = "final", title = .("Final set"), type = "text")
            final <- inscale[[steps]]
            for (i in seq_along(items)) {
                values <- list(item = items[i])
                for (s in seq_len(steps))
                    values[[paste0("s", s)]] <- if (isTRUE(inscale[[s]][i])) .("In") else "—"
                values$final <- if (isTRUE(final[i])) .("Kept") else .("Removed")
                .mk_add_row(table, items[i], values)
            }
            table$setNote("ca", txt("check", "ca_note"))

            if (isTRUE(self$options$caWindex) && !is.null(fit$value$Index)) {
                idx <- fit$value$Index[[1]]
                flags <- fit$value$Flagged[[1]]
                wtab <- self$results$caW
                for (i in seq_along(items)) {
                    w1 <- suppressWarnings(max(idx$W1[i, ], na.rm = TRUE))
                    w3 <- suppressWarnings(max(idx$W3[i, ], na.rm = TRUE))
                    .mk_add_row(wtab, items[i], list(
                        item = items[i], w1 = .mk_clean_num(w1), w1flag = as.integer(sum(flags$F1[i, ], na.rm = TRUE)),
                        w2 = .mk_clean_num(idx$W2[1, i]),
                        w2flag = if (isTRUE(flags$F2[1, i] == 1)) .("Flagged") else "—",
                        w3 = .mk_clean_num(w3), w3flag = as.integer(sum(flags$F3[i, ], na.rm = TRUE))))
                }
                wtab$setNote("w", txt("check", "w_note"))
            }

            removed <- items[!final]
            text <- if (length(removed) == 0) {
                private$.addKey("findings", txt("check", "key_ca_ok"))
                txt("check", "ca_ok")
            } else {
                private$.addKey("warnings", sprintf(txt("check", "key_ca_removed"), length(removed),
                                                    paste(removed, collapse = ", ")))
                sprintf(txt("check", "ca_removed"), length(removed), paste(removed, collapse = ", "))
            }
            list(warnings = fit$warnings, text = text)
        },


        # ---------------------------------------------------------------------
        # Plots. Bands use the opacity option; colors follow jamovi's theme
        # and global palette through ggtheme (CODE_STYLE §18).
        # ES: Gráficos. Las bandas usan la opción de opacidad; los colores
        # siguen el tema y la paleta global de jamovi a través de ggtheme.
        # ---------------------------------------------------------------------
        # Item response functions: one panel per item and curve type (IRF on
        # the item-score scale, ISRFs on the probability scale).
        # ES: Funciones de respuesta: un panel por ítem y tipo de curva (IRF
        # en la escala del ítem, ISRF en la escala de probabilidad).
        .plotMono = function(image, ggtheme, theme, ...) {
            state <- image$state
            if (is.null(state) || is.null(state$data) || nrow(state$data) == 0)
                return(FALSE)
            df <- state$data
            df$facet <- paste0(df$item, " · ", df$panel)
            df$facet <- factor(df$facet, levels = unique(df$facet))
            style <- .mk_plot_style(self$options, length(levels(df$facet)))
            opacity <- as.numeric(self$options$plotTransparency) / 100
            plot <- ggplot2::ggplot(df, ggplot2::aes(x = group, y = est, color = curve, group = curve))
            if (isTRUE(state$ci) && any(is.finite(df$lower)))
                plot <- plot + ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper, fill = curve),
                                                    alpha = opacity, color = NA, show.legend = FALSE)
            plot <- plot +
                ggplot2::geom_line(linewidth = style$line) +
                ggplot2::geom_point(size = style$point)
            if (style$labels)
                plot <- plot + ggplot2::geom_text(ggplot2::aes(label = formatC(est, format = "f", digits = 2)),
                                                  vjust = -0.8, size = 2.6, show.legend = FALSE)
            # Full range: 0 to 1 for the ISRFs, lowest to highest item score
            # for the IRF, drawn with invisible points so each panel keeps
            # its own scale.
            # ES: Rango completo: 0 a 1 para las ISRF, del puntaje mínimo al
            # máximo para la IRF, con puntos invisibles para que cada panel
            # conserve su propia escala.
            if (style$full && all(c("ymin", "ymax") %in% names(df)))
                plot <- plot + ggplot2::geom_blank(ggplot2::aes(y = ymin)) + ggplot2::geom_blank(ggplot2::aes(y = ymax))
            plot <- plot +
                ggplot2::facet_wrap(~ facet, scales = "free", ncol = style$ncol) +
                ggplot2::labs(x = state$labels$x, y = state$labels$y, color = state$labels$curve) +
                ggtheme + .mk_plot_theme(self$options)
            print(plot)
            TRUE
        },

        # Item pairs: the mean score of both items by rest-score group.
        # ES: Pares de ítems: la media de ambos ítems por grupo de
        # rest-score.
        .plotPairs = function(image, ggtheme, theme, ...) {
            state <- image$state
            if (is.null(state) || is.null(state$data) || nrow(state$data) == 0)
                return(FALSE)
            df <- state$data
            df$pair <- factor(df$pair, levels = unique(df$pair))
            df$slot <- ifelse(df$item == sub(" – .*$", "", as.character(df$pair)), "first", "second")
            df$label <- df$item
            style <- .mk_plot_style(self$options, length(levels(df$pair)))
            opacity <- as.numeric(self$options$plotTransparency) / 100
            plot <- ggplot2::ggplot(df, ggplot2::aes(x = group, y = est, color = slot, group = slot))
            if (isTRUE(state$ci) && any(is.finite(df$lower)))
                plot <- plot + ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper, fill = slot),
                                                    alpha = opacity, color = NA, show.legend = FALSE)
            plot <- plot +
                ggplot2::geom_line(linewidth = style$line) + ggplot2::geom_point(size = style$point)
            if (style$labels)
                plot <- plot + ggplot2::geom_text(ggplot2::aes(label = formatC(est, format = "f", digits = 2)),
                                                  vjust = -0.8, size = 2.6, show.legend = FALSE)
            # The last point of each curve carries the item name, so the two
            # curves of every panel are identified without a shared legend.
            # ES: El último punto de cada curva lleva el nombre del ítem,
            # para identificar las dos curvas de cada panel sin una leyenda
            # común.
            ends <- do.call(rbind, lapply(split(df, list(df$pair, df$slot), drop = TRUE),
                                          function(d) d[nrow(d), , drop = FALSE]))
            plot <- plot +
                ggplot2::geom_text(data = ends, ggplot2::aes(label = label), hjust = -0.15, size = 3,
                                   show.legend = FALSE) +
                ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0.05, 0.25))) +
                ggplot2::facet_wrap(~ pair, ncol = style$ncol, scales = "free_x") +
                ggplot2::labs(x = state$labels$x, y = state$labels$y, color = NULL) +
                ggtheme + .mk_plot_theme(self$options) + ggplot2::theme(legend.position = "none")
            print(plot)
            TRUE
        },

        # P-matrix: one line per item step across the other item steps,
        # ordered from the most to the least popular.
        # ES: P-matrix: una línea por paso de ítem a lo largo de los otros
        # pasos, ordenados del más al menos popular.
        .plotPmatrix = function(image, ggtheme, theme, ...) {
            state <- image$state
            if (is.null(state) || is.null(state$data) || nrow(state$data) == 0)
                return(FALSE)
            df <- state$data
            df$facet <- factor(paste0(df$matrix, " · ", df$item), levels = unique(paste0(df$matrix, " · ", df$item)))
            style <- .mk_plot_style(self$options, length(levels(df$facet)))
            opacity <- as.numeric(self$options$plotTransparency) / 100
            plot <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = est, color = step, group = step))
            if (isTRUE(state$ci))
                plot <- plot + ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper, fill = step),
                                                    alpha = opacity, color = NA, show.legend = FALSE)
            plot <- plot +
                ggplot2::geom_line(linewidth = style$line) + ggplot2::geom_point(size = style$point * 0.6) +
                ggplot2::facet_wrap(~ facet, ncol = style$ncol, scales = if (style$full) "fixed" else "free_y") +
                ggplot2::labs(x = state$labels$x, y = state$labels$y, color = state$labels$step) +
                ggtheme + .mk_plot_theme(self$options)
            if (style$full)
                plot <- plot + ggplot2::coord_cartesian(ylim = c(0, 1))
            print(plot)
            TRUE
        },

        # Violation profile: crit of each item in each check, with the
        # thresholds of Molenaar and Sijtsma (2000) at 40 and 80 shaded.
        # ES: Perfil de violaciones: crit de cada ítem en cada verificación,
        # con los umbrales de Molenaar y Sijtsma (2000) en 40 y 80
        # sombreados.
        .plotCrit = function(image, ggtheme, theme, ...) {
            state <- image$state
            if (is.null(state) || is.null(state$data) || nrow(state$data) == 0)
                return(FALSE)
            df <- state$data
            df$item <- factor(df$item, levels = rev(state$items))
            df$check <- factor(df$check, levels = unique(df$check))
            missing <- is.na(df$crit)
            df$zone <- factor(ifelse(missing, state$labels$na, ifelse(df$crit > 80, state$labels$serious,
                              ifelse(df$crit >= 40, state$labels$doubtful, state$labels$none))),
                              levels = c(state$labels$none, state$labels$doubtful, state$labels$serious, state$labels$na))
            style <- .mk_plot_style(self$options, length(levels(df$check)))
            top <- max(100, suppressWarnings(max(df$crit, na.rm = TRUE)) * 1.1, na.rm = TRUE)
            plot <- ggplot2::ggplot(df, ggplot2::aes(x = item, y = crit, fill = zone)) +
                ggplot2::annotate("rect", xmin = -Inf, xmax = Inf, ymin = 40, ymax = 80, alpha = 0.08, fill = "orange") +
                ggplot2::annotate("rect", xmin = -Inf, xmax = Inf, ymin = 80, ymax = Inf, alpha = 0.08, fill = "red") +
                ggplot2::geom_col(data = df[!missing, , drop = FALSE], width = 0.7) +
                ggplot2::geom_text(data = df[missing, , drop = FALSE], ggplot2::aes(y = 0, label = state$labels$na_short),
                                   hjust = 0, size = 2.8, color = "grey35", show.legend = FALSE) +
                ggplot2::geom_hline(yintercept = c(40, 80), linetype = "dashed", linewidth = style$line * 0.6)
            if (style$labels)
                plot <- plot + ggplot2::geom_text(ggplot2::aes(label = crit), hjust = -0.2, size = 2.8)
            plot <- plot +
                ggplot2::coord_flip(ylim = c(0, top)) +
                ggplot2::facet_wrap(~ check, ncol = style$ncol) +
                ggplot2::labs(x = NULL, y = state$labels$x, fill = state$labels$zone) +
                ggtheme + .mk_plot_theme(self$options)
            values <- c("#4C9F70", "#E69F00", "#C0392B", "grey70")
            plot <- plot + ggplot2::scale_fill_manual(values = values, drop = FALSE)
            print(plot)
            TRUE
        }
    )
)

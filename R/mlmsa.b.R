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
# Two-Level Mokken Scale Analysis.
# ES: Análisis de escalamiento de Mokken de dos niveles.
#
# Raters (rows) are nested in subjects (the persons or objects they rate,
# for example teachers rated by their students). The scale is meant to order
# subjects, so scalability is split into a within-rater part (HW: how
# consistently each rater's item scores follow the item ordering), a
# between-rater part (HB: how consistently the item scores of different
# raters of the same subject follow it) and their ratio BW (Snijders, 2001;
# Crisan et al., 2016). This file wraps mokken::MLcoefH, MLcoefZ, ICC and
# MLweight.
#
# ES: Los evaluadores (filas) están anidados en sujetos (las personas u
# objetos que evalúan, por ejemplo docentes evaluados por sus alumnos). La
# escala busca ordenar a los sujetos, así que la escalabilidad se divide en
# una parte dentro de evaluadores (HW), una entre evaluadores (HB) y su razón
# BW (Snijders, 2001; Crisan et al., 2016). Este archivo envuelve
# mokken::MLcoefH, MLcoefZ, ICC y MLweight.
#
# Workflow
#   1. Validate and prepare items and subject (Category A)
#   2. MLcoefH with standard errors and intervals; MLcoefZ
#   3. Item pairs, covariance matrices, ICC, weights (Category B)
#   4. Plot state, interpretation, key findings, notes
#
# ES: Flujo
#   1. Validar y preparar ítems y sujeto (Categoría A)
#   2. MLcoefH con errores estándar e intervalos; MLcoefZ
#   3. Pares, matrices de covarianza, ICC, pesos (Categoría B)
#   4. Estado del gráfico, interpretación, hallazgos, notas
#
# Not exposed: MLcoefH(fixed.itemstep.order =) and MLweight(itemstep.order =),
# which take a matrix of item-step ranks that cannot be typed in jamovi's
# option panel; mokken then estimates the order from the data, its default.
#
# ES: No expuestos: MLcoefH(fixed.itemstep.order =) y
# MLweight(itemstep.order =), que reciben una matriz de rangos de pasos de
# ítem que no puede escribirse en el panel de opciones de jamovi; mokken
# estima entonces el orden a partir de los datos, su valor por defecto.
# -----------------------------------------------------------------------------

mlmsaClass <- if (requireNamespace("jmvcore", quietly = TRUE)) R6::R6Class(
    "mlmsaClass",
    inherit = mlmsaBase,
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
            self$results$intro$setContent(.mk_html_block(NULL, txt("ml", "intro")))

            if (length(self$options$vars) == 0 || is.null(self$options$subject))
                return()
            if (isTRUE(self$options$showGuides))
                self$results$mlGuide$setContent(.mk_guide_block(lang, "ml"))
            private$.key <- list(findings = character(0), uncertain = character(0), warnings = character(0))

            # -----------------------------------------------------------------
            # Validation and preparation (Category A).
            # ES: Validación y preparación (Categoría A).
            # -----------------------------------------------------------------
            subject_name <- self$options$subject
            prep <- .mk_prepare_items(self$data, self$options$vars, self$options, min_items = 2, keep = subject_name)
            if (!isTRUE(prep$ok))
                .mk_reject_prep(lang, prep)
            subject <- .mk_level_two(prep, subject_name)
            raters <- table(subject)
            if (length(raters) < 2)
                jmvcore::reject(txt("ml", "too_few_subjects"))
            X <- as.matrix(prep$data)
            items <- colnames(X)
            XS <- cbind(Subject = subject, X)
            level <- as.numeric(self$options$ciWidth) / 100

            if (isTRUE(self$options$showChoicesGuide))
                self$results$choicesGuide$setContent(.mk_choices_guide(lang, c(
                    .mk_data_choices(lang, self$options, prep),
                    list(c(paste0(txt("choices", "h_weigh"), ": ", if (isTRUE(self$options$weighProps))
                               tr("Weighted", "Ponderadas") else tr("Averaged", "Promediadas"), "."),
                           txt("choices", if (isTRUE(self$options$weighProps)) "weigh_true" else "weigh_false"))),
                    if (isTRUE(self$options$ci)) list(c(
                        paste0(txt("choices", "h_ci"), ": ", if (identical(self$options$typeCI, "RP"))
                            tr("Range-preserving", "Preservador de rango") else tr("Wald-based", "De Wald"), "."),
                        txt("choices", paste0("ci_", self$options$typeCI)))))))

            # -----------------------------------------------------------------
            # Two-level coefficients (Category A if MLcoefH() fails).
            # ES: Coeficientes de dos niveles (Categoría A si MLcoefH()
            # falla).
            # -----------------------------------------------------------------
            args <- list(X = XS, se = TRUE, nice.output = FALSE, subject = 1,
                         weigh.props = isTRUE(self$options$weighProps), type.ci = self$options$typeCI,
                         cov.mat = isTRUE(self$options$showCov))
            if (isTRUE(self$options$ci)) args$ci <- level
            fit <- .mk_capture(do.call(mokken::MLcoefH, args))
            if (is.null(fit$value))
                jmvcore::reject(sprintf(txt("ml", "failed"), fit$error))
            warnings <- fit$warnings
            H <- fit$value

            zfit <- NULL
            if (isTRUE(self$options$showZ)) {
                zres <- .mk_capture(mokken::MLcoefZ(XS, lowerbound = as.numeric(self$options$zLowerbound),
                                                    type.z = self$options$typeZ))
                warnings <- c(warnings, zres$warnings)
                zfit <- zres$value
                if (is.null(zfit))
                    private$.addKey("warnings", sprintf(txt("ml", "key_z_failed"), zres$error))
            }
            pz <- function(z) { z <- .mk_clean_num(z); if (is.na(z)) NA_real_ else stats::pnorm(z, lower.tail = FALSE) }

            # -----------------------------------------------------------------
            # Summary.
            # ES: Resumen.
            # -----------------------------------------------------------------
            rows <- list(
                list(tr("Items", "Ítems"), as.character(length(items))),
                list(tr("Raters (rows) used", "Evaluadores (filas) usados"), .mk_cases_label(tr, prep)),
                list(tr("Subjects", "Sujetos"), sprintf(tr("%d (%s)", "%d (%s)"), length(raters), subject_name)),
                list(tr("Raters per subject", "Evaluadores por sujeto"), sprintf(
                    tr("minimum %d, median %s, maximum %d", "mínimo %d, mediana %s, máximo %d"),
                    as.integer(min(raters)), .mk_fmt_num(stats::median(raters), 1), as.integer(max(raters)))),
                list(tr("Group proportions", "Proporciones de grupo"), if (isTRUE(self$options$weighProps))
                    tr("Weighted by group size", "Ponderadas por tamaño de grupo") else tr("Averaged", "Promediadas")))
            rows[[length(rows) + 1]] <- list(tr("Negatively worded items (reversed)", "Ítems redactados en negativo (invertidos)"),
                                                 .mk_reversed_label(tr, prep))
            for (i in seq_along(rows))
                .mk_add_row(self$results$summary, i, list(element = rows[[i]][[1]], value = rows[[i]][[2]]))
            if (any(raters < 2))
                self$results$summary$setNote("single", sprintf(txt("ml", "single_rater"), sum(raters < 2)))
            # The design is what makes the coefficients interpretable: the
            # subject variable must identify who or what is rated by several
            # raters. A grouping such as sex runs but answers no two-level
            # question (independent audit, 2026-11-04), so the requirement is
            # always stated, and very few subjects are flagged.
            # ES: El diseño es lo que hace interpretables los coeficientes: la
            # variable de sujeto debe identificar a quién o qué evalúan varios
            # evaluadores. Una agrupación como el sexo se ejecuta pero no
            # responde ninguna pregunta de dos niveles, así que el requisito
            # siempre se declara, y se señalan los casos con muy pocos sujetos.
            self$results$summary$setNote("design", txt("ml", "design_note"))
            if (length(raters) < 10) {
                self$results$summary$setNote("few", sprintf(txt("ml", "few_subjects"), length(raters)))
                private$.addKey("uncertain", sprintf(txt("ml", "key_few_subjects"), length(raters)))
            }

            # -----------------------------------------------------------------
            # Scale and item tables. Intervals are mokken's own (ci.*.l and
            # ci.*.u), computed with the chosen type.
            # ES: Tablas de la escala y de los ítems. Los intervalos son los de
            # mokken (ci.*.l y ci.*.u), con el tipo elegido.
            # ---------------------------------------------------------------
            Hs <- H$H
            get <- function(df, col, i = 1) if (col %in% names(df)) .mk_clean_num(df[i, col]) else NA_real_
            z_scale <- if (!is.null(zfit)) list(HW = get(zfit$Z, "HW"), HB = get(zfit$Z, "HB")) else list()
            readings <- list(HW = txt("ml", "read_hw"), HB = txt("ml", "read_hb"), HBW = txt("ml", "read_bw"))
            for (cf in c("HW", "HB", "HBW")) {
                label <- switch(cf, HW = tr("Within raters (HW)", "Dentro de evaluadores (HW)"),
                                HB = tr("Between raters (HB)", "Entre evaluadores (HB)"),
                                HBW = tr("Ratio (BW = HB / HW)", "Razón (BW = HB / HW)"))
                z <- if (cf %in% names(z_scale)) z_scale[[cf]] else NA_real_
                .mk_add_row(self$results$scale, cf, list(
                    coef = label, est = get(Hs, cf), se = get(Hs, paste0("se.", cf)),
                    lower = get(Hs, paste0("ci.", cf, ".l")), upper = get(Hs, paste0("ci.", cf, ".u")),
                    z = z, p = pz(z), reading = readings[[cf]]))
            }
            if (!is.null(zfit))
                self$results$scale$setNote("z", sprintf(txt("ml", "z_note"), .mk_fmt_num(self$options$zLowerbound, 2),
                    if (identical(self$options$typeZ, "RP")) tr("range-preserving", "preservadora de rango")
                    else tr("Wald-based", "de Wald")))

            Hi <- H$Hi
            for (i in seq_along(items)) {
                zw <- if (!is.null(zfit)) get(zfit$Zi, "HWi", i) else NA_real_
                zb <- if (!is.null(zfit)) get(zfit$Zi, "HBi", i) else NA_real_
                .mk_add_row(self$results$items, items[i], list(
                    item = items[i], w = get(Hi, "HWi", i), wse = get(Hi, "se.HWi", i),
                    b = get(Hi, "HBi", i), bse = get(Hi, "se.HBi", i), bw = get(Hi, "HBWi", i), zw = zw, zb = zb))
            }

            if (isTRUE(self$options$showHij)) {
                Hij <- H$Hij
                # Pairs are identified by position: MLcoefH() returns them in
                # combn() order (1-2, 1-3, ..., 2-3, ...). Splitting its row
                # names at spaces would break item names that contain spaces.
                # ES: Los pares se identifican por posición: MLcoefH() los
                # devuelve en el orden de combn(). Partir sus nombres de fila
                # por espacios rompería nombres de ítem con espacios.
                pairs <- utils::combn(length(items), 2)
                if (nrow(Hij) != ncol(pairs)) {
                    .mk_add_row(self$results$pairs, 1, list(item1 = txt("common", "not_computable")))
                    self$results$pairs$addFootnote(rowKey = 1, col = "item1", txt("ml", "pairs_mismatch"))
                } else {
                    for (k in seq_len(nrow(Hij)))
                        .mk_add_row(self$results$pairs, k, list(
                            item1 = items[pairs[1, k]], item2 = items[pairs[2, k]],
                            w = get(Hij, "HWij", k), wse = get(Hij, "se.HWij", k), b = get(Hij, "HBij", k),
                            bse = get(Hij, "se.HBij", k), bw = get(Hij, "HBWij", k)))
                }
            }

            if (isTRUE(self$options$showCov)) {
                private$.fillCov(self$results$covH, H$covH)
                private$.fillCov(self$results$covHi, H$covHi)
                if (isTRUE(self$options$showHij))
                    private$.fillCov(self$results$covHij, H$covHij)
            }

            # -----------------------------------------------------------------
            # Intraclass correlations (Category B).
            # ES: Correlaciones intraclase (Categoría B).
            # -----------------------------------------------------------------
            if (isTRUE(self$options$showICC)) {
                icc <- .mk_capture(mokken::ICC(XS))
                table <- self$results$icc
                if (is.null(icc$value)) {
                    .mk_add_row(table, 1, list(item = txt("common", "not_computable")))
                    table$addFootnote(rowKey = 1, col = "item", sprintf(txt("ml", "icc_failed"), icc$error))
                } else {
                    it <- icc$value$itemICC
                    for (i in seq_len(nrow(it)))
                        .mk_add_row(table, i, list(item = as.character(it$Item[i]), icc = .mk_clean_num(it$ICC[i])))
                    sc <- icc$value$scaleICC
                    .mk_add_row(table, "scale", list(item = tr("Total score", "Puntuación total"),
                        icc = .mk_clean_num(sc$ICC), f = .mk_clean_num(sc$F), df1 = as.integer(sc$df1),
                        df2 = as.integer(sc$df2), p = .mk_clean_num(sc$p.value)))
                    table$setNote("icc", txt("ml", "icc_note"))
                    private$.addKey("findings", sprintf(txt("ml", "key_icc"), .mk_fmt_num(sc$ICC, 2)))
                }
                warnings <- c(warnings, icc$warnings)
            }

            # -----------------------------------------------------------------
            # Weights for two-level Guttman errors, one column per score
            # pattern (score of the first item, score of the second).
            # ES: Pesos para errores de Guttman de dos niveles, una columna por
            # patrón de puntajes (puntaje del primer ítem, del segundo).
            # -----------------------------------------------------------------
            if (isTRUE(self$options$showWeights))
                warnings <- c(warnings, private$.fillWeights(XS, items, prep$range, lang, tr, txt))

            # -----------------------------------------------------------------
            # Plot state: HWi and HBi of every item with intervals.
            # ES: Estado del gráfico: HWi y HBi de cada ítem con intervalos.
            # -----------------------------------------------------------------
            if (isTRUE(self$options$levelPlot)) {
                z <- stats::qnorm(1 - (1 - level) / 2)
                hw <- as.numeric(Hi$HWi); hb <- as.numeric(Hi$HBi)
                sw <- as.numeric(Hi$se.HWi); sb <- as.numeric(Hi$se.HBi)
                lw <- if ("ci.HWi.l" %in% names(Hi)) as.numeric(Hi$ci.HWi.l) else hw - z * sw
                uw <- if ("ci.HWi.u" %in% names(Hi)) as.numeric(Hi$ci.HWi.u) else hw + z * sw
                lb <- if ("ci.HBi.l" %in% names(Hi)) as.numeric(Hi$ci.HBi.l) else hb - z * sb
                ub <- if ("ci.HBi.u" %in% names(Hi)) as.numeric(Hi$ci.HBi.u) else hb + z * sb
                within <- tr("Within (HWi)", "Dentro (HWi)"); between <- tr("Between (HBi)", "Entre (HBi)")
                .mk_set_size(self$results$levelPlot, self$options, 600, max(340, 30 * length(items) + 160))
                self$results$levelPlot$setState(list(
                    data = data.frame(item = rep(items, 2), level = rep(c(within, between), each = length(items)),
                                      est = c(hw, hb), lower = c(lw, lb), upper = c(uw, ub), stringsAsFactors = FALSE),
                    items = items, ci = isTRUE(self$options$ci),
                    labels = list(x = tr("Item scalability", "Escalabilidad del ítem"), color = tr("Level", "Nivel"))))
            }

            # -----------------------------------------------------------------
            # Interpretation, key findings, notes.
            # ES: Interpretación, hallazgos, notas.
            # -----------------------------------------------------------------
            hw <- get(Hs, "HW"); hb <- get(Hs, "HB"); bw <- get(Hs, "HBW")
            parts <- sprintf(txt("ml", "interp_values"), .mk_fmt_num(hw, 3), .mk_fmt_num(hb, 3), .mk_fmt_num(bw, 3))
            private$.addKey("findings", sprintf(txt("ml", "key_values"), .mk_fmt_num(hw, 2), .mk_fmt_num(hb, 2),
                                                .mk_fmt_num(bw, 2)))
            lb_hb <- get(Hs, "ci.HB.l")
            if (!is.na(hb) && hb <= 0) {
                parts <- c(parts, txt("ml", "interp_hb_zero"))
                private$.addKey("warnings", txt("ml", "key_hb_zero"))
            } else if (!is.na(lb_hb) && lb_hb <= 0) {
                parts <- c(parts, txt("ml", "interp_hb_ci"))
                private$.addKey("uncertain", txt("ml", "key_hb_ci"))
            } else if (!is.na(hb)) {
                parts <- c(parts, txt("ml", "interp_hb_pos"))
            }
            if (!is.na(bw))
                parts <- c(parts, sprintf(txt("ml", "interp_bw"), .mk_fmt_num(bw, 2)))
            neg_b <- items[is.finite(as.numeric(Hi$HBi)) & as.numeric(Hi$HBi) <= 0]
            if (length(neg_b) > 0) {
                parts <- c(parts, sprintf(txt("ml", "interp_items_hb"), length(neg_b), paste(neg_b, collapse = ", ")))
                private$.addKey("warnings", sprintf(txt("ml", "key_items_hb"), length(neg_b)))
            }
            parts <- c(parts, txt("ml", "interp_next"))
            self$results$interpretation$setContent(.mk_interpretation(lang, parts))

            for (section in .mk_failed_sections(self$results, lang))
                private$.addKey("warnings", sprintf(txt("common", "key_failed"), section))
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

        # Covariance matrix as a table: one row and one column per
        # coefficient.
        # ES: Matriz de covarianzas como tabla: una fila y una columna por
        # coeficiente.
        .fillCov = function(table, m) {
            if (is.null(m)) return(invisible(NULL))
            m <- as.matrix(m)
            names <- if (is.null(colnames(m))) paste0("V", seq_len(ncol(m))) else colnames(m)
            for (j in seq_len(ncol(m)))
                table$addColumn(name = paste0("c", j), title = names[j], type = "number", format = "zto")
            for (i in seq_len(nrow(m))) {
                values <- list(coef = if (is.null(rownames(m))) names[i] else rownames(m)[i])
                for (j in seq_len(ncol(m)))
                    values[[paste0("c", j)]] <- .mk_clean_num(m[i, j])
                .mk_add_row(table, i, values)
            }
        },

        .fillWeights = function(XS, items, range, lang, tr, txt) {
            table <- self$results$weights
            scores <- seq(range[1], range[2])
            patterns <- expand.grid(b = scores, a = scores)[, c("a", "b")]
            for (k in seq_len(nrow(patterns)))
                table$addColumn(name = paste0("w", k), title = paste0("(", patterns$a[k], ", ", patterns$b[k], ")"),
                                type = "number", superTitle = tr("Score pattern (first, second item)",
                                                                 "Patrón de puntajes (primer, segundo ítem)"))
            warnings <- character(0)
            row <- 0
            for (i in seq_along(items)) for (j in seq_along(items)) {
                if (i >= j) next
                row <- row + 1
                res <- .mk_capture(mokken::MLweight(XS[, c(1, i + 1, j + 1)], minx = range[1], maxx = range[2]))
                warnings <- c(warnings, res$warnings)
                values <- list(pair = paste(items[i], "–", items[j]))
                if (!is.null(res$value))
                    for (k in seq_along(res$value))
                        values[[paste0("w", k)]] <- .mk_clean_num(res$value[k])
                .mk_add_row(table, row, values)
                # mokken 3.1.2 fails inside MLweight() for some pairs whose
                # item-step popularities are tied ("xtfrm" on a data frame);
                # the footnote explains it instead of showing the raw error.
                # ES: mokken 3.1.2 falla dentro de MLweight() en algunos pares
                # cuyas popularidades de pasos de ítem están empatadas; la nota
                # lo explica en lugar de mostrar el error crudo.
                if (is.null(res$value))
                    table$addFootnote(rowKey = row, col = "pair", if (grepl("xtfrm", res$error, fixed = TRUE))
                        txt("ml", "weights_ties") else sprintf(txt("ml", "weights_failed"), res$error))
            }
            table$setNote("w", txt("ml", "weights_note"))
            warnings
        },

        .setTitles = function(tr) {
            titles <- list(
                c("intro", "Introduction", "Introducción"),
                c("keySummary", "Key findings", "Hallazgos clave"),
                c("choicesGuide", "About your choices", "Acerca de sus elecciones"),
                c("mlGuide", "How to read two-level scalability", "Cómo leer la escalabilidad de dos niveles"),
                c("summary", "Analysis summary", "Resumen del análisis"),
                c("scale", "Two-level scalability of the scale", "Escalabilidad de dos niveles de la escala"),
                c("items", "Two-level item scalability", "Escalabilidad de dos niveles de los ítems"),
                c("pairs", "Two-level item-pair scalability", "Escalabilidad de dos niveles de los pares de ítems"),
                c("covH", "Covariance matrix of the scale coefficients", "Matriz de covarianzas de los coeficientes de la escala"),
                c("covHi", "Covariance matrix of the item coefficients", "Matriz de covarianzas de los coeficientes de los ítems"),
                c("covHij", "Covariance matrix of the item-pair coefficients", "Matriz de covarianzas de los coeficientes de los pares"),
                c("icc", "Intraclass correlations", "Correlaciones intraclase"),
                c("weights", "Weights for two-level Guttman errors", "Pesos para errores de Guttman de dos niveles"),
                c("levelPlot", "Within and between scalability", "Escalabilidad dentro y entre"),
                c("interpretation", "Applied interpretation", "Interpretación aplicada"),
                c("notes", "Notes", "Notas"))
            for (t in titles)
                tryCatch(self$results[[t[1]]]$setTitle(tr(t[2], t[3])), error = function(e) NULL)
            cols <- list(
                list("summary", "element", "Element", "Elemento"), list("summary", "value", "Value", "Valor"),
                list("scale", "coef", "Coefficient", "Coeficiente"), list("scale", "est", "Estimate", "Estimación"),
                list("scale", "se", "SE", "EE"), list("scale", "lower", "Lower", "Inferior"),
                list("scale", "upper", "Upper", "Superior"), list("scale", "reading", "Reading", "Lectura"),
                list("items", "item", "Item", "Ítem"), list("items", "w", "Within", "Dentro"),
                list("items", "wse", "SE", "EE"), list("items", "b", "Between", "Entre"), list("items", "bse", "SE", "EE"),
                list("items", "bw", "Ratio BW", "Razón BW"), list("items", "zw", "z (within)", "z (dentro)"),
                list("items", "zb", "z (between)", "z (entre)"),
                list("pairs", "item1", "Item", "Ítem"), list("pairs", "item2", "Item", "Ítem"),
                list("pairs", "w", "Within", "Dentro"), list("pairs", "wse", "SE", "EE"),
                list("pairs", "b", "Between", "Entre"), list("pairs", "bse", "SE", "EE"),
                list("pairs", "bw", "Ratio BW", "Razón BW"),
                list("covH", "coef", "Coefficient", "Coeficiente"), list("covHi", "coef", "Coefficient", "Coeficiente"),
                list("covHij", "coef", "Coefficient", "Coeficiente"),
                list("icc", "item", "Item", "Ítem"), list("weights", "pair", "Item pair", "Par de ítems"))
            for (cdef in cols)
                tryCatch(self$results[[cdef[[1]]]]$getColumn(cdef[[2]])$setTitle(tr(cdef[[3]], cdef[[4]])),
                         error = function(e) NULL)
            ci_title <- tr("Confidence interval", "Intervalo de confianza")
            for (col in c("lower", "upper"))
                tryCatch(self$results$scale$getColumn(col)$setSuperTitle(ci_title), error = function(e) NULL)
        },

        # ---------------------------------------------------------------------
        # Within and between scalability of every item, side by side, with
        # zero marked: an item whose between interval crosses zero does not
        # help to order the subjects.
        # ES: Escalabilidad dentro y entre de cada ítem, lado a lado, con el
        # cero marcado: un ítem cuyo intervalo entre cruza el cero no ayuda a
        # ordenar a los sujetos.
        # ---------------------------------------------------------------------
        .plotLevels = function(image, ggtheme, theme, ...) {
            state <- image$state
            if (is.null(state) || is.null(state$data) || nrow(state$data) == 0)
                return(FALSE)
            df <- state$data
            df$item <- factor(df$item, levels = rev(state$items))
            style <- .mk_plot_style(self$options)
            dodge <- ggplot2::position_dodge(width = 0.5)
            plot <- ggplot2::ggplot(df, ggplot2::aes(x = item, y = est, color = level)) +
                ggplot2::geom_hline(yintercept = 0, color = "grey40") +
                ggplot2::geom_hline(yintercept = 0.3, linetype = "dashed", color = "grey60")
            if (isTRUE(state$ci))
                plot <- plot + ggplot2::geom_errorbar(ggplot2::aes(ymin = lower, ymax = upper), width = 0.2,
                                                      position = dodge, linewidth = style$line)
            plot <- plot + ggplot2::geom_point(size = style$point * 1.4, position = dodge)
            if (style$labels)
                plot <- plot + ggplot2::geom_text(ggplot2::aes(label = formatC(est, format = "f", digits = 2)),
                                                  position = dodge, vjust = -0.9, size = 2.6, show.legend = FALSE)
            plot <- plot + ggplot2::coord_flip(ylim = if (style$full) c(min(-0.2, min(df$lower, na.rm = TRUE)), 1) else NULL) +
                ggplot2::labs(x = NULL, y = state$labels$x, color = state$labels$color) +
                ggtheme + .mk_plot_theme(self$options)
            palette <- .mk_plot_series_palette(self$options$plotPalette)
            if (!is.null(palette))
                plot <- plot + ggplot2::scale_color_manual(values = palette[c(5, 6)])
            print(plot)
            TRUE
        }
    )
)

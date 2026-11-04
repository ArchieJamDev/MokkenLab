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
# Shared, purely structural helpers used across module .b.R files.
# ES: Helpers compartidos y puramente estructurales, usados por los archivos
# .b.R de los módulos.
#
# Same contract as EGAnetLab's and AssumptionsLab's shared-helpers.R: this
# file holds only plumbing (translation, HTML assembly, row adding, number
# cleaning, palette lookup, item preparation, error capture). Narrative and
# interpretation text lives in texts.R; statistical decisions live in each
# analysis' .b.R. Every helper uses the `.mk_` prefix (MokkenLab) so that this
# module can be loaded next to EGAnetLab (`.el_`) and AssumptionsLab (`.al_`)
# in the same R session without name clashes.
#
# ES: Mismo contrato que el shared-helpers.R de EGAnetLab y AssumptionsLab:
# este archivo contiene solo plomería (traducción, ensamblado HTML, filas,
# limpieza de números, paletas, preparación de ítems, captura de errores). El
# texto narrativo e interpretativo vive en texts.R; las decisiones
# estadísticas viven en el .b.R de cada análisis. Cada helper usa el prefijo
# `.mk_` (MokkenLab) para que este módulo pueda cargarse junto a EGAnetLab
# (`.el_`) y AssumptionsLab (`.al_`) en la misma sesión de R sin choques de
# nombres.
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# .mk_tr()
#
# Input: lang - normalized language code ("es" or "en", see
#   .mk_normalize_lang() in texts.R); en - English text; es - Spanish text
#   (optional; defaults to `en`).
# Output: `es` when lang is "es", otherwise `en`.
#
# Report text that varies at runtime goes through this function, never
# through jmvcore's `.()`: `.()` caches its translator once per R process, so
# dynamic text routed through it freezes at the language active when jamovi
# started (the FiabilityLab regression documented in ARCHITECTURE.md §12).
#
# ES: El texto del informe que varía en tiempo de ejecución pasa por esta
# función, nunca por `.()` de jmvcore: `.()` guarda su traductor una sola vez
# por proceso de R, así que el texto dinámico que pasa por ahí se congela en
# el idioma activo cuando jamovi arrancó (la regresión de FiabilityLab
# documentada en ARCHITECTURE.md §12).
# -----------------------------------------------------------------------------
.mk_tr <- function(lang, en, es = NULL) {
    if (is.null(es))
        es <- en
    if (identical(lang, "es")) es else en
}

# -----------------------------------------------------------------------------
# .mk_html_escape() / .mk_html_block() / .mk_html_list()
#
# HTML assembly for every `Html` result. Identical in behavior to the
# .el_html_* family of EGAnetLab (justified paragraphs, 7.25in maximum width,
# real <ul>/<ol> lists instead of hyphen-prefixed prose). Text that goes into
# a native Table cell is never escaped: Table cells are plain text.
#
# ES: Ensamblado HTML para cada resultado `Html`. Mismo comportamiento que la
# familia .el_html_* de EGAnetLab (párrafos justificados, ancho máximo de
# 7.25in, listas <ul>/<ol> reales). El texto que va a una celda de una Table
# nativa nunca se escapa: las celdas de Table son texto plano.
# -----------------------------------------------------------------------------
.mk_html_escape <- function(x) {
    x <- as.character(x)
    x <- gsub("&", "&amp;", x, fixed = TRUE)
    x <- gsub("<", "&lt;", x, fixed = TRUE)
    x <- gsub(">", "&gt;", x, fixed = TRUE)
    x <- gsub("\"", "&quot;", x, fixed = TRUE)
    x
}

.mk_html_block <- function(title = NULL, text, paragraphs = TRUE, escape = TRUE, raw = FALSE) {

    if (isTRUE(raw)) {
        body <- paste(text, collapse = "\n")
    } else {
        parts <- .mk_clean_text(text)

        if (length(parts) == 0)
            return("")

        p_style <- "margin: 0.15em 0 0.65em 0; line-height: 1.40;"
        maybe_escape <- if (isTRUE(escape)) .mk_html_escape else identity

        if (isTRUE(paragraphs)) {
            body <- paste0("<p style=\"", p_style, "\">", maybe_escape(parts), "</p>", collapse = "\n")
        } else {
            body <- paste0("<p style=\"", p_style, "\">", maybe_escape(paste(parts, collapse = " ")), "</p>")
        }
    }

    if (!nzchar(body))
        return("")

    h_style <- "margin: 0.55em 0 0.50em 0; line-height: 1.25;"
    div_style <- "max-width: 7.25in; width: 100%; box-sizing: border-box; text-align: justify;"

    if (!is.null(title) && nzchar(title))
        body <- paste0("<h3 style=\"", h_style, "\">", .mk_html_escape(title), "</h3>\n", body)

    paste0("<div style=\"", div_style, "\">\n", body, "\n</div>")
}

.mk_html_list <- function(items, ordered = FALSE, escape = TRUE) {
    parts <- .mk_clean_text(items)

    if (length(parts) == 0)
        return("")

    maybe_escape <- if (isTRUE(escape)) .mk_html_escape else identity
    tag <- if (isTRUE(ordered)) "ol" else "ul"
    li_style <- "margin: 0 0 0.30em 0; line-height: 1.40; text-align: justify;"
    list_style <- "margin: 0.15em 0 0.65em 0; padding-left: 1.35em;"

    items_html <- paste0("<li style=\"", li_style, "\">", maybe_escape(parts), "</li>", collapse = "\n")

    paste0("<", tag, " style=\"", list_style, "\">\n", items_html, "\n</", tag, ">")
}

# -----------------------------------------------------------------------------
# .mk_section()
#
# Input: title - section heading; paragraphs - plain text paragraphs;
#   bullets - optional plain text list items appended after the paragraphs.
# Output: one Html block with the heading inside it. jamovi does not print
#   the title of an Html result, so every section heading goes inside the
#   block (the same convention as EGAnetLab's status blocks).
# ES: Salida: un bloque Html con el encabezado dentro. jamovi no muestra el
#   título de un resultado Html, así que cada encabezado de sección va dentro
#   del bloque (la misma convención que los bloques de estado de EGAnetLab).
# -----------------------------------------------------------------------------
.mk_section <- function(title, paragraphs = character(0), bullets = character(0)) {
    p_style <- "margin: 0.15em 0 0.65em 0; line-height: 1.40;"
    parts <- .mk_clean_text(paragraphs)
    html <- if (length(parts) > 0)
        paste0("<p style=\"", p_style, "\">", .mk_html_escape(parts), "</p>", collapse = "\n") else ""
    if (length(.mk_clean_text(bullets)) > 0)
        html <- paste0(html, "\n", .mk_html_list(bullets))
    .mk_html_block(title, html, raw = TRUE)
}

# -----------------------------------------------------------------------------
# .mk_add_row() / .mk_clean_num() / .mk_fmt_num()
#
# Row adding and number cleaning. .mk_clean_num() turns NULL, NA, NaN and Inf
# into NA_real_, so that a Category B failure (a statistic that came back
# non-finite without an error) is never displayed as a real number.
#
# ES: Agregar filas y limpiar números. .mk_clean_num() convierte NULL, NA,
# NaN e Inf en NA_real_, para que un fallo de Categoría B (un estadístico que
# volvió no finito sin lanzar error) nunca se muestre como número real.
# -----------------------------------------------------------------------------
.mk_add_row <- function(table, key, values) {
    table$addRow(rowKey = key, values = values)
}

.mk_clean_num <- function(x) {
    if (length(x) == 0)
        return(NA_real_)

    x <- suppressWarnings(as.numeric(x[1]))

    if (is.na(x) || is.nan(x) || is.infinite(x))
        return(NA_real_)

    x
}

.mk_fmt_num <- function(x, digits = 3, missing = "") {
    x <- .mk_clean_num(x)
    if (is.na(x))
        return(missing)
    formatC(x, format = "f", digits = digits)
}

# -----------------------------------------------------------------------------
# .mk_plot_series_palette()
#
# Input: choice - the analysis' plotPalette option ("jamovi", "colorblind" or
#   "viridis").
# Output: NULL for "jamovi" (let jamovi's own ggtheme scale handle colors),
#   otherwise a character vector of colors.
#
# Same policy as EGAnetLab and AssumptionsLab: the default follows jamovi's
# global theme; only the two palettes jamovi does not ship are added. The
# viridis stops are hard-coded so the result never depends on whether
# viridisLite is installed.
#
# ES: Misma política que EGAnetLab y AssumptionsLab: por defecto se sigue el
# tema global de jamovi; solo se agregan las dos paletas que jamovi no trae.
# Los puntos de viridis están fijos para que el resultado nunca dependa de si
# viridisLite está instalado.
# -----------------------------------------------------------------------------
.mk_plot_series_palette <- function(choice) {
    if (is.null(choice) || length(choice) == 0 || identical(choice, "jamovi"))
        return(NULL)

    if (identical(choice, "viridis"))
        return(c("#440154", "#414487", "#2A788E", "#22A884", "#7AD151", "#FDE725"))

    if (identical(choice, "colorblind"))
        return(c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"))

    NULL
}

# -----------------------------------------------------------------------------
# .mk_plot_colors()
#
# Input: theme - the theme list jamovi passes to every render function;
#   palette - the plotPalette option.
# Output: list(line =, fill =, alert =) for single-series plots. The default
#   derives from jamovi's own theme (theme$color / theme$fill); a module
#   palette only replaces it when the user picked one jamovi does not ship.
# ES: Salida: list(line =, fill =, alert =) para gráficos de una sola serie.
#   Por defecto se deriva del propio tema de jamovi; una paleta del módulo
#   solo lo reemplaza si el usuario eligió una que jamovi no trae.
# -----------------------------------------------------------------------------
.mk_plot_colors <- function(theme, palette) {
    series <- .mk_plot_series_palette(palette)
    base <- if (!is.null(theme$color) && length(theme$color) >= 1) theme$color[1] else "#333333"
    accent <- if (!is.null(theme$fill) && length(theme$fill) >= 2) theme$fill[2] else "#6B9DE8"
    if (!is.null(series))
        return(list(line = series[5], fill = series[2], alert = series[6]))
    list(line = base, fill = accent, alert = "#C0392B")
}

# -----------------------------------------------------------------------------
# .mk_prepare_items()
#
# Input: data - self$data; vars - the item names selected by the user;
#   options - list(naHandling =, twowaySets =, twowaySeed =, scoreRange =,
#   scoreMin =, scoreMax =, negItems =); min_items - minimum number of usable
#   items; keep - names of extra columns (cluster or group variables) that
#   must stay aligned with the item rows.
# Output: list(ok =, reason =, detail =, data =, extra =, n_total =,
#   n_complete =, n_used =, imputed =, completed =, neg =, range =).
#   `ok = FALSE` carries a `reason` code that the caller turns into a
#   Category A jmvcore::reject() message, and `detail` names the items.
#
# mokken refuses missing values and needs integer item scores, so this is
# where the two decisions the package leaves to the user are taken, and both
# are reported: listwise deletion, or two-way imputation (mokken::twoway(),
# van Ginkel et al., 2007), which keeps every respondent. Negatively worded
# items are reversed as declared minimum + declared maximum - score (the same
# operation as mokken::recode()). Observations are never removed silently
# (CODE_STYLE §21): the counts are returned so the report can state them.
#
# ES: mokken rechaza valores perdidos y necesita puntajes enteros, así que
# aquí se toman las dos decisiones que el paquete deja al usuario, y ambas se
# informan: eliminación por lista, o imputación two-way (mokken::twoway(),
# van Ginkel et al., 2007), que conserva a cada persona. Los ítems redactados
# en negativo se invierten como mínimo declarado + máximo declarado -
# puntaje (la misma operación que mokken::recode()). Nunca se eliminan
# observaciones en silencio (CODE_STYLE §21): se devuelven los conteos para
# que el informe los diga.
# -----------------------------------------------------------------------------
.mk_prepare_items <- function(data, vars, options, min_items = 2, keep = character(0)) {

    out <- list(ok = FALSE, reason = NULL, detail = character(0), data = NULL, extra = NULL,
                n_total = 0L, n_complete = 0L, n_used = 0L, imputed = 0L, completed = NULL,
                neg = character(0), range = c(NA_real_, NA_real_))

    if (is.null(data) || nrow(data) == 0) {
        out$reason <- "no_data"
        return(out)
    }

    vars <- vars[!is.na(vars) & nzchar(vars)]

    if (length(vars) < min_items) {
        out$reason <- if (min_items <= 2) "too_few_items" else "too_few_items_3"
        return(out)
    }

    items <- as.data.frame(
        lapply(vars, function(v) jmvcore::toNumeric(data[[v]])),
        stringsAsFactors = FALSE
    )
    names(items) <- vars
    # Keep jamovi's row numbers (they skip filtered rows) so scores saved back
    # to the dataset land on the right rows.
    # ES: Se conservan los números de fila de jamovi (omiten las filas
    # filtradas) para que las puntuaciones guardadas caigan en las filas
    # correctas.
    rownames(items) <- rownames(data)

    extra <- NULL
    keep <- keep[!is.na(keep) & nzchar(keep)]
    if (length(keep) > 0) {
        extra <- as.data.frame(lapply(keep, function(v) data[[v]]), stringsAsFactors = FALSE)
        names(extra) <- keep
        rownames(extra) <- rownames(data)
    }

    out$n_total <- nrow(items)

    observed <- vapply(items, function(x) sum(!is.na(x)), numeric(1))
    if (any(observed < 2)) {
        out$reason <- "empty_item"
        out$detail <- names(observed)[observed < 2]
        return(out)
    }

    # mokken works on item scores 0, 1, ..., m (or any integer range); a
    # non-integer value is a data-entry or variable-type problem, not
    # something to round silently.
    # ES: mokken trabaja con puntajes de ítem enteros; un valor no entero es
    # un problema de captura o de tipo de variable, no algo para redondear en
    # silencio.
    non_integer <- vapply(items, function(x) any(abs(x - round(x)) > 1e-8, na.rm = TRUE), logical(1))
    if (any(non_integer)) {
        out$reason <- "non_integer"
        out$detail <- names(non_integer)[non_integer]
        return(out)
    }

    # Declared score range: the observed range, or the one the user typed
    # (needed when an extreme category was never chosen in the sample).
    # ES: Rango de puntajes declarado: el observado, o el que el usuario
    # escribió (necesario cuando una categoría extrema nunca se eligió en la
    # muestra).
    observed_min <- min(vapply(items, min, numeric(1), na.rm = TRUE))
    observed_max <- max(vapply(items, max, numeric(1), na.rm = TRUE))
    if (identical(options$scoreRange, "manual")) {
        range <- c(as.numeric(options$scoreMin), as.numeric(options$scoreMax))
        if (!(range[1] < range[2]) || observed_min < range[1] || observed_max > range[2]) {
            out$reason <- "bad_range"
            out$detail <- c(.mk_fmt_num(range[1], 0), .mk_fmt_num(range[2], 0),
                            .mk_fmt_num(observed_min, 0), .mk_fmt_num(observed_max, 0))
            return(out)
        }
    } else {
        range <- c(observed_min, observed_max)
    }
    out$range <- range

    complete <- stats::complete.cases(items)
    if (!is.null(extra))
        complete <- complete & stats::complete.cases(extra)
    out$n_complete <- sum(complete)

    if (identical(options$naHandling, "twoway") && any(!stats::complete.cases(items))) {
        # Cases with a missing cluster or group value cannot be placed, so
        # they are always left out, and counted.
        # ES: Los casos sin valor en la variable de conglomerado o de grupo no
        # pueden ubicarse, así que siempre se dejan fuera, y se cuentan.
        if (!is.null(extra)) {
            keep_rows <- stats::complete.cases(extra)
            items <- items[keep_rows, , drop = FALSE]
            extra <- extra[keep_rows, , drop = FALSE]
        }
        sets <- max(1L, as.integer(options$twowaySets))
        seed <- as.integer(options$twowaySeed)
        out$imputed <- sum(is.na(items))
        completed <- .mk_capture(mokken::twoway(
            as.matrix(items), nCompletedDataSets = sets,
            minX = range[1], maxX = range[2], seed = seed))
        if (is.null(completed$value)) {
            out$reason <- "twoway_failed"
            out$detail <- completed$error
            return(out)
        }
        sets_list <- if (sets == 1) list(completed$value) else completed$value
        sets_list <- lapply(sets_list, function(m) {
            m <- as.data.frame(m)
            names(m) <- vars
            rownames(m) <- rownames(items)
            m
        })
        out$completed <- sets_list
        items <- sets_list[[1]]
    } else {
        items <- items[complete, , drop = FALSE]
        if (!is.null(extra))
            extra <- extra[complete, , drop = FALSE]
    }

    if (nrow(items) < 2) {
        out$reason <- "too_few_cases"
        return(out)
    }

    spread <- vapply(items, function(x) stats::sd(x), numeric(1))
    constant <- names(spread)[!is.finite(spread) | spread == 0]
    if (length(constant) > 0) {
        out$reason <- "constant_item"
        out$detail <- constant
        return(out)
    }

    neg <- intersect(as.character(unlist(options$negItems)), vars)
    out$neg <- neg
    items <- .mk_reverse_items(items, neg, range)
    if (!is.null(out$completed))
        out$completed <- lapply(out$completed, .mk_reverse_items, items = neg, range = range)

    out$ok <- TRUE
    out$data <- items
    out$extra <- extra
    out$n_used <- nrow(items)
    out
}

# -----------------------------------------------------------------------------
# .mk_reverse_items()
#
# Input: data - item data frame; items - names of the items to reverse;
#   range - c(minimum, maximum) declared score range.
# Output: data with those items reversed as minimum + maximum - score (a 0-4
#   item becomes 4-0), exactly as mokken::recode(). Other columns untouched.
# ES: Salida: data con esos ítems invertidos como mínimo + máximo - puntaje
#   (un ítem 0-4 pasa a 4-0), exactamente como mokken::recode(). Las demás
#   columnas no cambian.
# -----------------------------------------------------------------------------
.mk_reverse_items <- function(data, items, range) {
    for (v in intersect(items, names(data)))
        data[[v]] <- range[1] + range[2] - data[[v]]
    data
}

# -----------------------------------------------------------------------------
# .mk_capture()
#
# Input: expr - an expression (usually a mokken call).
# Output: list(value =, error =, warnings =). `value` is NULL on error;
#   `warnings` keeps every warning message so the report can surface it
#   instead of suppressing it (CODE_STYLE §21). Console output is captured
#   and discarded: several mokken functions print their results as a side
#   effect, which would otherwise end up in jamovi's log.
# ES: Salida: list(value =, error =, warnings =). `value` es NULL si hubo
#   error; `warnings` conserva cada advertencia para que el informe la
#   muestre (CODE_STYLE §21). La salida de consola se captura y se descarta:
#   varias funciones de mokken imprimen sus resultados como efecto
#   secundario, que de otro modo terminaría en el registro de jamovi.
# -----------------------------------------------------------------------------
.mk_capture <- function(expr) {
    warnings <- character(0)
    value <- NULL
    utils::capture.output(
        value <- withCallingHandlers(
            tryCatch(expr, error = function(e) structure(list(message = conditionMessage(e)), class = "mk_error")),
            warning = function(w) {
                warnings <<- c(warnings, trimws(gsub("[[:space:]]+", " ", conditionMessage(w))))
                invokeRestart("muffleWarning")
            }
        )
    )

    if (inherits(value, "mk_error"))
        return(list(value = NULL, error = trimws(gsub("[[:space:]]+", " ", value$message)),
                    warnings = unique(warnings)))

    list(value = value, error = NULL, warnings = unique(warnings))
}

# -----------------------------------------------------------------------------
# .mk_translate_notices()
#
# Input: warnings - messages captured from mokken; lang.
# Output: the same messages, with the known mokken notices rewritten in the
#   report language, so the Notes block never mixes languages.
# ES: Salida: los mismos mensajes, con los avisos conocidos de mokken
#   reescritos en el idioma del informe, para que el bloque de Notas nunca
#   mezcle idiomas.
# -----------------------------------------------------------------------------
.mk_translate_notices <- function(warnings, lang) {
    out <- character(0)
    for (w in unique(warnings)) {
        key <- NULL
        if (grepl("Varying numbers of item scores", w, fixed = TRUE))
            key <- "varying_categories"
        else if (grepl("to enable testing Hi > c", w, fixed = TRUE))
            key <- "typez_wb_testhi"
        else if (grepl("type.z has been changed to 'WB'", w, fixed = TRUE))
            key <- "typez_wb"
        else if (grepl("minx has not been specified", w, fixed = TRUE) ||
                 grepl("maxx has not been specified", w, fixed = TRUE))
            next
        out <- c(out, if (is.null(key)) w else .mk_text(lang, "notices", key))
    }
    unique(out)
}

# -----------------------------------------------------------------------------
# .mk_key_summary()
#
# Input: lang; key - list with character vectors findings, uncertain and
#   warnings, each a one-sentence line that names its section.
# Output: the HTML of the key findings summary shown at the top of the
#   report. Empty lists say so, so the reader knows they were checked.
# ES: Salida: el HTML del resumen de hallazgos que se muestra al inicio del
#   informe. Las listas vacías lo dicen, para que el lector sepa que se
#   revisaron.
# -----------------------------------------------------------------------------
.mk_key_summary <- function(lang, key) {
    txt <- function(k) .mk_text(lang, "key", k)
    p_style <- "margin: 0.15em 0 0.65em 0; line-height: 1.40;"
    h_style <- "margin: 0.65em 0 0.20em 0; font-weight: bold;"
    section <- function(heading, lines, empty) {
        lines <- unique(lines)
        body <- if (length(lines) > 0) .mk_html_list(lines)
                else paste0("<p style=\"", p_style, "\">", .mk_html_escape(empty), "</p>")
        paste0("<p style=\"", h_style, "\">", .mk_html_escape(heading), "</p>\n", body)
    }
    parts <- c(
        paste0("<p style=\"", p_style, "\">", .mk_html_escape(txt("intro")), "</p>"),
        section(txt("h_findings"), key$findings, txt("none_findings")),
        section(txt("h_uncertain"), key$uncertain, txt("none_uncertain")),
        section(txt("h_warnings"), key$warnings, txt("none_warnings")))
    .mk_html_block(txt("title"), parts, raw = TRUE)
}

# -----------------------------------------------------------------------------
# .mk_choices_guide()
#
# Input: lang; paragraphs - list of c(heading, body) pairs, one per decision
#   currently selected.
# Output: the HTML of the "About your choices" block: an introduction, then
#   one paragraph per decision headed "Decision: selected value."
# ES: Salida: el HTML del bloque "Acerca de sus elecciones": una
#   introducción y un párrafo por decisión, con el encabezado "Decisión:
#   valor seleccionado."
# -----------------------------------------------------------------------------
.mk_choices_guide <- function(lang, paragraphs) {
    p_style <- "margin: 0.15em 0 0.65em 0; line-height: 1.40;"
    parts <- paste0("<p style=\"", p_style, "\">",
                    .mk_html_escape(.mk_text(lang, "choices", "intro")), "</p>")
    for (p in paragraphs)
        parts <- c(parts, paste0(
            "<p style=\"", p_style, "\"><strong>", .mk_html_escape(p[1]), "</strong> ",
            .mk_html_escape(p[2]), "</p>"))
    .mk_html_block(.mk_text(lang, "choices", "title"), parts, raw = TRUE)
}

# -----------------------------------------------------------------------------
# .mk_guide_block() / .mk_interpretation()
#
# Input: lang; key - a section of texts.R "guide" (and "guide_titles");
#   parts - interpretation paragraphs.
# Output: the HTML of a teaching block or of the applied interpretation, with
#   its heading inside the block: jamovi does not print the title of an Html
#   result, and without a heading the reader cannot tell a guide from a
#   result (seen in the first exported report, 2026-11-04).
# ES: Salida: el HTML de un bloque didáctico o de la interpretación
#   aplicada, con su encabezado dentro del bloque: jamovi no muestra el
#   título de un resultado Html, y sin encabezado el lector no distingue una
#   guía de un resultado (visto en el primer informe exportado, 2026-11-04).
# -----------------------------------------------------------------------------
.mk_guide_block <- function(lang, key) {
    .mk_html_block(.mk_text(lang, "guide_titles", key), .mk_html_list(.mk_text(lang, "guide", key)), raw = TRUE)
}

.mk_interpretation <- function(lang, parts) {
    .mk_html_block(.mk_text(lang, "common", "interp_title"), parts)
}

# -----------------------------------------------------------------------------
# .mk_data_choices()
#
# Input: lang; options; prep - the result of .mk_prepare_items().
# Output: the c(heading, body) pairs that explain the data decisions shared
#   by every analysis (missing values, score range, negatively worded items),
#   for .mk_choices_guide().
# ES: Salida: los pares c(encabezado, cuerpo) que explican las decisiones de
#   datos comunes a todos los análisis (valores perdidos, rango de puntajes,
#   ítems redactados en negativo), para .mk_choices_guide().
# -----------------------------------------------------------------------------
.mk_data_choices <- function(lang, options, prep = NULL) {
    txt <- function(k) .mk_text(lang, "choices", k)
    tr <- function(en, es) .mk_tr(lang, en, es)
    out <- list()
    if (identical(options$naHandling, "twoway"))
        out[[length(out) + 1]] <- c(
            paste0(txt("h_na"), ": ", tr("Two-way imputation", "Imputación two-way"), "."),
            sprintf(txt("na_twoway"), as.integer(options$twowaySets), as.integer(options$twowaySeed)))
    else
        out[[length(out) + 1]] <- c(paste0(txt("h_na"), ": ", tr("Listwise deletion", "Eliminación por lista"), "."),
                                    txt("na_listwise"))
    out[[length(out) + 1]] <- c(
        paste0(txt("h_range"), ": ", if (identical(options$scoreRange, "manual"))
            tr("Declared", "Declarado") else tr("Observed", "Observado"), "."),
        txt(if (identical(options$scoreRange, "manual")) "range_manual" else "range_observed"))
    if (!is.null(prep) && length(prep$neg) > 0)
        out[[length(out) + 1]] <- c(
            paste0(txt("h_neg"), ": ", paste(prep$neg, collapse = ", "), "."),
            txt("neg_body"))
    out
}

# -----------------------------------------------------------------------------
# .mk_reject_prep()
#
# Input: lang; prep - a failed .mk_prepare_items() result.
# Output: never returns: calls jmvcore::reject() with the explanation of the
#   blocking condition (Category A, CODE_STYLE §19.1).
# ES: Salida: nunca retorna: llama a jmvcore::reject() con la explicación de
#   la condición que bloquea (Categoría A, CODE_STYLE §19.1).
# -----------------------------------------------------------------------------
.mk_reject_prep <- function(lang, prep) {
    message <- .mk_text(lang, "common", prep$reason)
    if (prep$reason %in% c("empty_item", "constant_item", "non_integer"))
        message <- sprintf(message, paste(prep$detail, collapse = ", "))
    else if (identical(prep$reason, "bad_range"))
        message <- sprintf(message, prep$detail[1], prep$detail[2], prep$detail[3], prep$detail[4])
    else if (identical(prep$reason, "twoway_failed"))
        message <- sprintf(message, prep$detail)
    jmvcore::reject(message)
}

# -----------------------------------------------------------------------------
# .mk_cases_label()
#
# Input: tr; prep.
# Output: the "Cases used" text of every summary table: how many cases
#   entered, under which missing-data rule, and how many were complete.
# ES: Salida: el texto "Casos usados" de cada tabla resumen: cuántos casos
#   entraron, con qué regla de datos perdidos y cuántos estaban completos.
# -----------------------------------------------------------------------------
.mk_cases_label <- function(tr, prep) {
    if (!is.null(prep$completed)) {
        label <- sprintf(tr("%d of %d (two-way imputation of %d missing scores; %d complete cases)",
                            "%d de %d (imputación two-way de %d puntajes perdidos; %d casos completos)"),
                         prep$n_used, prep$n_total, as.integer(prep$imputed), prep$n_complete)
        if (length(prep$completed) > 1)
            label <- paste0(label, sprintf(tr("; results from completed data set 1 of %d, not pooled",
                                              "; resultados del conjunto completado 1 de %d, sin combinar"),
                                           length(prep$completed)))
        return(label)
    }
    sprintf(tr("%d of %d (listwise deletion)", "%d de %d (eliminación por lista)"),
            prep$n_used, prep$n_total)
}

# -----------------------------------------------------------------------------
# .mk_default_minsize()
#
# Input: n - number of respondents.
# Output: mokken's default minimum size of a rest-score group: N/10 if
#   N >= 500, N/5 if 250 <= N < 500, max(N/3, 50) if N < 250. Computed here
#   only so the report can state the value actually used.
# ES: Salida: el tamaño mínimo por defecto de un grupo de rest-score en
#   mokken. Se calcula aquí solo para que el informe diga el valor usado.
# -----------------------------------------------------------------------------
.mk_default_minsize <- function(n) {
    if (n >= 500) return(floor(n / 10))
    if (n >= 250) return(floor(n / 5))
    floor(max(n / 3, 50))
}

# -----------------------------------------------------------------------------
# .mk_ci()
#
# Input: est, se - estimates and standard errors; level - confidence level
#   (0-1); type - "WB" (Wald-based) or "RP" (range-preserving).
# Output: list(lower =, upper =).
#
# mokken::coefH() fails with ci and nice.output = FALSE together (an internal
# object is not created in that branch, mokken 3.1.2), and its nice output
# only carries the interval as text. The intervals are therefore computed
# here with the formulas of Koopman, Zijlstra and van der Ark (2022): Wald
# H +/- z se, and range-preserving on log(1 - H), whose upper limit never
# exceeds 1. validation/ checks them against mokken's own printed intervals.
#
# ES: mokken::coefH() falla con ci y nice.output = FALSE juntos (un objeto
# interno no se crea en esa rama, mokken 3.1.2), y su salida formateada solo
# trae el intervalo como texto. Por eso los intervalos se calculan aquí con
# las fórmulas de Koopman, Zijlstra y van der Ark (2022): Wald H +/- z ee, y
# preservador de rango sobre log(1 - H), cuyo límite superior nunca pasa de
# 1. validation/ los contrasta con los intervalos impresos por mokken.
# -----------------------------------------------------------------------------
.mk_ci <- function(est, se, level = 0.95, type = "WB") {
    z <- stats::qnorm(1 - (1 - level) / 2)
    if (identical(type, "RP")) {
        lower <- 1 - exp(log(1 - est) + z * se / (1 - est))
        upper <- 1 - exp(log(1 - est) - z * se / (1 - est))
        # Boundary: at H = 1 the log(1 - H) scale is undefined. With a zero
        # standard error the interval collapses to the estimate; otherwise
        # the Wald limits are used, the upper one capped at 1.
        # ES: Frontera: con H = 1 la escala log(1 - H) no está definida.
        # Con error estándar cero el intervalo se reduce a la estimación; si
        # no, se usan los límites de Wald, con el superior acotado en 1.
        boundary <- is.finite(est) & est >= 1
        if (any(boundary)) {
            lower[boundary] <- ifelse(se[boundary] == 0, est[boundary], est[boundary] - z * se[boundary])
            upper[boundary] <- 1
        }
    } else {
        lower <- est - z * se
        upper <- est + z * se
    }
    degenerate <- is.finite(est) & is.finite(se) & se == 0
    lower[degenerate] <- est[degenerate]
    upper[degenerate] <- est[degenerate]
    list(lower = lower, upper = upper)
}

# -----------------------------------------------------------------------------
# .mk_h_label()
#
# Input: lang; h - a scalability coefficient.
# Output: Mokken's (1971) verbal label for the strength of a scale: below .3
#   unscalable, .3 to .4 weak, .4 to .5 moderate, .5 or higher strong.
# ES: Salida: la etiqueta verbal de Mokken (1971) para la fuerza de una
#   escala: menos de .3 no escalable, .3 a .4 débil, .4 a .5 moderada, .5 o
#   más fuerte.
# -----------------------------------------------------------------------------
.mk_h_label <- function(lang, h) {
    h <- .mk_clean_num(h)
    if (is.na(h)) return(.mk_text(lang, "common", "not_computable"))
    key <- if (h < 0.3) "h_unscalable" else if (h < 0.4) "h_weak" else if (h < 0.5) "h_moderate" else "h_strong"
    .mk_text(lang, "common", key)
}

# -----------------------------------------------------------------------------
# .mk_level_two()
#
# Input: prep; name - the cluster variable name (may be NULL).
# Output: the cluster codes aligned with prep$data rows as an integer vector,
#   or NULL. mokken expects a numeric vector, so factor levels are coded.
# ES: Salida: los códigos de conglomerado alineados con las filas de
#   prep$data como vector entero, o NULL. mokken espera un vector numérico,
#   así que los niveles de factor se codifican.
# -----------------------------------------------------------------------------
.mk_level_two <- function(prep, name) {
    if (is.null(name) || length(name) == 0 || is.null(prep$extra) || !(name %in% names(prep$extra)))
        return(NULL)
    values <- prep$extra[[name]]
    as.integer(factor(as.character(values)))
}

# -----------------------------------------------------------------------------
# .mk_plot_style() / .mk_plot_theme() / .mk_set_size()
#
# The shared plot settings of every MokkenLab analysis (Plot settings box):
# point size, line width, panels per row, legend position, x-axis label
# angle, y-axis range, value labels and image size. Every render function
# reads them through these helpers so all plots respond to the same controls
# in the same way. .mk_plot_theme() is added after jamovi's ggtheme: it only
# adjusts the legend and the x-axis labels, never replaces the theme.
#
# ES: Los ajustes de gráficos comunes a todos los análisis de MokkenLab
# (caja Ajustes de gráficos): tamaño de punto, grosor de línea, paneles por
# fila, posición de la leyenda, ángulo de las etiquetas del eje X, rango del
# eje Y, etiquetas de valor y tamaño de la imagen. Cada función de render los
# lee con estos helpers para que todos los gráficos respondan igual a los
# mismos controles. .mk_plot_theme() se agrega después del ggtheme de jamovi:
# solo ajusta la leyenda y las etiquetas del eje X, nunca reemplaza el tema.
# -----------------------------------------------------------------------------
.mk_plot_style <- function(options, n_panels = 1) {
    ncol <- as.integer(options$plotNcol)
    if (is.null(options$plotNcol) || is.na(ncol) || ncol <= 0)
        ncol <- min(3L, max(1L, as.integer(n_panels)))
    list(point = if (is.null(options$plotPointSize)) 2 else as.numeric(options$plotPointSize),
         line = if (is.null(options$plotLineWidth)) 0.8 else as.numeric(options$plotLineWidth),
         ncol = ncol,
         full = !identical(options$plotYRange, "data"),
         labels = isTRUE(options$plotValueLabels))
}

.mk_plot_theme <- function(options) {
    angle <- switch(if (is.null(options$plotAxisAngle)) "a45" else options$plotAxisAngle,
                    a0 = 0, a45 = 45, a90 = 90, 45)
    legend <- if (is.null(options$plotLegend)) "bottom" else options$plotLegend
    ggplot2::theme(
        legend.position = legend,
        axis.text.x = ggplot2::element_text(angle = angle, hjust = if (angle == 0) 0.5 else 1,
                                            vjust = if (angle == 90) 0.5 else 1))
}

.mk_set_size <- function(image, options, auto_width, auto_height) {
    width <- as.integer(options$plotWidth)
    height <- as.integer(options$plotHeight)
    if (is.null(options$plotWidth) || is.na(width) || width <= 0) width <- auto_width
    if (is.null(options$plotHeight) || is.na(height) || height <= 0) height <- auto_height
    image$setSize(width, height)
}

# -----------------------------------------------------------------------------
# .mk_panel_size()
#
# Input: n_panels; ncol; n_rows_each - rows of panels per facet group.
# Output: c(width, height) in pixels for a faceted plot, so panels keep a
#   readable size however many items are plotted.
# ES: Salida: c(ancho, alto) en píxeles para un gráfico con paneles, para que
#   los paneles conserven un tamaño legible sin importar cuántos ítems se
#   grafiquen.
# -----------------------------------------------------------------------------
.mk_panel_size <- function(n_panels, ncol, n_rows_each = 1) {
    ncol <- max(1, min(ncol, n_panels))
    nrow <- ceiling(n_panels / ncol) * n_rows_each
    c(max(450, 250 * ncol + 60), max(330, 220 * nrow + 110))
}

# -----------------------------------------------------------------------------
# .mk_failed_sections()
#
# Input: results - the analysis' results group; lang.
# Output: the titles of the visible tables that contain a "Not computable"
#   cell. Every analysis adds them to the key findings, so a section that
#   could not be computed is never hidden behind a reassuring "No warnings"
#   (independent audit, 2026-11-04). Scanning the finished tables catches
#   every Category B failure, including ones added in the future.
# ES: Salida: los títulos de las tablas visibles que contienen una celda "No
#   computable". Cada análisis los agrega a los hallazgos clave, para que una
#   sección que no pudo calcularse nunca quede oculta tras un "No hay
#   advertencias" tranquilizador (auditoría independiente, 2026-11-04).
# -----------------------------------------------------------------------------
.mk_failed_sections <- function(results, lang) {
    marker <- .mk_text(lang, "common", "not_computable")
    out <- character(0)
    for (name in results$itemNames) {
        item <- tryCatch(results$get(name), error = function(e) NULL)
        if (is.null(item) || !inherits(item, "Table"))
            next
        visible <- tryCatch(isTRUE(item$visible), error = function(e) FALSE)
        if (!visible)
            next
        df <- tryCatch(item$asDF, error = function(e) NULL)
        if (is.null(df) || nrow(df) == 0)
            next
        if (any(vapply(df, function(col) any(as.character(col) %in% marker), logical(1))))
            out <- c(out, item$title)
    }
    unique(out)
}

# -----------------------------------------------------------------------------
# .mk_reversed_label()
#
# Input: tr; prep.
# Output: the value of the "Negatively worded items (reversed)" summary row,
#   always shown: the reversed items, or an explicit "none" with a reminder
#   to confirm the scoring direction. Reversal is the data decision most
#   easily left inconsistent between analyses (independent audit,
#   2026-11-04), so it is never left implicit.
# ES: Salida: el valor de la fila "Ítems redactados en negativo
#   (invertidos)", que se muestra siempre: los ítems invertidos, o un
#   "ninguno" explícito con el recordatorio de confirmar la dirección de
#   puntuación. La inversión es la decisión de datos que más fácilmente queda
#   inconsistente entre análisis, así que nunca queda implícita.
# -----------------------------------------------------------------------------
.mk_reversed_label <- function(tr, prep) {
    if (length(prep$neg) > 0)
        return(paste(prep$neg, collapse = ", "))
    tr("None. Confirm that every item is scored in the same direction, and use the same reversals in every MokkenLab analysis of these items.",
       "Ninguno. Confirme que todos los ítems están puntuados en la misma dirección, y use las mismas inversiones en todos los análisis de MokkenLab con estos ítems.")
}

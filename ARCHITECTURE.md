# MokkenLab Architecture

**Version:** 1.0 (2026-11-04: adopted from EGAnetLab's ARCHITECTURE.md v1.0,
itself adopted from AssumptionsLab's v1.2, keeping every architectural rule
that came out of jamovi's official review of AssumptionsLab; sections 6, 10,
11, 14 and 16 rewritten for Mokken scale analysis with mokken)  
**Project:** MokkenLab  
**License:** GNU General Public License v3.0  
**Author:** Arquímedes De León Chacón Chacón

---

# Table of Contents

1. Introduction
2. Architectural Philosophy
3. High-Level Architecture
4. Directory Structure
5. Module Architecture
6. Data Flow
7. Analysis Lifecycle
8. Report Generation Architecture
9. Interpretation Engine
10. Graphical Architecture
11. Methodological Library
12. Internationalization
13. Documentation Architecture
14. Future Expansion
15. Design Principles

16. mokken-Specific Architecture

---

# 1. Introduction

MokkenLab has been designed as a modular scientific software platform for
evaluating psychological scales with Mokken scale analysis (nonparametric
item response theory), and supporting methodological decision making.

The architecture prioritizes

• scientific rigor;

• modularity;

• maintainability;

• educational value;

• reproducibility.

Every component has a clearly defined responsibility.

-------------------------------------------------------------------------------

# Introducción

MokkenLab ha sido diseñado como una plataforma modular para la evaluación
de escalas psicológicas con el análisis de escalamiento de Mokken (teoría de
respuesta al ítem no paramétrica) y el apoyo a la toma de decisiones
metodológicas.

La arquitectura prioriza

• rigor científico;

• modularidad;

• mantenibilidad;

• valor educativo;

• reproducibilidad.

Cada componente posee una responsabilidad claramente definida.

-------------------------------------------------------------------------------

# 2. Architectural Philosophy

The architecture follows five fundamental principles.

### Separation of responsibilities

Each file performs one well-defined task.

### Progressive workflow

Every analysis follows the same logical sequence.

### Educational software

The software explains statistical decisions rather than merely producing
results.

### Scientific transparency

Every methodological decision can be identified inside the source code.

### Scalability

New analyses should integrate without modifying the existing architecture.

-------------------------------------------------------------------------------

# Filosofía arquitectónica

Cada nuevo módulo debe adaptarse a la arquitectura existente.

La arquitectura nunca debe adaptarse a un módulo específico.

-------------------------------------------------------------------------------

# 3. High-Level Architecture

```
                USER

                  │

                  ▼

        User Interface (.yaml)

                  │

                  ▼

        Analysis Engine (.R)

                  │

                  ▼

    Statistical Computations

                  │

                  ▼

      Diagnostic Evaluation

                  │

                  ▼

 Methodological Interpretation

                  │

                  ▼

        Report Generation

                  │

                  ▼

            Final Output
```

-------------------------------------------------------------------------------

# Arquitectura general

Toda información sigue un flujo unidireccional.

No existen ciclos innecesarios.

-------------------------------------------------------------------------------

# 4. Directory Structure

```
MokkenLab/

│

├── R/

├── jamovi/

│   └── assets/

├── inst/

│   └── assets/

├── data/

├── data-raw/

├── docs/

├── tests/

├── .github/

├── README.md

├── LICENSE

├── CODE_STYLE.md

├── DEVELOPER_GUIDE.md

├── ARCHITECTURE.md

├── NEWS.md

├── DESCRIPTION

└── NAMESPACE
```

-------------------------------------------------------------------------------

## Responsibilities

### R/

Implements every statistical algorithm. `R/shared-helpers.R` holds every
small function (`.mk_*()`) duplicated identically across two or more
analysis files — translation, HTML block builders, row-adding wrappers,
number cleaning, plot palettes and shared plot settings, item preparation
(integer scores, two-way imputation, reversal) and mokken error capture.
A new duplicate belongs there, not copy-pasted into a third file. See
DEVELOPER_GUIDE.md §13.

### jamovi/

Defines analysis options and user interfaces. Also holds `jamovi/assets/`,
the module's own icon.

### inst/

Package-installed resources: icons/logos and the plain-text `CITATION` file.

### data/ and data-raw/

`data/` ships the four bundled example datasets shown inside jamovi
(registered under `datasets:` in `jamovi/0000.yaml`); `data-raw/` holds the
script that produces them reproducibly from the mokken package.

### docs/

Contains project documentation.

### tests/

Stores validation procedures.

-------------------------------------------------------------------------------

# 5. Module Architecture

Every analysis consists of four primary components.

```
analysis.a.yaml

↓

analysis.u.yaml

↓

analysis.r.yaml

↓

analysis.b.R
```

-------------------------------------------------------------------------------

## analysis.a.yaml

Defines

• options

• variables

• controls

• defaults

-------------------------------------------------------------------------------

## analysis.u.yaml

Defines

• layout

• groups

• visibility

• interface organization

-------------------------------------------------------------------------------

## analysis.r.yaml

Defines

• result tables

• images

• HTML outputs

• textual reports

-------------------------------------------------------------------------------

## analysis.b.R

Implements

• validation

• computations

• diagnostics

• graphics

• interpretations

• reporting

-------------------------------------------------------------------------------

# 6. Data Flow

```
Item Selection

↓

Input Validation (Category A: enough items, no empty, constant or
non-integer item, a declared score range that contains every score)

↓

Missing Data Handling (listwise deletion or mokken::twoway() imputation,
chosen by the user, reported with counts and seed)

↓

Reversal of negatively worded items (declared min + max - score, as
mokken::recode())

↓

mokken call wrapped in .mk_capture() (errors -> reject or Category B,
warnings -> translated Notes, console output discarded)

↓

Tables (scalability, selection, assumption checks, two-level coefficients)

↓

Teaching guides, plots (state = what is drawn only)

↓

Interpretation Engine (four questions) and key findings

↓

Output
```

-------------------------------------------------------------------------------

# Flujo de datos

Cada etapa depende únicamente de la anterior.

No deben existir cálculos redundantes: cada objeto devuelto por mokken se
calcula una sola vez por ejecución y se reutiliza para tablas, gráficos e
interpretación. mokken no acepta valores perdidos ni puntajes no enteros, así
que esas dos decisiones se toman antes de cualquier llamada y siempre se
informan.

-------------------------------------------------------------------------------

# 7. Analysis Lifecycle

Every module should follow exactly the same lifecycle.

```
Initialization

↓

Validation

↓

Preparation

↓

Analysis

↓

Diagnostics

↓

Interpretation

↓

Recommendations

↓

Report

↓

Finish
```

This workflow must remain consistent throughout the project.

-------------------------------------------------------------------------------

# Ciclo de vida del análisis

La experiencia del usuario debe ser idéntica en todos los módulos.

-------------------------------------------------------------------------------

# 8. Report Generation Architecture

Reports are generated after every statistical computation has been completed.

The report is composed of

Introduction

↓

Descriptive Statistics

↓

Assumption Diagnostics

↓

Interpretation

↓

Recommendations

↓

References

Each section should be generated independently.

A report is only ever fully blocked (`jmvcore::reject()`) by a condition
that prevents the whole analysis from running. A single failed diagnostic
test never blocks the report — its row always renders, tiered "Not
computable" with a specific footnote. See CODE_STYLE.md §19.1.

-------------------------------------------------------------------------------

# Arquitectura del informe

La generación del informe nunca debe mezclarse con los cálculos estadísticos.

Un informe solo queda completamente bloqueado (`jmvcore::reject()`) por
una condición que impide ejecutar todo el análisis. Una única prueba
diagnóstica fallida nunca bloquea el informe — su fila siempre se
renderiza, como "No computable" con una nota al pie específica. Ver
CODE_STYLE.md §19.1.

-------------------------------------------------------------------------------

# 9. Interpretation Engine

The interpretation engine represents one of the most important components of
MokkenLab.

Its objective is to transform statistical outputs into methodological
recommendations.

```
Statistical Results

↓

Methodological Rules

↓

Interpretation Templates

↓

Educational Explanation

↓

Recommendations
```

The engine should never merely reproduce numerical values.

-------------------------------------------------------------------------------

# Motor de interpretación

Toda interpretación debe responder

¿Qué ocurrió?

¿Por qué?

¿Qué significa?

¿Qué debe hacer ahora el investigador?

-------------------------------------------------------------------------------

# 10. Graphical Architecture

Graphs are organized according to methodological purpose.

```
Scalability (Hi with intervals, Hij heat map, Hi by group)

↓

Item selection (partition across lower bounds)

↓

Assumptions (violation profile, item response functions, item pairs,
P-matrix)

↓

Two-level scalability (within and between) and norms
```

mokken's own plot methods (plot.monotonicity.class, plot.iio.class,
plot.restscore.class, plot.pmatrix.class) draw with base graphics and ask the
user to press a key between panels. MokkenLab rebuilds every one of them in
ggplot from the same result matrices, so they follow jamovi's Theme/Palette
through `ggtheme`, show all panels at once, and survive export (render
functions read only `image$state`, which holds only what is drawn). Rest
scores are plotted on a numeric axis (the midpoint of each rest-score group)
because each item forms different groups; a shared categorical axis would
misplace them.

Every plot reads one shared set of plot settings through `.mk_plot_style()`,
`.mk_plot_theme()` and `.mk_set_size()`: width and height (or automatic,
scaled to the number of panels), point size, line width, panels per row,
legend position, x-axis label angle, y-axis range (full theoretical range or
fitted to the data) and value labels. Colours come from jamovi's own palette
through `.mk_global_palette()` (`jmvcore::colorPalette()`), so plots match
every other module; there is no module palette option. mokken's own plot
arguments are exposed: items and item pairs (Items to plot), curves (IRF,
ISRFs or both), ci, alpha and transparancy (band opacity); mokken's `color`
argument is covered by jamovi's palette.

-------------------------------------------------------------------------------

# Arquitectura gráfica

Los gráficos constituyen herramientas metodológicas.

No elementos decorativos.

Los métodos plot de mokken dibujan con gráficos base y piden pulsar una tecla
entre paneles. MokkenLab los reconstruye todos en ggplot a partir de las
mismas matrices, para que sigan el Tema/Paleta de jamovi vía `ggtheme`,
muestren todos los paneles a la vez y sobrevivan a la exportación (las
funciones de render leen solo `image$state`). Los rest scores se grafican en
un eje numérico (el punto medio de cada grupo) porque cada ítem forma grupos
distintos; un eje categórico común los ubicaría mal.

Todos los gráficos leen un mismo conjunto de ajustes mediante
`.mk_plot_style()`, `.mk_plot_theme()` y `.mk_set_size()`: ancho y alto (o
automático), tamaño de punto, grosor de línea, paneles por fila, leyenda,
ángulo del eje X, rango del eje Y y etiquetas de valor. Los colores vienen de
la paleta de jamovi mediante `.mk_global_palette()`
(`jmvcore::colorPalette()`), así que coinciden con los de los demás módulos;
no hay opción de paleta propia. Los argumentos de los métodos plot de mokken
están expuestos, y `color` lo cubre la paleta de jamovi.

-------------------------------------------------------------------------------

# 11. Methodological Library

Teaching lives inside every analysis rather than in a separate library
analysis: each table is preceded by a "How to read" block (option "How to
read each table" in the Report Content box, off by default so the output
stays compact) that says what the statistic measures, how
it works, what values to expect and the most common misreading; an "About
your choices" block explains every decision currently selected and when an
alternative fits better; and the interpretation answers the four questions
of §9. All of this is text in `texts.R` (sections `guide`, `choices` and one
per analysis), wrapped in `.()` and translated through jamovi's catalog
(§12).

Per-analysis citations use jamovi's native mechanism (`jamovi/00refs.yaml` +
`refs:` on each `Table`). The complete APA 7th edition reference list lives in
`docs/Bibliography.md`. There is no Bibliography analysis in the menu.

-------------------------------------------------------------------------------

# Biblioteca metodológica

La enseñanza vive dentro de cada análisis y no en un análisis de biblioteca
aparte: cada tabla va precedida de un bloque "Cómo leer" (opción de la caja Contenido del informe,
desactivada por defecto para que la salida sea compacta) que dice qué mide el estadístico, cómo funciona, qué valores esperar
y la lectura errónea más común; un bloque "Acerca de sus elecciones" explica
cada decisión seleccionada y cuándo conviene otra; y la interpretación
responde las cuatro preguntas de §9. Todo es texto de `texts.R`, envuelto en
`.()` y traducido con el catálogo de jamovi (§12).

Las citas por análisis usan el mecanismo nativo de jamovi
(`jamovi/00refs.yaml` + `refs:` en cada `Table`). La lista completa en APA 7.ª
edición vive en `docs/Bibliography.md`.

-------------------------------------------------------------------------------

# 12. Internationalization

MokkenLab follows a bilingual philosophy, implemented as two separate
mechanisms that must not be confused with one another.

```
Source code                    Interface and report
comments                       (jamovi i18n)

English, then a Spanish        Follows jamovi's own language setting.
"# ES:" block below            jamovi/i18n/catalog.pot + es.po hold:
(CODE_STYLE.md)                 - every title/description/label in
                                  .a.yaml/.u.yaml/.r.yaml/0000.yaml,
                                  extracted with no markup;
                                - every .("...") string in R/*.R
                                  (texts.R, *.b.R, shared-helpers.R).
                               inst/i18n/es.json is built from es.po.
```

Both the interface and the report use jamovi's own catalog, not a custom
reimplementation. The compiler extracts the yaml strings and every `.()`
literal in the R sources; `jmvtools::i18nUpdate("es")` re-syncs `es.po` as
strings change, and `jmvtools::prepare()` writes `inst/i18n/es.json`. Because
gettext's model is one source string to one translation, every source string
is written in English; when one English word needs two Spanish translations,
it carries a context (`.("Wald-based [inline]")`).

Report text lives in `R/texts.R` as `.mk_text_table(.)`, a function of the
translator, so every entry is a `.()` literal the compiler can see. Each run
builds it once with `.mk_lang(options)` and reads it with `.mk_text(lang,
section, key)`.

`.()` does not freeze. `jmvcore::Options$translate()` creates its translator
for each `Options` object, from the language jamovi sends with each run, and
`createTranslator()` caches one translator per package and language. A change
of jamovi's language therefore reaches the next run without a restart.
`tests/testthat/test-msa.R` runs the same analysis in Spanish, English and
Spanish again in one process. An earlier version of this section said the
opposite and kept a per-analysis `reportLang` option; reading jmvcore 2.7.38
showed that claim was wrong, and the option was removed in 1.0.0.

A future language needs only one more `.po` file.

The module has no language option of its own: to read the results in
another language, the user changes jamovi's language in its settings.

-------------------------------------------------------------------------------

# Internacionalización

La arquitectura de MokkenLab sigue una filosofía bilingüe, implementada como
dos mecanismos separados que no deben confundirse entre sí.

```
Comentarios de                 Interfaz e informe
código fuente                  (i18n de jamovi)

Inglés, seguido de un          Sigue el ajuste de idioma de jamovi.
bloque "# ES:" en español      jamovi/i18n/catalog.pot + es.po guardan:
(CODE_STYLE.md)                 - cada title/description/label de
                                  .a.yaml/.u.yaml/.r.yaml/0000.yaml,
                                  extraídos sin ninguna marca;
                                - cada string .("...") de R/*.R
                                  (texts.R, *.b.R, shared-helpers.R).
                               inst/i18n/es.json se genera desde es.po.
```

La interfaz y el informe usan el catálogo propio de jamovi, no una
reimplementación. El compilador extrae los strings del yaml y cada literal
`.()` de los fuentes R; `jmvtools::i18nUpdate("es")` resincroniza `es.po`
cuando cambian los strings, y `jmvtools::prepare()` escribe
`inst/i18n/es.json`. Como el modelo de gettext es un string fuente por cada
traducción, todo string fuente se escribe en inglés; cuando una palabra
inglesa necesita dos traducciones, lleva un contexto
(`.("Wald-based [inline]")`).

El texto del informe vive en `R/texts.R` como `.mk_text_table(.)`, una
función del traductor, así que cada entrada es un literal `.()` visible para
el compilador. Cada ejecución lo construye una vez con `.mk_lang(options)` y
lo lee con `.mk_text(lang, section, key)`.

`.()` no se congela. `jmvcore::Options$translate()` crea su traductor para
cada objeto `Options`, con el idioma que jamovi envía en cada ejecución, y
`createTranslator()` guarda un traductor por paquete e idioma. Un cambio del
idioma de jamovi llega a la siguiente ejecución sin reiniciar.
`tests/testthat/test-msa.R` ejecuta el mismo análisis en español, inglés y de
nuevo español en un solo proceso. Una versión anterior de esta sección decía
lo contrario y mantenía una opción `reportLang` por análisis; la lectura de
jmvcore 2.7.38 mostró que esa afirmación era errónea, y la opción se quitó en
la versión 1.0.0.

Un idioma futuro solo necesita un archivo `.po` más.

El módulo no tiene opción de idioma propia: para leer los resultados en otro
idioma, el usuario cambia el idioma de jamovi en sus preferencias.

-------------------------------------------------------------------------------

# 13. Documentation Architecture

Documentation exists at four levels.

```
Repository

↓

Module

↓

Source File

↓

Function
```

Each level should answer progressively more detailed questions.

Repository

What is MokkenLab?

Module

What analysis is implemented?

Source File

How is the analysis organized?

Function

How is each task performed?

-------------------------------------------------------------------------------

# Arquitectura documental

La documentación constituye un componente de la arquitectura.

No un elemento accesorio.

-------------------------------------------------------------------------------

# 14. Future Expansion

Every capability of mokken 3.1.2 is already mapped to one analysis:

| Analysis | mokken functions |
| --- | --- |
| Mokken Scale Analysis (`msa`) | `coefH`, `coefZ`, `check.reliability`, `check.errors`, `check.bounds`, `check.norms`, `twoway`, `recode` |
| Automated Item Selection (`aisp`) | `aisp` (normal and genetic algorithm, single and sequence of lower bounds) |
| Model Assumptions (`msacheck`) | `check.monotonicity`, `check.iio` (MIIO, MSCPM, IT; estimated, listed, clustered or typed order), `check.restscore`, `check.pmatrix`, `check.ca`, and the plot methods of their classes |
| Two-Level Mokken Scale Analysis (`mlmsa`) | `MLcoefH`, `MLcoefZ`, `ICC`, `MLweight` |

Deliberately not exposed, each for a stated reason: `aisp(search =
"extended")` (an unfinished stub in mokken 3.1.2 that never receives the
items and fails on any data), the `verbose` arguments (console progress
only), the `ask`/`color` arguments of the plot methods (jamovi shows every
panel; colors follow its theme), and `fixed.itemstep.order` /
`itemstep.order` (a matrix of item-step ranks that cannot be typed in an
option panel; mokken's default, the estimated order, is used). A new mokken
function is added as options and results of the analysis it belongs to, or
as a new analysis, without structural changes.

-------------------------------------------------------------------------------

# Expansión futura

Cada capacidad de mokken 3.1.2 ya está asignada a un análisis (tabla de
arriba). Lo no expuesto lo está por una razón declarada. Una función nueva de
mokken se agrega como opciones y resultados del análisis al que pertenece, o
como un análisis nuevo, sin cambios estructurales.

-------------------------------------------------------------------------------

# 15. Design Principles

Every architectural decision should satisfy the following principles.

### Scientific

Algorithms should faithfully implement accepted statistical procedures.

### Educational

Users should understand every methodological decision.

### Modular

Components should remain independent whenever possible.

### Transparent

Every important decision should be documented.

### Maintainable

Future developers should understand the architecture without external guidance.

### Consistent

Every module should behave as part of the same software ecosystem.

### Reproducible

Analyses should produce reproducible results from identical data.

-------------------------------------------------------------------------------

# Principios de diseño

La arquitectura de MokkenLab pretende equilibrar

ingeniería del software,

metodología estadística,

experiencia del usuario

y

valor educativo.

Estos principios deberán preservarse durante toda la evolución del proyecto.

-------------------------------------------------------------------------------

# 16. mokken-Specific Architecture

Four conditions shape every MokkenLab analysis.

**Data requirements.** mokken refuses missing values and needs integer item
scores. `.mk_prepare_items()` therefore rejects non-integer items (Category
A, never rounded silently), applies the missing-data rule the user chose
(listwise deletion, or two-way imputation with a visible seed and an optional
number of completed data sets whose spread is reported), and reverses
negatively worded items over the declared or observed score range.

**Computation time.** `check.ca()`, `check.iio()` with many items (TROG: 80
items), the genetic algorithm and the bootstrap of the AISP partition can take
tens of seconds. Each runs only when its checkbox is ticked. jamovi re-runs
`.run()` on every option change, so every mokken call goes through
`.mk_cached_capture()`: results are kept in a hidden `cacheStore` element,
keyed by a hash of the data and of every argument (a cached result is never
reused for other data or options). Only the parts of each result that the
report reads are stored (the `extract` argument, built with `.mk_keep()`), and
the store is capped at 8 entries and 2 MB because it is saved with the .omv
file. A cosmetic change (plot size, labels, guides) therefore recomputes
nothing.

**Reproducibility record.** Every analysis ends with the software versions,
the seeds and the equivalent R code with the exact mokken calls it made
(`.mk_repro_block()`), checked by the tests to parse and, in validation, to
reproduce the results in a clean R session.

**Reproducibility.** Two-way imputation and the genetic algorithm are
stochastic. Both expose their seed in the interface with a fixed default; the
genetic algorithm, which has no seed argument in mokken, runs under
`set.seed()` with R's previous random state restored afterwards.

**Known quirks of mokken 3.1.2, handled explicitly.** `coefH()` fails when
`ci` and `nice.output = FALSE` are combined, so intervals are computed in
`.mk_ci()` with the published formulas and validated against mokken's printed
ones (validation/). `check.bounds()` can return upper bounds outside [-1, 1];
those are shown but left out of the verdict, with a footnote. Result columns
are named after item positions (E(X3)), so they are located by position.
Several functions print to the console; `.mk_capture()` discards that output. `check.ca()` returns
`list(InScale, Index, Flagged)` with `Windex = TRUE` but the bare list of
iterations with `Windex = FALSE`; both are normalized. `MLweight()` fails
inside mokken for some item pairs with tied item-step popularities; the row
explains it. `MLcoefH()` names item pairs by pasting item names with a space,
so pairs are identified by position (`combn()` order), never by splitting
names.

-------------------------------------------------------------------------------

# Arquitectura específica de mokken

Cuatro condiciones dan forma a cada análisis de MokkenLab: requisitos de datos
(sin perdidos, puntajes enteros, inversión sobre el rango declarado), tiempo
de cómputo (cada verificación lenta solo corre si se marca), reproducibilidad
(semillas visibles para la imputación y el algoritmo genético) y peculiaridades
conocidas de mokken 3.1.2 tratadas explícitamente (intervalos de coefH,
cotas superiores imposibles, columnas nombradas por posición, salida de
consola).

-------------------------------------------------------------------------------

# Final Statement

MokkenLab is designed to make Mokken scale analysis teachable: every
scaling decision it reports can be traced to its source literature and
explained in plain language.

-------------------------------------------------------------------------------

# Declaración final

MokkenLab está diseñado para que el análisis de escalamiento de Mokken pueda
enseñarse: cada decisión de escalamiento que informa puede rastrearse hasta su
literatura de origen y explicarse en lenguaje claro.

-------------------------------------------------------------------------------


**End of document**

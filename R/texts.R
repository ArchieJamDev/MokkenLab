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
# Text repository.
# ES: Repositorio de textos.
#
# Every user-facing report string that is assembled at runtime lives here,
# keyed by (section, key), written in English inside jamovi's `.()` so the
# jamovi compiler extracts it into the same catalog as the interface text
# (jamovi/i18n/es.po; `jmvtools::i18nUpdate("es")` adds new strings). Report
# text follows jamovi's own language, like every other analysis (jamovi
# audit, 2026-11-05). Adding a language needs only one more .po file.
#
# .mk_text_table() takes the translator of the current run (`.`, see
# .mk_lang() in shared-helpers.R) and returns the translated table; it is
# built once per run and kept in `lang`.
#
# ES: Todo texto del informe que se arma en tiempo de ejecución vive aquí,
# indexado por (sección, clave), escrito en inglés dentro de `.()` de jamovi
# para que el compilador de jamovi lo extraiga al mismo catálogo que el
# texto de la interfaz (jamovi/i18n/es.po; `jmvtools::i18nUpdate("es")`
# agrega los textos nuevos). El texto del informe sigue el idioma de jamovi,
# como en cualquier otro análisis (auditoría de jamovi, 2026-11-05). Agregar
# un idioma solo requiere otro archivo .po.
#
# .mk_text_table() recibe el traductor de la ejecución actual (`.`, ver
# .mk_lang() en shared-helpers.R) y devuelve la tabla traducida; se arma una
# vez por ejecución y se guarda en `lang`.
#
# Writing rule: every interpretation answers four questions (what happened,
# why, what it means, what the researcher should do now). Never write a
# bracketed reference number; append "see references" after an author-year
# mention (CODE_STYLE.md §21).
#
# ES: Regla de redacción: cada interpretación responde cuatro preguntas (qué
# ocurrió, por qué, qué significa, qué debe hacer ahora el investigador).
# Nunca escribir un número de referencia entre corchetes; agregar "see
# references" después de una mención autor-año (CODE_STYLE.md §21).
# -----------------------------------------------------------------------------

.mk_text_table <- function(.) list(

    common = list(
        no_data = .("No data are available."),
        too_few_items = .("Select at least two items. Scalability coefficients describe how consistently items order respondents, which needs at least one pair of items."),
        too_few_items_3 = .("Select at least three items. This check compares each item with the rest of the scale, which needs at least three items."),
        empty_item = .("The following items have fewer than two observed scores and cannot be analyzed: %s. Remove them from the analysis."),
        constant_item = .("The following items have zero variance in the cases analyzed (every respondent gave the same score): %s. A constant item cannot order respondents, so its scalability is undefined. Remove it from the analysis."),
        non_integer = .("The following items have non-integer scores: %s. Mokken scale analysis works with ordered item scores (0, 1, 2, ...). Check that these variables are item scores and not scale means or other derived values."),
        bad_range = .("The declared score range (%s to %s) does not contain every observed score (observed range %s to %s), or its lowest value is not below its highest. Correct the declared range, or use the range observed in the data."),
        twoway_failed = .("Two-way imputation could not complete the data: %s. Switch Missing values to listwise deletion, or review the missing-data pattern."),
        too_few_cases = .("Fewer than two cases remain after removing incomplete cases. Switch Missing values to two-way imputation, or review the missing-data pattern."),
        estimation_failed = .("MokkenLab could not compute the scalability coefficients: %s. This usually means an item has scores in only one category for every pair of items, or a variable that is not an item score was selected. Check the selected items and their scores."),
        warnings_title = .("Messages from MokkenLab"),
        interp_title = .("Applied interpretation"),
        key_failed = .("%s: could not be computed, see the footnote of that table. Results that could not be computed are not evidence in favor of the scale."),
        not_computable = .("Not computable"),
        h_unscalable = .("unscalable (H < .30)"),
        h_weak = .("weak scale (.30 ≤ H < .40)"),
        h_moderate = .("moderate scale (.40 ≤ H < .50)"),
        h_strong = .("strong scale (H ≥ .50)")
    ),

    # Known mokken notices, rewritten in the report language.
    # ES: Avisos conocidos de mokken, reescritos en el idioma del informe.
    notices = list(
        varying_categories = .("Not every item uses the same number of score categories in these data. Either some categories were never chosen in this sample, or the items have different numbers of categories by design; in the latter case the total score cannot be used as an ordinal measurement (mokken notice)."),
        typez_wb = .("With nested data, mokken tests the coefficients with the Wald-based Z test; the original Z test was replaced by it."),
        typez_wb_testhi = .("Testing Hi against the lower bound needs the Wald-based Z test, so mokken replaced the original Z test by it.")
    ),

    key = list(
        title = .("Key findings"),
        intro = .("A one-line summary of the results below, each line naming the section it comes from. Read the sections for the details and the reasons."),
        h_findings = .("Findings"),
        h_uncertain = .("Points of uncertainty"),
        h_warnings = .("Warnings"),
        none_findings = .("No findings to report."),
        none_uncertain = .("No points of uncertainty were detected."),
        none_warnings = .("No warnings.")
    ),

    # Explanation of every decision shared across analyses or specific to
    # one. Each paragraph says what the choice does and when to pick an
    # alternative.
    # ES: Explicación de cada decisión común o propia de un análisis.
    # Cada párrafo dice qué hace la elección y cuándo conviene otra.
    choices = list(
        title = .("About your choices"),
        intro = .("Each decision below is explained for the option currently selected, with guidance on when an alternative would fit your data better."),
        h_na = .("Missing values"),
        na_listwise = .("Listwise deletion keeps only respondents who answered every selected item, because mokken does not accept missing scores. It is appropriate when few cases are incomplete and missingness is unrelated to the trait. With many incomplete cases it wastes information and may bias the results; two-way imputation is then preferable."),
        na_twoway = .("Two-way imputation fills each missing score from the respondent's mean, the item's mean and the overall mean, plus a random error (van Ginkel et al., 2007; see references). In simulations it preserved scalability coefficients and item selection well when up to about 10%% of the scores were missing at random. MokkenLab produced %d completed data set(s) with seed %d; the same seed and data always reproduce the same imputation. With several sets, the main results come from the first and a table shows how much H varies between them."),
        h_range = .("Score range"),
        range_observed = .("The lowest and highest scores observed in the data define the score range used to reverse negatively worded items and to bound imputed scores. If an extreme category was never chosen in this sample, declare the range instead, so reversal maps each score to its true mirror."),
        range_manual = .("The range you declared defines how negatively worded items are reversed (lowest + highest - score) and bounds imputed scores. Declare the range of the response format, for example 0 to 4 for a five-point item scored from 0."),
        h_neg = .("Negatively worded items"),
        neg_body = .("These items were reversed before the analysis (lowest + highest - score, as mokken::recode()), so that a higher score means more of the attribute on every item. Unreversed negatively worded items produce negative scalability coefficients and break the scale."),
        h_ci = .("Confidence intervals"),
        ci_WB = .("Wald-based intervals are the estimate plus and minus a normal quantile times the standard error, with standard errors from the marginal-modelling approach (Kuijpers et al., 2013; see references). They are symmetric and can exceed 1 for coefficients near the ceiling; then prefer range-preserving intervals."),
        ci_RP = .("Range-preserving intervals are built on the log(1 - H) scale and transformed back, so they never exceed 1 and are asymmetric near the ceiling (Koopman et al., 2021; see references). They are preferable for strong scales or small samples, where Wald-based intervals can cross 1."),
        h_z = .("Z test"),
        z_Z = .("The original Z test (Mokken, 1971; see references) tests whether each coefficient is larger than zero. It cannot test against a lower bound such as .30 (the value %s you typed is used only by the other tests); choose the Wald-based or range-preserving test to check whether coefficients exceed a lower bound."),
        z_WB = .("The Wald-based test divides the difference between each coefficient and the lower bound %s by its standard error. A significant one-sided result means the coefficient is larger than the bound, which is stronger evidence than a point estimate above it (Koopman et al., 2022; see references)."),
        z_RP = .("The range-preserving test compares log(1 - H) with log(1 - %s), using a standard error on that scale, so it stays accurate near the ceiling (Koopman et al., 2021; see references). Prefer it to the Wald-based test for strong scales or small samples."),
        h_cluster = .("Nested data"),
        cluster_body = .("Respondents are nested in the clusters of this variable. The coefficients are estimated as usual, but their standard errors, intervals and tests take the dependence between respondents of the same cluster into account (Koopman et al., 2020, 2022; see references). Ignoring nesting makes standard errors too small and tests too liberal."),
        h_search = .("Search procedure"),
        search_normal = .("Mokken's procedure starts the first scale with the pair of items with the highest Hij, then adds, one at a time, the item that most increases H, as long as the item's Hi stays at or above c and its scalability is significant. When no item can be added it starts a second scale with the remaining items, and so on. It is fast and transparent, but greedy: an early choice cannot be undone, so it may miss the best partition."),
        search_ga = .("The genetic algorithm searches many candidate partitions at once, recombining and mutating them across generations, and keeps the partition with the largest scales that meet the criteria (Straat et al., 2013; see references). It is less likely than Mokken's procedure to get trapped by an early choice, at the price of computing time and some randomness; the seed makes the result reproducible. Compare both: agreement strengthens the conclusion."),
        h_lb = .("Lower bound c"),
        lb_single = .("Every item admitted to a scale must have Hi of at least c (and the scale H of at least c). The usual value is .30, Mokken's minimum for a scale; higher values give fewer, stronger and shorter scales."),
        lb_sequence = .("Running the selection for a sequence of c values, rather than one, shows how the partition changes as the criterion tightens. This is the exploration recommended by Hemker et al. (1995; see references) and by the tutorial of Sijtsma and van der Ark (2017; see references), because one c value can hide whether the items measure one attribute or several."),
        testhi_false = .("Items are admitted when their point estimate Hi reaches c and Hi is significantly larger than zero. This is Mokken's original rule."),
        testhi_true = .("Items are admitted only when Hi is significantly larger than c, not just when its point estimate reaches c (Koopman et al., 2022; see references). This is stricter and protects against admitting items whose Hi exceeds c only by sampling error, especially in small samples."),
        h_start = .("Start set"),
        start_body = .("The first scale starts from the items you chose instead of from the pair with the highest Hij. Use it to build a scale around items that define the attribute theoretically, and check that the result does not depend on that choice by running the selection without a start set."),
        h_weigh = .("Group proportions"),
        weigh_true = .("The item-score proportions of each subject are weighted by the number of raters, so subjects rated by many raters count more. This is mokken's current default, which corrects a bias of averaged proportions when group sizes differ (Koopman et al., 2022; see references)."),
        weigh_false = .("The item-score proportions of each subject are averaged, so every subject counts equally whatever its number of raters. This reproduces the original estimator of Snijders (2001) and Koopman et al. (2020); with unequal group sizes the weighted version is preferred."),
        h_minsize = .("Minimum rest-score group size"),
        minsize_body = .("Respondents are grouped by their rest score (total score without the item checked), and adjacent rest scores are merged until each group has at least this many respondents. Larger groups give more stable proportions but fewer points on each curve. mokken's default grows with the sample: N/10 from 500 respondents, N/5 from 250, and max(N/3, 50) below 250."),
        h_minvi = .("Minimum violation"),
        minvi_body = .("A decrease of the response function smaller than this value is ignored as negligible; larger ones are counted and tested. The default .03 follows Molenaar and Sijtsma (2000; see references). Lowering it makes the check more sensitive and flags more, mostly trivial, violations."),
        h_iio_method = .("Invariant item ordering method"),
        iio_MIIO = .("Manifest invariant item ordering (MIIO) compares the mean scores of every pair of items in each rest-score group: if item A is easier than item B for the whole sample, its mean should be higher in every group (Ligtvoet et al., 2010; see references). It is the standard method for polytomous items."),
        iio_MSCPM = .("The manifest scale cumulative probability model (MSCPM) compares the item step response functions instead of the item means, so it checks a stronger property: that every item step keeps its order (Ligtvoet et al., 2011; see references). Use it when you need the ordering at the level of response categories."),
        iio_IT = .("Increasingness in transposition (IT) checks the ordering through the joint distribution of each pair of items (Ligtvoet et al., 2011; see references). It is the most demanding property and needs large samples."),
        h_iio_order = .("Item order"),
        order_estimated = .("The item order is estimated from the item means in this sample, which is the usual choice when no ordering is hypothesized."),
        order_listed = .("The item order is fixed as the order of the item list, as a hypothesis to test (for example, items designed from easy to hard). Items listed first are expected to be the most popular."),
        order_clusters = .("Consecutive items form clusters with a tied position in the order, and the analysis checks the ordering between clusters as well as between items, as in the clustered-item analysis of the TROG data (Koopman & Braeken). Use it when items come in blocks that share a stimulus or difficulty level."),
        order_custom = .("The order codes you typed fix the position of each item; equal codes form clusters of tied items. Use it to test a hypothesized order that is not the list order."),
        h_ca = .("Conditional association"),
        ca_body = .("Local independence is checked through three special cases of conditional association (Holland & Rosenbaum, 1986; see references): covariances between items must not be negative conditional on sum scores of other items. The worst offending item is removed and the check repeated, until no item is flagged (Straat et al., 2016; see references).")
    ),

    guide_titles = list(
        scale = .("How to read scalability"), hij = .("How to read item pairs"), reliability = .("How to read reliability"),
        errors = .("How to read Guttman errors"), bounds = .("How to read correlation bounds"),
        norms = .("How to read norm statistics"), aisp = .("How the automated item selection works"),
        sequence = .("How to read the sequence of lower bounds"), mono = .("How to read monotonicity"),
        iio = .("How to read invariant item ordering"), rest = .("How to read the rest-score method"),
        pmat = .("How to read the P-matrix method"), ca = .("How to read conditional association"),
        ml = .("How to read two-level scalability"),
        boot = .("How to read bootstrap stability")
    ),

    # Teaching blocks shown before each table (option "How to read each
    # table"). Each item answers: what it measures, how it works, what
    # values to expect, and the most common misreading.
    # ES: Bloques didácticos antes de cada tabla. Cada punto responde: qué
    # mide, cómo funciona, qué valores esperar y la lectura errónea más
    # común.
    guide = list(
        scale = c(
            .("What H measures: how well the items order respondents in the same way. Think of a Guttman scale: if items are ordered from easiest to hardest, a respondent who endorses a hard item should also endorse every easier one. Each time this fails is a Guttman error."),
            .("How it works: H = 1 - (observed Guttman errors / Guttman errors expected if the items were unrelated). H = 1 means no errors at all (a perfect Guttman scale); H = 0 means as many errors as pure chance; negative values mean more errors than chance."),
            .("Hi is the same idea for one item against all the others, and H for the whole set. Mokken's criteria: every Hi and H at least .30; .30-.40 weak, .40-.50 moderate, .50 or more strong. These are conventions, not laws: always read them with their confidence intervals."),
            .("Standard errors show how much H would vary from sample to sample. The z test tells you whether a coefficient is reliably above the lower bound, which is stronger evidence than a point estimate that happens to exceed .30."),
            .("Common misreading: H is not a reliability coefficient and not a correlation. A long scale can be very reliable (high alpha) and still have weak scalability, because reliability rewards the number of items while H measures how consistently they order people.")
        ),
        hij = c(
            .("Hij is the scalability of one pair of items: the Guttman errors of that pair relative to those expected by chance. Under the model every Hij is non-negative in the population; in a sample, small negative estimates can arise by chance, so judge their size and uncertainty."),
            .("Read the matrix by rows: an item whose row has many low or negative values is the one that does not fit. A single negative Hij is usually the trace of an item that still needs reversing."),
            .("Common misreading: a few Hij below .30 are not a problem by themselves; the .30 criterion applies to Hi and H. What matters at the pair level is the sign.")
        ),
        reliability = c(
            .("Reliability is the proportion of total-score variance that is not measurement error: how consistently the sum score would rank respondents if the test were repeated."),
            .("MS (Molenaar-Sijtsma) is the reliability estimate native to Mokken scale analysis: it uses the estimated item-step response functions and is less biased than alpha when invariant item ordering holds. Lambda-2 and alpha are lower bounds to reliability; lambda-2 is never smaller than alpha. LCRC estimates reliability from a latent class model and is usually the least biased when its assumptions hold."),
            .("Values of about .80 or more are usually expected for research on groups, and .90 or more for decisions about individuals."),
            .("The item-rest correlation is the correlation of each item with the sum of the other items. A low value marks an item that contributes little to the total score.")
        ),
        errors = c(
            .("A respondent's Guttman errors (G+) count the pairs of item steps they answered in the unexpected order: passing a step that most people fail while failing a step that most people pass. Respondents with many errors answer inconsistently with the item ordering of the rest of the sample."),
            .("O+ counts how many of the respondent's item-score combinations are rare in the sample. Both are person-fit statistics: they assess the respondent, not the item."),
            .("The fences are outlier thresholds. Tukey's fence (U1) assumes a roughly symmetric distribution; error counts are usually skewed, so the medcouple-adjusted fence (U2) is the better guide."),
            .("Common misreading: an outlier is a signal to inspect, not a reason to delete. Unusual patterns can come from careless responding, but also from respondents for whom the attribute works differently, and removing them changes what the scale means.")
        ),
        bounds = c(
            .("Ellis (2014) showed that, if dichotomous items measure one attribute with monotone response functions, the correlation of any two items cannot be arbitrarily small or large: it must lie between bounds computed from their associations with the other items."),
            .("A correlation outside its bounds is evidence against one monotone latent variable for that pair, even when the scalability coefficients look acceptable."),
            .("L2 and U1 are the conservative bounds: only crossing them is a clear violation. L1 and U2 are lenient: crossing only them is a possible violation that deserves a second look.")
        ),
        norms = c(
            .("Norms translate a total score into a position in a reference population: a standard score (z), a stanine (nine bands of the normal distribution) or a percentile rank (percentage of the norm group scoring lower, counting half of those with the same score)."),
            .("Norms are estimates from a sample, so they have standard errors too (Oosterhuis et al., 2017; see references). Wide intervals mean the norm sample is too small for precise decisions at that score."),
            .("Use these norms only if this sample represents the population you want to compare people with; otherwise they describe this sample only.")
        ),
        aisp = c(
            .("Automated item selection answers a question that comes before evaluating a scale: which of these items belong together in one or more Mokken scales? It is an exploratory tool, comparable in purpose to exploratory factor analysis but built on scalability instead of correlations."),
            .("Step 1: the pair of items with the highest significant Hij starts the first scale. Step 2: the item that most increases H is added, if its Hi with the selected items is at least the lower bound c and significant. Step 3: step 2 repeats until no item qualifies. Step 4: the remaining items start a second scale, and so on. Items that never qualify are unscalable (scale 0)."),
            .("Scale numbers are labels in order of formation, not quality: scale 1 is simply the first one built, usually the largest."),
            .("Common misreading: a selected scale is not yet a validated scale. The selection only guarantees scalability; monotonicity, invariant item ordering and the theoretical coherence of the items still have to be checked.")
        ),
        sequence = c(
            .("Each column shows the partition obtained with one lower bound c; each cell is the scale an item joins (0 = unscalable)."),
            .("Pattern of one attribute: at low c all items form one scale, and as c rises items leave that scale one by one and become unscalable, but no second scale appears."),
            .("Pattern of several attributes: as c rises, the single scale splits into two or more scales that keep their items over a range of c values. Stable groupings across neighbouring c values are the ones to interpret."),
            .("Choose the c at which the partition is stable and theoretically interpretable, usually between .30 and .45, and justify it in the report.")
        ),
        mono = c(
            .("Monotonicity means that the probability of a higher item score never decreases as the attribute increases. It is the core assumption of Mokken's models: without it, a higher total score would not mean more of the attribute."),
            .("The attribute is unobserved, so it is replaced by the rest score: the total score on the other items. Respondents are grouped by rest score and, for each item step, the proportion P(X ≥ k) is compared across groups. Every decrease larger than the minimum violation (#vi) is tested; #zsig counts the significant ones."),
            .("Crit combines all the violation statistics into one value (Molenaar & Sijtsma, 2000): below 40 no serious violation, 40 to 80 doubtful, above 80 a serious violation. It is a heuristic, not a test."),
            .("In the plot, curves should rise from left to right. Small dips within the confidence band are sampling noise; a clear, sustained fall is a real violation.")
        ),
        iio = c(
            .("Invariant item ordering (IIO) means that the items are ordered by difficulty in the same way for every respondent, whatever their attribute level. It is needed when you interpret the item order itself, for example in developmental or hierarchical scales, or when you compare people on individual items."),
            .("MIIO checks it pair by pair: within each rest-score group, the easier item must have the higher mean. Items with too many violations are removed one by one (backward selection) until the remaining set has an invariant order."),
            .("HT summarizes how accurately the remaining items are ordered: below .30 the order is too inaccurate to use, .30 to .40 weak, .40 to .50 medium, .50 or more high (Ligtvoet et al., 2010). Report HT only after the backward selection has removed violating items."),
            .("Common misreading: a scale can be perfectly usable without IIO. To order respondents by the total score, what matters is the monotone homogeneity model (one attribute, local independence and monotonicity); IIO is an additional requirement only when the order of the items itself is interpreted.")
        ),
        rest = c(
            .("The rest-score method checks nonintersection of the item step response functions, which the double monotonicity model requires: for any two items, the curves of their item steps must not cross."),
            .("For every pair of items, respondents are grouped by the rest score on the remaining items, and the order of the two items' step proportions is compared across groups. A reversal larger than the minimum violation is counted and tested, and crit summarizes the evidence as for monotonicity."),
            .("In the plot, the two curves of each pair should run side by side without crossing.")
        ),
        pmat = c(
            .("The P-matrix method checks nonintersection with two matrices: P(++) holds the proportion of respondents passing each pair of item steps, and P(--) the proportion failing both."),
            .("Under the double monotonicity model, when the columns are ordered from the most to the least popular item step, every row of P(++) must decrease and every row of P(--) must increase. A reversal is a violation."),
            .("In the plot, each line is one item step: lines of P(++) should only go down and lines of P(--) only go up.")
        ),
        ca = c(
            .("Local independence means that, once the attribute is taken into account, item responses are unrelated. Locally dependent items (for example, items sharing a stimulus or almost identical wording) inflate scalability and reliability."),
            .("Conditional association (CA) gives testable consequences of local independence. For each pair of items, covariances conditional on sum scores of other items are inspected: W1 and W3 flag positive local dependence and W2 negative local dependence, using outlier rules on these indices."),
            .("The procedure removes the item with the worst flags, then repeats the check on the remaining items, until none is flagged. Being stepwise, it does not guarantee the largest possible set without flagged items."),
            .("Removed items are candidates for local dependence, not automatically bad items: inspect their content before deleting them.")
        ),
        ml = c(
            .("In two-level data several raters score the same subject: students rate their teacher, nurses rate a patient, observers rate a classroom. The scale is meant to order the subjects, not the raters."),
            .("HW (within) is the ordinary scalability computed on each rater's own item scores: it tells whether every rater uses the items consistently. HB (between) uses item scores of different raters of the same subject: it tells whether the items order the subjects consistently across raters. For a scale that measures subjects, HB is the coefficient that matters most."),
            .("BW = HB / HW compares the two. Close to 1, raters agree almost as much with each other as with themselves; close to 0, the ordering exists within raters but not between them, and the subjects' scores depend on who rated them."),
            .("Standard errors take the nesting into account (Koopman et al., 2020; see references). Since HB is usually much smaller than HW, test whether HB is larger than zero before interpreting the scale as a measure of the subjects.")
        ),
        boot = c(
            .("A partition found in one sample may not replicate: automated item selection is greedy and capitalizes on chance, so borderline items can move with small changes in the data."),
            .("The bootstrap repeats the selection, with the same options, on many samples drawn with replacement from your respondents (whole clusters, with nested data), the way a new sample of the same population might look."),
            .("With its scale-mates: for each item, the average proportion of samples in which it falls in the same scale as each item it was grouped with. Same scale as found: the proportion of samples in which its set of scale-mates is exactly the one found. Unscalable: how often it was left out."),
            .("There is no agreed cut-off. Values near 1 mean the grouping hardly depends on the sample; items that change scale in a large share of samples should be interpreted, and reported, as uncertain. Bootstrap stability is not replication: confirm the partition in new data.")
        )
    ),

    ml = list(
        intro = c(
            .("Two-level Mokken scale analysis evaluates a scale whose items are scored by several raters per subject, such as students rating their teacher (Snijders, 2001; Crisan et al., 2016; see references)."),
            .("It splits scalability into a within-rater part (HW), a between-rater part (HB) and their ratio (BW). Select the variable that identifies the rated subject, then the items.")
        ),
        too_few_subjects = .("The subject variable has fewer than two subjects. Two-level scalability compares raters of different subjects; select the variable that identifies the rated person or object."),
        failed = .("MokkenLab could not compute the two-level coefficients: %s. Check that each subject has several raters and that the items vary within subjects."),
        key_z_failed = .("Z tests (two-level scalability): the tests could not be computed: %s."),
        single_rater = .("%d subject(s) have a single rater; they contribute nothing to between-rater coefficients."),
        design_note = .("Design: each row is a rater, and the subject variable identifies the person or object those raters scored (students rating their teacher, observers rating a classroom). A grouping such as sex, school grade or country is not a subject: the analysis runs, but HB and BW then have no meaning. To compare groups, use the grouping variable of Mokken Scale Analysis."),
        few_subjects = .("Only %d subjects. Between-rater coefficients describe how items order the subjects, so their precision depends mainly on the number of subjects, not of raters; with this few, read HB and BW as very imprecise and confirm that the subject variable is really the rated person or object."),
        key_few_subjects = .("Two-level design: only %d subjects; between-rater coefficients are very imprecise."),
        z_note = .("z: %2$s test of each coefficient against %1$s; p is one-sided. mokken does not test the ratio BW."),
        read_hw = .("Consistency of each rater's own item ordering"),
        read_hb = .("Consistency of the subjects' ordering across raters"),
        read_bw = .("Rater agreement relative to rater consistency"),
        icc_failed = .("Intraclass correlations could not be computed: %s."),
        icc_note = .("ICC: proportion of the variance that lies between subjects (Snijders & Bosker, 2012). The F test for the total score checks whether subjects differ at all."),
        key_icc = .("Intraclass correlations: ICC of the total score = %s."),
        weights_failed = .("The weights could not be computed: %s."),
        weights_ties = .("Not computable for this pair: two or more of its item steps are equally popular, and mokken 3.1.2 fails when it averages the weights over the tied orderings (an error inside MLweight, not a problem of your data). The other pairs are unaffected."),
        pairs_mismatch = .("mokken returned a number of item pairs that does not match the selected items, so the pairs cannot be labeled safely."),
        weights_note = .("Each cell is the weight of one score pattern of the item pair in two-level Guttman errors (Koopman et al., 2017): 0 for patterns consistent with the item-step ordering, larger for more serious errors. Ties in item-step popularity are averaged over the possible orderings."),
        interp_values = .("Within raters, H = %s; between raters, H = %s; their ratio BW = %s."),
        key_values = .("Two-level scalability: HW = %s, HB = %s, BW = %s."),
        interp_hb_zero = .("HB is zero or negative: the items do not order the subjects consistently across raters. The scale may still describe each rater's own perceptions (HW), but it does not measure differences between subjects. Review rater training, item wording, or whether the attribute is really a property of the subject."),
        key_hb_zero = .("Two-level scalability: HB is not positive; the scale does not order subjects across raters."),
        interp_hb_ci = .("HB is positive but its confidence interval includes zero, so this sample does not show that the items order subjects consistently across raters. More subjects (not only more raters per subject) would make HB more precise."),
        key_hb_ci = .("Two-level scalability: the interval of HB includes zero."),
        interp_hb_pos = .("HB is positive and its confidence interval excludes zero: the items order the subjects consistently across raters, which supports using the scale to compare subjects."),
        interp_bw = .("BW = %s: the closer to 1, the more raters of the same subject agree with each other relative to their own consistency; low values mean the subject's score depends substantially on who rated them, so average several raters per subject."),
        interp_items_hb = .("Items whose between-rater scalability is zero or negative (%d): %s. Raters do not agree on how these items order the subjects; they contribute little to measuring the subjects."),
        key_items_hb = .("Two-level item scalability: %d item(s) with non-positive HBi."),
        interp_next = .("Next step: report HW, HB and BW with their standard errors, the number of subjects and raters per subject, and check monotonicity in Model Assumptions with the subject as cluster variable.")
    ),

    check = list(
        intro = c(
            .("Scalability shows that items order respondents consistently; this analysis checks whether the assumptions behind using their total score hold (Sijtsma & Molenaar, 2002; Sijtsma & van der Ark, 2017; see references)."),
            .("The monotone homogeneity model (one attribute, local independence, monotonicity) justifies ordering respondents by their total score. The double monotonicity model adds nonintersecting item step response functions, which also justifies ordering the items in the same way for everyone (invariant item ordering)."),
            .("Check monotonicity first. Check invariant item ordering, nonintersection and local independence when your use of the scale needs them.")
        ),
        failed = .("The check could not be computed: %s."),
        verdict_none = .("No violations"),
        verdict_na = .("Not evaluable"),
        not_evaluable = .("%s: the check could not be evaluated for %d item(s): %s. This is not evidence that they satisfy the assumption; it usually means too few respondents in the rest-score groups. Lower the minimum group size or collect more data."),
        key_not_evaluable = .("%s: %d item(s) could not be evaluated."),
        verdict_minor = .("No serious violation"),
        verdict_doubtful = .("Doubtful"),
        verdict_serious = .("Serious violation"),
        crit_note = .("Crit (Molenaar & Sijtsma, 2000): below 40 no serious violation, 40-80 doubtful, above 80 serious. #ac: active comparisons; #vi: violations larger than the minimum; #zsig (or #tsig): significant violations."),
        levels_note = .("Within: the item scores of raters within each cluster; Between: the cluster means (two-level fit statistics of Koopman et al.)."),
        clusters_note = .("Each row is a cluster of items with a tied position in the fixed order; the statistics check whether the clusters keep their order for every rest-score group."),
        order_note = .("Fixed item order (codes in the order of the item list): %s."),
        order_bad = .("The order codes must be %d whole numbers, one per item, separated by commas or spaces."),
        ht_note = .("HT is computed on the items left after the backward selection (Ligtvoet et al., 2010). Below .30 inaccurate, .30-.40 weak, .40-.50 medium, .50 or higher high."),
        ht_low = .("Inaccurate ordering (HT < .30)"),
        ht_weak = .("Weak ordering (.30 ≤ HT < .40)"),
        ht_medium = .("Medium ordering (.40 ≤ HT < .50)"),
        ht_high = .("High ordering accuracy (HT ≥ .50)"),
        htb_meaning = .("Accuracy of the ordering between clusters"),
        htratio_meaning = .("Share of the ordering accuracy carried by the clusters"),
        ca_note = .("Each step removes the item with the worst local-dependence flags and checks the rest again (Straat et al., 2016). Kept: items in the final set, where the procedure flags no further local dependence; a stepwise procedure does not guarantee the largest such set."),
        w_note = .("W indices of the first step (all items). W1 and W3: positive local dependence with another item (largest value and number of flagged pairs); W2: negative local dependence. Flags use outlier rules on each index (Straat et al., 2016)."),
        doubtful = .("%s: items with doubtful violations (crit 40-80) (%d): %s. Inspect their curves before deciding; with large samples small, irrelevant violations become significant."),
        mono_ok = .("Monotonicity: no serious violation was detected with these settings. This supports, but does not by itself establish, ordering respondents by the total score: the monotone homogeneity model also requires one attribute and local independence, and with polytomous items the total score orders respondents only in a weaker, average sense (Hemker et al., 1997; see references)."),
        key_mono_ok = .("Monotonicity: no serious violations."),
        mono_serious = .("Monotonicity: items with serious violations (crit > 80) (%d): %s. For these items a higher rest score does not consistently mean a higher item score. Inspect their response functions: a sustained fall suggests an ambiguous or multidimensional item, which should be revised or removed."),
        key_mono_serious = .("Monotonicity: %d item(s) with serious violations (%s)."),
        key_mono_doubtful = .("Monotonicity: %d item(s) with doubtful violations."),
        mono_failed = .("Monotonicity could not be checked: %s."),
        iio_ok = .("Invariant item ordering: no item shows a serious violation, so the items keep the same order of difficulty across the attribute range."),
        key_iio_ok = .("Invariant item ordering: no serious violations."),
        iio_serious = .("Invariant item ordering: items with serious violations (crit > 80) (%d): %s. Their position in the difficulty order changes with the attribute level."),
        key_iio_serious = .("Invariant item ordering: %d item(s) with serious violations (%s)."),
        key_iio_doubtful = .("Invariant item ordering: %d item(s) with doubtful violations."),
        iio_removed = .("The backward selection removed %d item(s), in this order: %s. With them removed, the procedure detects no further violations under these settings; that is not proof that the remaining items have an invariant ordering."),
        key_iio_removed = .("Invariant item ordering: %d item(s) removed by the backward selection."),
        iio_ht = .("HT = %s: %s. HT tells how far apart the item response functions are, and so how reliably the items are ordered."),
        key_ht = .("Accuracy of the item ordering: HT = %s (%s)."),
        iio_failed = .("Invariant item ordering could not be checked: %s."),
        rest_ok = .("Rest-score method: no item shows a serious violation of nonintersection."),
        key_rest_ok = .("Rest-score method: no serious violations."),
        rest_serious = .("Rest-score method: items with serious violations of nonintersection (crit > 80) (%d): %s. Their item step response functions cross those of other items."),
        key_rest_serious = .("Rest-score method: %d item(s) with serious violations (%s)."),
        key_rest_doubtful = .("Rest-score method: %d item(s) with doubtful violations."),
        rest_failed = .("The rest-score method could not be computed: %s."),
        pmat_ok = .("P-matrix method: no item shows a serious violation of nonintersection."),
        key_pmat_ok = .("P-matrix method: no serious violations."),
        pmat_serious = .("P-matrix method: items with serious violations (crit > 80) (%d): %s."),
        key_pmat_serious = .("P-matrix method: %d item(s) with serious violations (%s)."),
        key_pmat_doubtful = .("P-matrix method: %d item(s) with doubtful violations."),
        pmat_failed = .("The P-matrix method could not be computed: %s."),
        ca_ok = .("Conditional association: no item was flagged for local dependence with these settings, which is consistent with local independence (not a proof of it)."),
        key_ca_ok = .("Local independence: no item flagged."),
        ca_removed = .("Conditional association: %d item(s) were removed for local dependence: %s. Check whether they share content, wording or a stimulus with other items; locally dependent items inflate scalability and reliability."),
        key_ca_removed = .("Local independence: %d item(s) removed (%s)."),
        ca_failed = .("Conditional association could not be computed: %s."),
        section_mono = .("Monotonicity"),
        section_iio = .("Invariant item ordering"),
        section_rest = .("Rest-score method"),
        section_pmat = .("P-matrix method"),
        section_ca = .("Conditional association"),
        key_not_run = .("Not checked in this run: %s. Their absence from the warnings says nothing about them."),
        interp_nothing = .("No check was selected."),
        interp_next = .("Next step: revise or remove items with serious violations, re-run Mokken Scale Analysis and these checks on the remaining items, and report crit, the removed items and HT (if invariant item ordering matters for your use).")
    ),

    aisp = list(
        intro = c(
            .("Automated item selection partitions a set of items into Mokken scales: groups of items in which every item has scalability Hi of at least a lower bound c, plus a set of unscalable items (Mokken, 1971; Sijtsma & Molenaar, 2002; see references)."),
            .("Use it when you do not yet know whether the items form one scale, several scales or none, for example when developing a questionnaire or re-examining an existing one.")
        ),
        bad_sequence = .("The sequence of lower bounds is empty: From must be smaller than To. Correct the values in Selection criterion."),
        boot_failed = .("The bootstrap could not be computed: %s."),
        boot_note = .("%d bootstrap samples, seed %d (the same seed and data reproduce the same values). The whole partition was reproduced exactly in a proportion of %s of the samples."),
        boot_failed_some = .("%d of %d bootstrap samples could not be analyzed (for example, an item without variation in the resample) and were left out."),
        key_boot_weak = .("Bootstrap stability: %d item(s) keep their scale-mates in fewer than half of the samples (%s); treat their assignment as uncertain."),
        key_boot_ok = .("Bootstrap stability: every scaled item keeps its scale-mates in at least half of the samples; the whole partition reappears in a proportion of %s."),
        failed = .("MokkenLab could not run the item selection: %s. Check that the items are item scores with variation, and with the genetic algorithm try a larger population or more generations."),
        z_changed = .("The original Z test was replaced by the Wald-based test, because it cannot test Hi against c or account for nested data."),
        start_ga = .("The genetic algorithm does not use a start set; the start set was ignored. Use Mokken's procedure to start the first scale from chosen items."),
        coef_failed = .("The scalability of this scale could not be computed: %s."),
        scales_note = .("Lower and Upper: 95% Wald-based confidence interval of H. Run Mokken Scale Analysis on each scale's items for intervals, tests and item-level detail."),
        membership_note = .("Scale numbers are labels in order of formation, not of quality. Hi is the scalability of the item within its own scale."),
        sequence_note = .("Each cell is the scale an item joins at that lower bound; 0 means unscalable."),
        interp_none = .("No scale could be formed with c = %s: no pair of items has a significant Hij of at least that value. The items do not share enough common ordering to form a Mokken scale. Lower c to explore weaker structures, review whether negatively worded items need reversing, or reconsider whether the items measure a common attribute."),
        key_none = .("Scale membership: no Mokken scale could be formed."),
        interp_one_all = .("Every item joined one single scale with c = %s. This supports treating the items as one scale; check its strength in the table above and its assumptions in Model Assumptions."),
        key_one_all = .("Scale membership: all items form one scale."),
        interp_many = .("The selection formed %d scales with c = %s. The items order respondents consistently within each group but not across groups, which suggests related but distinct attributes, or method effects such as item wording."),
        key_many = .("Scale membership: %d scales formed."),
        interp_scale = .("%s: %d items, H = %s (%s)."),
        interp_unscalable = .("Unscalable items (%d): %s. They did not reach Hi of c with any scale. Review their content: they may measure a different attribute, be poorly worded, or need reversing."),
        key_unscalable = .("Scale membership: %d unscalable item(s) (%s)."),
        interp_caution = .("The selection is exploratory and capitalizes on chance: with another sample the borderline items may move. Confirm the partition in new data, with the genetic algorithm, or with a sequence of lower bounds."),
        interp_next = .("Next step: run Mokken Scale Analysis on the items of each scale to obtain intervals and tests, then Model Assumptions to check monotonicity and, if needed, invariant item ordering."),
        interp_seq_intro = .("The selection was run for %d lower bounds from %s to %s."),
        interp_seq_split = .("From c = %s on, the items split into more than one scale (up to %d). This is the pattern of several related attributes: interpret the groupings that remain stable over neighbouring c values."),
        key_seq_split = .("Partition across lower bounds: the items split into several scales from c = %s (up to %d)."),
        interp_seq_one = .("At no lower bound did a second scale appear: as c rises items only become unscalable. This is the pattern of one attribute measured by items of different quality."),
        key_seq_one = .("Partition across lower bounds: never more than one scale (one-attribute pattern)."),
        interp_seq_30 = .("At c = .30, Mokken's usual criterion, the selection gives %d scale(s) and %d unscalable item(s)."),
        interp_seq_next = .("Next step: choose the lower bound at which the partition is stable and interpretable, run the selection with that single value, and evaluate each scale with Mokken Scale Analysis and Model Assumptions.")
    ),

    msa = list(
        intro = c(
            .("Mokken scale analysis evaluates whether a set of items forms a scale that orders respondents on one attribute, using nonparametric item response theory: it assumes only that the probability of a higher item score increases with the attribute, not a particular response function (Mokken, 1971; Sijtsma & Molenaar, 2002; see references)."),
            .("The scalability coefficient H compares the observed Guttman errors (a respondent passing a harder item step but failing an easier one) with the number expected if items were unrelated. Hij describes an item pair, Hi an item and H the whole scale. Mokken's rule of thumb: a scale needs H and every Hi of at least .30; .30 to .40 is weak, .40 to .50 moderate, .50 or higher strong."),
            .("Read the results in this order: the scale H with its interval and test, the item Hi values, item pairs with negative Hij, and then reliability, outliers and norms.")
        ),
        key_z_failed = .("Z tests (Scalability of the scale): the tests could not be computed: %s."),
        ci_note = .("Confidence intervals: %s%%, %s."),
        hij_note = .("A negative Hij means the two items order respondents in opposite directions: usually an unreversed negatively worded item or an item that measures a different attribute."),
        sets_note = .("Each row is one data set completed by two-way imputation with a different random error. Every other table of this analysis comes from the first set only; this table is a sensitivity check, not pooled multiple-imputation inference: no combination rule is applied, because their validity for Mokken statistics has not been established."),
        group_small = .("groups with fewer than two cases: %s"),
        group_failed = .("The coefficients could not be computed by group: %s. Each group needs enough cases with variation on every item."),
        key_groups = .("Scalability by group: H ranges from %2$s to %3$s across %1$d groups."),
        rel_none = .("No reliability coefficient was selected."),
        rel_failed = .("Reliability could not be computed: %s."),
        rel_value_na = .("This coefficient could not be estimated for these data."),
        lcrc_note = .("Estimated with %d latent classes (%s). LCRC can change with the number of classes and with the starting values of the latent class model; re-run it with another number of classes and report the range if the values differ."),
        lcrc_auto = .("number of items minus one, mokken's default"),
        lcrc_manual = .("fixed by you"),
        errors_none = .("Select G+ or O+ to compute outlier statistics."),
        errors_failed = .("The outlier statistics could not be computed: %s."),
        errors_note = .("G+ counts each respondent's Guttman errors, weighted by item-step popularity; O+ counts infrequent item-score patterns (Zijlstra et al., 2007; see references). U1 is Tukey's upper fence (Q3 + 1.5 IQR) and U2 the medcouple-adjusted fence for skewed distributions (Hubert & Vandervieren, 2008; see references). Respondents above U2 are the clearest outlier candidates."),
        bounds_polytomous = .("Correlation bounds (Ellis, 2014) are defined only for dichotomous items; these items have more than two score categories."),
        bounds_none = .("Select lower bounds, upper bounds or both."),
        bounds_failed = .("The correlation bounds could not be computed: %s."),
        bounds_note = .("In a unidimensional monotone model, each inter-item correlation must lie between bounds derived from the other items (Ellis, 2014; see references). L1 and U1 overestimate the lower and upper bound, L2 and U2 underestimate them, so a correlation below L2 or above U1 is a clear violation and one below L1 or above U2 only a possible one."),
        bounds_invalid_upper = .("For %d item pair(s), mokken returned an upper bound outside the range of a correlation (-1 to 1). Such a bound cannot be crossed or respected in any meaningful way, so it is shown but not used in the verdict, which then rests on the lower bounds."),
        key_bounds = .("Correlation bounds: %d item pair(s) violate the bounds expected under one monotone latent variable."),
        norms_failed = .("The norm statistics could not be computed: %s."),
        norms_note = .("Total score = sum of the %d item scores (negatively worded items reversed). These are norm statistics of this sample, not norms for a population; they become norms only if the sample represents the population you compare people with. Standard errors follow Oosterhuis et al. (2017; see references)."),
        z_note_original = .("z: original Z test of each coefficient against zero (Mokken, 1971); p is one-sided."),
        z_note_bound = .("z: %2$s test of each coefficient against the lower bound %1$s; p is one-sided (coefficient larger than the bound)."),
        interp_h_na = .("The scale H could not be estimated, so the scale cannot be evaluated. Check the item scores and the messages in Notes."),
        interp_h_low = .("The scale H is %s, below Mokken's minimum of .30. The items do not order respondents consistently enough to be summed into one scale score: there are almost as many Guttman errors as would be expected if the items were unrelated. This usually happens when the items measure more than one attribute or include items unrelated to the rest. Run Automated Item Selection to see whether the items split into separate scales."),
        interp_h_ok = .("The scale H is %s: a %s. Taken together, the items order respondents more consistently than unrelated items would. This is necessary but not sufficient for using their sum score: a single H does not show that the items measure one attribute (a set made of two strong subscales can still reach it; check Automated Item Selection) nor that the model assumptions hold (check Model Assumptions)."),
        interp_ci_low = .("The lower limit of the confidence interval of H (%s) is below .30, so this sample does not rule out that the scale is unscalable. Interpret its strength with caution, and prefer a test against .30 (Wald-based or range-preserving) to the point estimate."),
        key_h = .("Scalability of the scale: H = %s, %s."),
        key_ci_low = .("Scalability of the scale: the confidence interval of H reaches below .30."),
        interp_low_items = .("Items with Hi below .30 (%d): %s. In this item set, each of them orders respondents less consistently with the rest than Mokken's criterion requires. Before removing any, check whether it needs reversing and whether the set mixes subscales: an item can fit its own subscale well and still have a low Hi in a mixed set. If you remove items, do it one at a time and re-run the analysis, because every Hi changes when an item is removed."),
        key_low_items = .("Item scalability: %d item(s) with Hi below .30 (%s)."),
        interp_items_ok = .("Every item has Hi of at least .30, so each item contributes to ordering respondents."),
        interp_z_nonsig = .("Items whose Hi is not significantly larger than %s (%d): %s. Their point estimates may meet the criterion, but the sample does not provide enough evidence; a larger sample or the removal of weaker items would clarify it."),
        key_z_nonsig = .("Item scalability: %d item(s) not significantly above %s."),
        interp_neg_pairs = .("Item pairs with a negative estimated Hij (%d): %s. In the population the monotone homogeneity model requires every Hij to be non-negative. A clearly negative estimate usually signals an item that has not been reversed or that measures a different attribute; an estimate close to zero may be sampling error, so judge its size and standard error before drawing conclusions."),
        key_neg_pairs = .("Item-pair scalability: %d pair(s) with negative Hij."),
        interp_sets = .("Across the completed data sets, H ranges from %s to %s. A narrow range means the conclusions do not depend on the imputed scores."),
        interp_rel = .("The Molenaar-Sijtsma reliability of the total score is %s. MS uses the item-step response functions estimated under the double monotonicity model, so it is most trustworthy when invariant item ordering holds (check it in Model Assumptions). High reliability does not show that the total score measures a single attribute: a sum of two correlated subscales can be very reliable."),
        key_rel = .("Reliability: MS = %s."),
        interp_errors = .("%d of %d respondents have more Guttman errors than the skew-adjusted fence U2. Their response patterns are unusually inconsistent with the item ordering: possible careless responding, misunderstanding or a different response process. Inspect them before deciding; do not remove them automatically."),
        key_errors = .("Guttman errors: %d respondent(s) above the skew-adjusted fence."),
        interp_errors_none = .("No respondent has more Guttman errors than the skew-adjusted fence U2, so no response pattern is extreme once the skewness of the error counts is taken into account. %d respondent(s) pass only Tukey's fence U1, which assumes a symmetric distribution and over-flags skewed counts; inspect them only if you have a substantive reason."),
        norms_perc_range = .("At the extremes of the score range, the Wald-based interval of a percentile rank can pass 0 or 100 (Oosterhuis et al., 2017). The values are shown as mokken computes them; read such limits as 0 or 100. The plot draws the band within the possible range."),
        interp_next_ok = .("Next step: check the model assumptions (monotonicity, and invariant item ordering if you need items ordered the same way for every respondent) in Model Assumptions before using the sum score."),
        interp_next_aisp = .("Next step: run Automated Item Selection with the same items to find the subsets that do form Mokken scales, then check the model assumptions for each scale."),
        interp_imputed = .("Missing scores were completed by two-way imputation; report the number of imputed scores and the seed with the results.")
    )
)

# -----------------------------------------------------------------------------
# Lookup helpers.
# ES: Funciones de búsqueda.
#
# .mk_text() resolves (section, key) in the table of the current run, built
# on first use, and shows a visible "[missing text: ...]" marker for an
# unknown key, so a missing string is caught in review instead of rendering
# as an empty paragraph.
#
# ES: .mk_text() resuelve (sección, clave) en la tabla de la ejecución
# actual, armada en el primer uso, y muestra una marca visible
# "[missing text: ...]" para una clave desconocida, para que un texto
# faltante se detecte en revisión en lugar de mostrarse vacío.
# -----------------------------------------------------------------------------
.mk_text <- function(lang, section, key) {
    if (is.null(lang$texts))
        lang$texts <- .mk_text_table(lang$tr)
    value <- lang$texts[[section]][[key]]
    if (is.null(value))
        value <- paste0("[missing text: ", section, ".", key, "]")
    value
}

.mk_clean_text <- function(x) {
    x <- unlist(x, recursive = TRUE, use.names = FALSE)
    x <- as.character(x)
    x <- gsub("[\t\r\n]+", " ", x)
    x <- gsub(" +", " ", x)
    x <- trimws(x)
    x[nzchar(x)]
}

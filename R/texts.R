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
# Every user-facing report string that is assembled at runtime lives here, in
# both report languages (en/es), keyed by (language, section, key). This is
# the `reportLang` mechanism inherited from AssumptionsLab and EGAnetLab
# (ARCHITECTURE.md §12): report text switches instantly when the user changes
# the analysis' Report Language option, independently of jamovi's own
# interface language. Static interface text (titles, labels, descriptions in
# the .yaml files) goes through jamovi's native i18n catalog instead
# (jamovi/i18n/es.po).
#
# ES: Todo texto del informe que se arma en tiempo de ejecución vive aquí, en
# ambos idiomas (en/es), indexado por (idioma, sección, clave). Es el
# mecanismo `reportLang` heredado de AssumptionsLab y EGAnetLab
# (ARCHITECTURE.md §12): el texto del informe cambia al instante cuando el
# usuario cambia la opción Report Language del análisis, sin depender del
# idioma de la interfaz de jamovi. El texto estático de la interfaz pasa por
# el catálogo i18n nativo de jamovi (jamovi/i18n/es.po).
#
# Writing rule: every interpretation answers four questions (what happened,
# why, what it means, what the researcher should do now). Never write a
# bracketed reference number; append "see references" / "ver referencias"
# after an author-year mention (CODE_STYLE.md §21).
#
# ES: Regla de redacción: cada interpretación responde cuatro preguntas (qué
# ocurrió, por qué, qué significa, qué debe hacer ahora el investigador).
# Nunca escribir un número de referencia entre corchetes; agregar "see
# references" / "ver referencias" después de una mención autor-año
# (CODE_STYLE.md §21).
# -----------------------------------------------------------------------------

.mk_texts <- list(

    en = list(

        common = list(
            no_data = "No data are available.",
            too_few_items = "Select at least two items. Scalability coefficients describe how consistently items order respondents, which needs at least one pair of items.",
            too_few_items_3 = "Select at least three items. This check compares each item with the rest of the scale, which needs at least three items.",
            empty_item = "The following items have fewer than two observed scores and cannot be analyzed: %s. Remove them from the analysis.",
            constant_item = "The following items have zero variance in the cases analyzed (every respondent gave the same score): %s. A constant item cannot order respondents, so its scalability is undefined. Remove it from the analysis.",
            non_integer = "The following items have non-integer scores: %s. Mokken scale analysis works with ordered item scores (0, 1, 2, ...). Check that these variables are item scores and not scale means or other derived values.",
            bad_range = "The declared score range (%s to %s) does not contain every observed score (observed range %s to %s), or its lowest value is not below its highest. Correct the declared range, or use the range observed in the data.",
            twoway_failed = "Two-way imputation could not complete the data: %s. Switch Missing values to listwise deletion, or review the missing-data pattern.",
            too_few_cases = "Fewer than two cases remain after removing incomplete cases. Switch Missing values to two-way imputation, or review the missing-data pattern.",
            estimation_failed = "MokkenLab could not compute the scalability coefficients: %s. This usually means an item has scores in only one category for every pair of items, or a variable that is not an item score was selected. Check the selected items and their scores.",
            warnings_title = "Messages from MokkenLab",
            interp_title = "Applied interpretation",
            key_failed = "%s: could not be computed, see the footnote of that table. Results that could not be computed are not evidence in favor of the scale.",
            not_computable = "Not computable",
            h_unscalable = "unscalable (H < .30)",
            h_weak = "weak scale (.30 ≤ H < .40)",
            h_moderate = "moderate scale (.40 ≤ H < .50)",
            h_strong = "strong scale (H ≥ .50)"
        ),

        # Known mokken notices, rewritten in the report language.
        # ES: Avisos conocidos de mokken, reescritos en el idioma del informe.
        notices = list(
            varying_categories = "Not every item uses the same number of score categories in these data. Either some categories were never chosen in this sample, or the items have different numbers of categories by design; in the latter case the total score cannot be used as an ordinal measurement (mokken notice).",
            typez_wb = "With nested data, mokken tests the coefficients with the Wald-based Z test; the original Z test was replaced by it.",
            typez_wb_testhi = "Testing Hi against the lower bound needs the Wald-based Z test, so mokken replaced the original Z test by it."
        ),

        key = list(
            title = "Key findings",
            intro = "A one-line summary of the results below, each line naming the section it comes from. Read the sections for the details and the reasons.",
            h_findings = "Findings",
            h_uncertain = "Points of uncertainty",
            h_warnings = "Warnings",
            none_findings = "No findings to report.",
            none_uncertain = "No points of uncertainty were detected.",
            none_warnings = "No warnings."
        ),

        # Explanation of every decision shared across analyses or specific to
        # one. Each paragraph says what the choice does and when to pick an
        # alternative.
        # ES: Explicación de cada decisión común o propia de un análisis.
        # Cada párrafo dice qué hace la elección y cuándo conviene otra.
        choices = list(
            title = "About your choices",
            intro = "Each decision below is explained for the option currently selected, with guidance on when an alternative would fit your data better.",
            h_na = "Missing values",
            na_listwise = "Listwise deletion keeps only respondents who answered every selected item, because mokken does not accept missing scores. It is appropriate when few cases are incomplete and missingness is unrelated to the trait. With many incomplete cases it wastes information and may bias the results; two-way imputation is then preferable.",
            na_twoway = "Two-way imputation fills each missing score from the respondent's mean, the item's mean and the overall mean, plus a random error (van Ginkel et al., 2007; see references). In simulations it preserved scalability coefficients and item selection well when up to about 10%% of the scores were missing at random. MokkenLab produced %d completed data set(s) with seed %d; the same seed and data always reproduce the same imputation. With several sets, the main results come from the first and a table shows how much H varies between them.",
            h_range = "Score range",
            range_observed = "The lowest and highest scores observed in the data define the score range used to reverse negatively worded items and to bound imputed scores. If an extreme category was never chosen in this sample, declare the range instead, so reversal maps each score to its true mirror.",
            range_manual = "The range you declared defines how negatively worded items are reversed (lowest + highest - score) and bounds imputed scores. Declare the range of the response format, for example 0 to 4 for a five-point item scored from 0.",
            h_neg = "Negatively worded items",
            neg_body = "These items were reversed before the analysis (lowest + highest - score, as mokken::recode()), so that a higher score means more of the attribute on every item. Unreversed negatively worded items produce negative scalability coefficients and break the scale.",
            h_ci = "Confidence intervals",
            ci_WB = "Wald-based intervals are the estimate plus and minus a normal quantile times the standard error, with standard errors from the marginal-modelling approach (Kuijpers et al., 2013; see references). They are symmetric and can exceed 1 for coefficients near the ceiling; then prefer range-preserving intervals.",
            ci_RP = "Range-preserving intervals are built on the log(1 - H) scale and transformed back, so they never exceed 1 and are asymmetric near the ceiling (Koopman et al., 2021; see references). They are preferable for strong scales or small samples, where Wald-based intervals can cross 1.",
            h_z = "Z test",
            z_Z = "The original Z test (Mokken, 1971; see references) tests whether each coefficient is larger than zero. It cannot test against a lower bound such as .30 (the value %s you typed is used only by the other tests); choose the Wald-based or range-preserving test to check whether coefficients exceed a lower bound.",
            z_WB = "The Wald-based test divides the difference between each coefficient and the lower bound %s by its standard error. A significant one-sided result means the coefficient is larger than the bound, which is stronger evidence than a point estimate above it (Koopman et al., 2022; see references).",
            z_RP = "The range-preserving test compares log(1 - H) with log(1 - %s), using a standard error on that scale, so it stays accurate near the ceiling (Koopman et al., 2021; see references). Prefer it to the Wald-based test for strong scales or small samples.",
            h_cluster = "Nested data",
            cluster_body = "Respondents are nested in the clusters of this variable. The coefficients are estimated as usual, but their standard errors, intervals and tests take the dependence between respondents of the same cluster into account (Koopman et al., 2020, 2022; see references). Ignoring nesting makes standard errors too small and tests too liberal.",
            h_search = "Search procedure",
            search_normal = "Mokken's procedure starts the first scale with the pair of items with the highest Hij, then adds, one at a time, the item that most increases H, as long as the item's Hi stays at or above c and its scalability is significant. When no item can be added it starts a second scale with the remaining items, and so on. It is fast and transparent, but greedy: an early choice cannot be undone, so it may miss the best partition.",
            search_ga = "The genetic algorithm searches many candidate partitions at once, recombining and mutating them across generations, and keeps the partition with the largest scales that meet the criteria (Straat et al., 2013; see references). It is less likely than Mokken's procedure to get trapped by an early choice, at the price of computing time and some randomness; the seed makes the result reproducible. Compare both: agreement strengthens the conclusion.",
            h_lb = "Lower bound c",
            lb_single = "Every item admitted to a scale must have Hi of at least c (and the scale H of at least c). The usual value is .30, Mokken's minimum for a scale; higher values give fewer, stronger and shorter scales.",
            lb_sequence = "Running the selection for a sequence of c values, rather than one, shows how the partition changes as the criterion tightens. This is the exploration recommended by Hemker et al. (1995; see references) and by the tutorial of Sijtsma and van der Ark (2017; see references), because one c value can hide whether the items measure one attribute or several.",
            testhi_false = "Items are admitted when their point estimate Hi reaches c and Hi is significantly larger than zero. This is Mokken's original rule.",
            testhi_true = "Items are admitted only when Hi is significantly larger than c, not just when its point estimate reaches c (Koopman et al., 2022; see references). This is stricter and protects against admitting items whose Hi exceeds c only by sampling error, especially in small samples.",
            h_start = "Start set",
            start_body = "The first scale starts from the items you chose instead of from the pair with the highest Hij. Use it to build a scale around items that define the attribute theoretically, and check that the result does not depend on that choice by running the selection without a start set.",
            h_weigh = "Group proportions",
            weigh_true = "The item-score proportions of each subject are weighted by the number of raters, so subjects rated by many raters count more. This is mokken's current default, which corrects a bias of averaged proportions when group sizes differ (Koopman et al., 2022; see references).",
            weigh_false = "The item-score proportions of each subject are averaged, so every subject counts equally whatever its number of raters. This reproduces the original estimator of Snijders (2001) and Koopman et al. (2020); with unequal group sizes the weighted version is preferred.",
            h_minsize = "Minimum rest-score group size",
            minsize_body = "Respondents are grouped by their rest score (total score without the item checked), and adjacent rest scores are merged until each group has at least this many respondents. Larger groups give more stable proportions but fewer points on each curve. mokken's default grows with the sample: N/10 from 500 respondents, N/5 from 250, and max(N/3, 50) below 250.",
            h_minvi = "Minimum violation",
            minvi_body = "A decrease of the response function smaller than this value is ignored as negligible; larger ones are counted and tested. The default .03 follows Molenaar and Sijtsma (2000; see references). Lowering it makes the check more sensitive and flags more, mostly trivial, violations.",
            h_iio_method = "Invariant item ordering method",
            iio_MIIO = "Manifest invariant item ordering (MIIO) compares the mean scores of every pair of items in each rest-score group: if item A is easier than item B for the whole sample, its mean should be higher in every group (Ligtvoet et al., 2010; see references). It is the standard method for polytomous items.",
            iio_MSCPM = "The manifest scale cumulative probability model (MSCPM) compares the item step response functions instead of the item means, so it checks a stronger property: that every item step keeps its order (Ligtvoet et al., 2011; see references). Use it when you need the ordering at the level of response categories.",
            iio_IT = "Increasingness in transposition (IT) checks the ordering through the joint distribution of each pair of items (Ligtvoet et al., 2011; see references). It is the most demanding property and needs large samples.",
            h_iio_order = "Item order",
            order_estimated = "The item order is estimated from the item means in this sample, which is the usual choice when no ordering is hypothesized.",
            order_listed = "The item order is fixed as the order of the item list, as a hypothesis to test (for example, items designed from easy to hard). Items listed first are expected to be the most popular.",
            order_clusters = "Consecutive items form clusters with a tied position in the order, and the analysis checks the ordering between clusters as well as between items, as in the clustered-item analysis of the TROG data (Koopman & Braeken). Use it when items come in blocks that share a stimulus or difficulty level.",
            order_custom = "The order codes you typed fix the position of each item; equal codes form clusters of tied items. Use it to test a hypothesized order that is not the list order.",
            h_ca = "Conditional association",
            ca_body = "Local independence is checked through three special cases of conditional association (Holland & Rosenbaum, 1986; see references): covariances between items must not be negative conditional on sum scores of other items. The worst offending item is removed and the check repeated, until no item is flagged (Straat et al., 2016; see references)."
        ),

        guide_titles = list(
            scale = "How to read scalability", hij = "How to read item pairs", reliability = "How to read reliability",
            errors = "How to read Guttman errors", bounds = "How to read correlation bounds",
            norms = "How to read norm statistics", aisp = "How the automated item selection works",
            sequence = "How to read the sequence of lower bounds", mono = "How to read monotonicity",
            iio = "How to read invariant item ordering", rest = "How to read the rest-score method",
            pmat = "How to read the P-matrix method", ca = "How to read conditional association",
            ml = "How to read two-level scalability",
            boot = "How to read bootstrap stability"
        ),

        # Teaching blocks shown before each table (option "How to read each
        # table"). Each item answers: what it measures, how it works, what
        # values to expect, and the most common misreading.
        # ES: Bloques didácticos antes de cada tabla. Cada punto responde: qué
        # mide, cómo funciona, qué valores esperar y la lectura errónea más
        # común.
        guide = list(
            scale = c(
                "What H measures: how well the items order respondents in the same way. Think of a Guttman scale: if items are ordered from easiest to hardest, a respondent who endorses a hard item should also endorse every easier one. Each time this fails is a Guttman error.",
                "How it works: H = 1 - (observed Guttman errors / Guttman errors expected if the items were unrelated). H = 1 means no errors at all (a perfect Guttman scale); H = 0 means as many errors as pure chance; negative values mean more errors than chance.",
                "Hi is the same idea for one item against all the others, and H for the whole set. Mokken's criteria: every Hi and H at least .30; .30-.40 weak, .40-.50 moderate, .50 or more strong. These are conventions, not laws: always read them with their confidence intervals.",
                "Standard errors show how much H would vary from sample to sample. The z test tells you whether a coefficient is reliably above the lower bound, which is stronger evidence than a point estimate that happens to exceed .30.",
                "Common misreading: H is not a reliability coefficient and not a correlation. A long scale can be very reliable (high alpha) and still have weak scalability, because reliability rewards the number of items while H measures how consistently they order people."
            ),
            hij = c(
                "Hij is the scalability of one pair of items: the Guttman errors of that pair relative to those expected by chance. Under the model every Hij is non-negative in the population; in a sample, small negative estimates can arise by chance, so judge their size and uncertainty.",
                "Read the matrix by rows: an item whose row has many low or negative values is the one that does not fit. A single negative Hij is usually the trace of an item that still needs reversing.",
                "Common misreading: a few Hij below .30 are not a problem by themselves; the .30 criterion applies to Hi and H. What matters at the pair level is the sign."
            ),
            reliability = c(
                "Reliability is the proportion of total-score variance that is not measurement error: how consistently the sum score would rank respondents if the test were repeated.",
                "MS (Molenaar-Sijtsma) is the reliability estimate native to Mokken scale analysis: it uses the estimated item-step response functions and is less biased than alpha when invariant item ordering holds. Lambda-2 and alpha are lower bounds to reliability; lambda-2 is never smaller than alpha. LCRC estimates reliability from a latent class model and is usually the least biased when its assumptions hold.",
                "Values of about .80 or more are usually expected for research on groups, and .90 or more for decisions about individuals.",
                "The item-rest correlation is the correlation of each item with the sum of the other items. A low value marks an item that contributes little to the total score."
            ),
            errors = c(
                "A respondent's Guttman errors (G+) count the pairs of item steps they answered in the unexpected order: passing a step that most people fail while failing a step that most people pass. Respondents with many errors answer inconsistently with the item ordering of the rest of the sample.",
                "O+ counts how many of the respondent's item-score combinations are rare in the sample. Both are person-fit statistics: they assess the respondent, not the item.",
                "The fences are outlier thresholds. Tukey's fence (U1) assumes a roughly symmetric distribution; error counts are usually skewed, so the medcouple-adjusted fence (U2) is the better guide.",
                "Common misreading: an outlier is a signal to inspect, not a reason to delete. Unusual patterns can come from careless responding, but also from respondents for whom the attribute works differently, and removing them changes what the scale means."
            ),
            bounds = c(
                "Ellis (2014) showed that, if dichotomous items measure one attribute with monotone response functions, the correlation of any two items cannot be arbitrarily small or large: it must lie between bounds computed from their associations with the other items.",
                "A correlation outside its bounds is evidence against one monotone latent variable for that pair, even when the scalability coefficients look acceptable.",
                "L2 and U1 are the conservative bounds: only crossing them is a clear violation. L1 and U2 are lenient: crossing only them is a possible violation that deserves a second look."
            ),
            norms = c(
                "Norms translate a total score into a position in a reference population: a standard score (z), a stanine (nine bands of the normal distribution) or a percentile rank (percentage of the norm group scoring lower, counting half of those with the same score).",
                "Norms are estimates from a sample, so they have standard errors too (Oosterhuis et al., 2017; see references). Wide intervals mean the norm sample is too small for precise decisions at that score.",
                "Use these norms only if this sample represents the population you want to compare people with; otherwise they describe this sample only."
            ),
            aisp = c(
                "Automated item selection answers a question that comes before evaluating a scale: which of these items belong together in one or more Mokken scales? It is an exploratory tool, comparable in purpose to exploratory factor analysis but built on scalability instead of correlations.",
                "Step 1: the pair of items with the highest significant Hij starts the first scale. Step 2: the item that most increases H is added, if its Hi with the selected items is at least the lower bound c and significant. Step 3: step 2 repeats until no item qualifies. Step 4: the remaining items start a second scale, and so on. Items that never qualify are unscalable (scale 0).",
                "Scale numbers are labels in order of formation, not quality: scale 1 is simply the first one built, usually the largest.",
                "Common misreading: a selected scale is not yet a validated scale. The selection only guarantees scalability; monotonicity, invariant item ordering and the theoretical coherence of the items still have to be checked."
            ),
            sequence = c(
                "Each column shows the partition obtained with one lower bound c; each cell is the scale an item joins (0 = unscalable).",
                "Pattern of one attribute: at low c all items form one scale, and as c rises items leave that scale one by one and become unscalable, but no second scale appears.",
                "Pattern of several attributes: as c rises, the single scale splits into two or more scales that keep their items over a range of c values. Stable groupings across neighbouring c values are the ones to interpret.",
                "Choose the c at which the partition is stable and theoretically interpretable, usually between .30 and .45, and justify it in the report."
            ),
            mono = c(
                "Monotonicity means that the probability of a higher item score never decreases as the attribute increases. It is the core assumption of Mokken's models: without it, a higher total score would not mean more of the attribute.",
                "The attribute is unobserved, so it is replaced by the rest score: the total score on the other items. Respondents are grouped by rest score and, for each item step, the proportion P(X ≥ k) is compared across groups. Every decrease larger than the minimum violation (#vi) is tested; #zsig counts the significant ones.",
                "Crit combines all the violation statistics into one value (Molenaar & Sijtsma, 2000): below 40 no serious violation, 40 to 80 doubtful, above 80 a serious violation. It is a heuristic, not a test.",
                "In the plot, curves should rise from left to right. Small dips within the confidence band are sampling noise; a clear, sustained fall is a real violation."
            ),
            iio = c(
                "Invariant item ordering (IIO) means that the items are ordered by difficulty in the same way for every respondent, whatever their attribute level. It is needed when you interpret the item order itself, for example in developmental or hierarchical scales, or when you compare people on individual items.",
                "MIIO checks it pair by pair: within each rest-score group, the easier item must have the higher mean. Items with too many violations are removed one by one (backward selection) until the remaining set has an invariant order.",
                "HT summarizes how accurately the remaining items are ordered: below .30 the order is too inaccurate to use, .30 to .40 weak, .40 to .50 medium, .50 or more high (Ligtvoet et al., 2010). Report HT only after the backward selection has removed violating items.",
                "Common misreading: a scale can be perfectly usable without IIO. To order respondents by the total score, what matters is the monotone homogeneity model (one attribute, local independence and monotonicity); IIO is an additional requirement only when the order of the items itself is interpreted."
            ),
            rest = c(
                "The rest-score method checks nonintersection of the item step response functions, which the double monotonicity model requires: for any two items, the curves of their item steps must not cross.",
                "For every pair of items, respondents are grouped by the rest score on the remaining items, and the order of the two items' step proportions is compared across groups. A reversal larger than the minimum violation is counted and tested, and crit summarizes the evidence as for monotonicity.",
                "In the plot, the two curves of each pair should run side by side without crossing."
            ),
            pmat = c(
                "The P-matrix method checks nonintersection with two matrices: P(++) holds the proportion of respondents passing each pair of item steps, and P(--) the proportion failing both.",
                "Under the double monotonicity model, when the columns are ordered from the most to the least popular item step, every row of P(++) must decrease and every row of P(--) must increase. A reversal is a violation.",
                "In the plot, each line is one item step: lines of P(++) should only go down and lines of P(--) only go up."
            ),
            ca = c(
                "Local independence means that, once the attribute is taken into account, item responses are unrelated. Locally dependent items (for example, items sharing a stimulus or almost identical wording) inflate scalability and reliability.",
                "Conditional association (CA) gives testable consequences of local independence. For each pair of items, covariances conditional on sum scores of other items are inspected: W1 and W3 flag positive local dependence and W2 negative local dependence, using outlier rules on these indices.",
                "The procedure removes the item with the worst flags, then repeats the check on the remaining items, until none is flagged. Being stepwise, it does not guarantee the largest possible set without flagged items.",
                "Removed items are candidates for local dependence, not automatically bad items: inspect their content before deleting them."
            ),
            ml = c(
                "In two-level data several raters score the same subject: students rate their teacher, nurses rate a patient, observers rate a classroom. The scale is meant to order the subjects, not the raters.",
                "HW (within) is the ordinary scalability computed on each rater's own item scores: it tells whether every rater uses the items consistently. HB (between) uses item scores of different raters of the same subject: it tells whether the items order the subjects consistently across raters. For a scale that measures subjects, HB is the coefficient that matters most.",
                "BW = HB / HW compares the two. Close to 1, raters agree almost as much with each other as with themselves; close to 0, the ordering exists within raters but not between them, and the subjects' scores depend on who rated them.",
                "Standard errors take the nesting into account (Koopman et al., 2020; see references). Since HB is usually much smaller than HW, test whether HB is larger than zero before interpreting the scale as a measure of the subjects."
            ),
            boot = c(
                "A partition found in one sample may not replicate: automated item selection is greedy and capitalizes on chance, so borderline items can move with small changes in the data.",
                "The bootstrap repeats the selection, with the same options, on many samples drawn with replacement from your respondents (whole clusters, with nested data), the way a new sample of the same population might look.",
                "With its scale-mates: for each item, the average proportion of samples in which it falls in the same scale as each item it was grouped with. Same scale as found: the proportion of samples in which its set of scale-mates is exactly the one found. Unscalable: how often it was left out.",
                "There is no agreed cut-off. Values near 1 mean the grouping hardly depends on the sample; items that change scale in a large share of samples should be interpreted, and reported, as uncertain. Bootstrap stability is not replication: confirm the partition in new data."
            )
        ),

        ml = list(
            intro = c(
                "Two-level Mokken scale analysis evaluates a scale whose items are scored by several raters per subject, such as students rating their teacher (Snijders, 2001; Crisan et al., 2016; see references).",
                "It splits scalability into a within-rater part (HW), a between-rater part (HB) and their ratio (BW). Select the variable that identifies the rated subject, then the items."
            ),
            too_few_subjects = "The subject variable has fewer than two subjects. Two-level scalability compares raters of different subjects; select the variable that identifies the rated person or object.",
            failed = "MokkenLab could not compute the two-level coefficients: %s. Check that each subject has several raters and that the items vary within subjects.",
            key_z_failed = "Z tests (two-level scalability): the tests could not be computed: %s.",
            single_rater = "%d subject(s) have a single rater; they contribute nothing to between-rater coefficients.",
            design_note = "Design: each row is a rater, and the subject variable identifies the person or object those raters scored (students rating their teacher, observers rating a classroom). A grouping such as sex, school grade or country is not a subject: the analysis runs, but HB and BW then have no meaning. To compare groups, use the grouping variable of Mokken Scale Analysis.",
            few_subjects = "Only %d subjects. Between-rater coefficients describe how items order the subjects, so their precision depends mainly on the number of subjects, not of raters; with this few, read HB and BW as very imprecise and confirm that the subject variable is really the rated person or object.",
            key_few_subjects = "Two-level design: only %d subjects; between-rater coefficients are very imprecise.",
            z_note = "z: %2$s test of each coefficient against %1$s; p is one-sided. mokken does not test the ratio BW.",
            read_hw = "Consistency of each rater's own item ordering",
            read_hb = "Consistency of the subjects' ordering across raters",
            read_bw = "Rater agreement relative to rater consistency",
            icc_failed = "Intraclass correlations could not be computed: %s.",
            icc_note = "ICC: proportion of the variance that lies between subjects (Snijders & Bosker, 2012). The F test for the total score checks whether subjects differ at all.",
            key_icc = "Intraclass correlations: ICC of the total score = %s.",
            weights_failed = "The weights could not be computed: %s.",
            weights_ties = "Not computable for this pair: two or more of its item steps are equally popular, and mokken 3.1.2 fails when it averages the weights over the tied orderings (an error inside MLweight, not a problem of your data). The other pairs are unaffected.",
            pairs_mismatch = "mokken returned a number of item pairs that does not match the selected items, so the pairs cannot be labeled safely.",
            weights_note = "Each cell is the weight of one score pattern of the item pair in two-level Guttman errors (Koopman et al., 2017): 0 for patterns consistent with the item-step ordering, larger for more serious errors. Ties in item-step popularity are averaged over the possible orderings.",
            interp_values = "Within raters, H = %s; between raters, H = %s; their ratio BW = %s.",
            key_values = "Two-level scalability: HW = %s, HB = %s, BW = %s.",
            interp_hb_zero = "HB is zero or negative: the items do not order the subjects consistently across raters. The scale may still describe each rater's own perceptions (HW), but it does not measure differences between subjects. Review rater training, item wording, or whether the attribute is really a property of the subject.",
            key_hb_zero = "Two-level scalability: HB is not positive; the scale does not order subjects across raters.",
            interp_hb_ci = "HB is positive but its confidence interval includes zero, so this sample does not show that the items order subjects consistently across raters. More subjects (not only more raters per subject) would make HB more precise.",
            key_hb_ci = "Two-level scalability: the interval of HB includes zero.",
            interp_hb_pos = "HB is positive and its confidence interval excludes zero: the items order the subjects consistently across raters, which supports using the scale to compare subjects.",
            interp_bw = "BW = %s: the closer to 1, the more raters of the same subject agree with each other relative to their own consistency; low values mean the subject's score depends substantially on who rated them, so average several raters per subject.",
            interp_items_hb = "Items whose between-rater scalability is zero or negative (%d): %s. Raters do not agree on how these items order the subjects; they contribute little to measuring the subjects.",
            key_items_hb = "Two-level item scalability: %d item(s) with non-positive HBi.",
            interp_next = "Next step: report HW, HB and BW with their standard errors, the number of subjects and raters per subject, and check monotonicity in Model Assumptions with the subject as cluster variable."
        ),

        check = list(
            intro = c(
                "Scalability shows that items order respondents consistently; this analysis checks whether the assumptions behind using their total score hold (Sijtsma & Molenaar, 2002; Sijtsma & van der Ark, 2017; see references).",
                "The monotone homogeneity model (one attribute, local independence, monotonicity) justifies ordering respondents by their total score. The double monotonicity model adds nonintersecting item step response functions, which also justifies ordering the items in the same way for everyone (invariant item ordering).",
                "Check monotonicity first. Check invariant item ordering, nonintersection and local independence when your use of the scale needs them."
            ),
            failed = "The check could not be computed: %s.",
            verdict_none = "No violations",
            verdict_na = "Not evaluable",
            not_evaluable = "%s: the check could not be evaluated for %d item(s): %s. This is not evidence that they satisfy the assumption; it usually means too few respondents in the rest-score groups. Lower the minimum group size or collect more data.",
            key_not_evaluable = "%s: %d item(s) could not be evaluated.",
            verdict_minor = "No serious violation",
            verdict_doubtful = "Doubtful",
            verdict_serious = "Serious violation",
            crit_note = "Crit (Molenaar & Sijtsma, 2000): below 40 no serious violation, 40-80 doubtful, above 80 serious. #ac: active comparisons; #vi: violations larger than the minimum; #zsig (or #tsig): significant violations.",
            levels_note = "Within: the item scores of raters within each cluster; Between: the cluster means (two-level fit statistics of Koopman et al.).",
            clusters_note = "Each row is a cluster of items with a tied position in the fixed order; the statistics check whether the clusters keep their order for every rest-score group.",
            order_note = "Fixed item order (codes in the order of the item list): %s.",
            order_bad = "The order codes must be %d whole numbers, one per item, separated by commas or spaces.",
            ht_note = "HT is computed on the items left after the backward selection (Ligtvoet et al., 2010). Below .30 inaccurate, .30-.40 weak, .40-.50 medium, .50 or higher high.",
            ht_low = "Inaccurate ordering (HT < .30)",
            ht_weak = "Weak ordering (.30 ≤ HT < .40)",
            ht_medium = "Medium ordering (.40 ≤ HT < .50)",
            ht_high = "High ordering accuracy (HT ≥ .50)",
            htb_meaning = "Accuracy of the ordering between clusters",
            htratio_meaning = "Share of the ordering accuracy carried by the clusters",
            ca_note = "Each step removes the item with the worst local-dependence flags and checks the rest again (Straat et al., 2016). Kept: items in the final set, where the procedure flags no further local dependence; a stepwise procedure does not guarantee the largest such set.",
            w_note = "W indices of the first step (all items). W1 and W3: positive local dependence with another item (largest value and number of flagged pairs); W2: negative local dependence. Flags use outlier rules on each index (Straat et al., 2016).",
            doubtful = "%s: items with doubtful violations (crit 40-80) (%d): %s. Inspect their curves before deciding; with large samples small, irrelevant violations become significant.",
            mono_ok = "Monotonicity: no serious violation was detected with these settings. This supports, but does not by itself establish, ordering respondents by the total score: the monotone homogeneity model also requires one attribute and local independence, and with polytomous items the total score orders respondents only in a weaker, average sense (Hemker et al., 1997; see references).",
            key_mono_ok = "Monotonicity: no serious violations.",
            mono_serious = "Monotonicity: items with serious violations (crit > 80) (%d): %s. For these items a higher rest score does not consistently mean a higher item score. Inspect their response functions: a sustained fall suggests an ambiguous or multidimensional item, which should be revised or removed.",
            key_mono_serious = "Monotonicity: %d item(s) with serious violations (%s).",
            key_mono_doubtful = "Monotonicity: %d item(s) with doubtful violations.",
            mono_failed = "Monotonicity could not be checked: %s.",
            iio_ok = "Invariant item ordering: no item shows a serious violation, so the items keep the same order of difficulty across the attribute range.",
            key_iio_ok = "Invariant item ordering: no serious violations.",
            iio_serious = "Invariant item ordering: items with serious violations (crit > 80) (%d): %s. Their position in the difficulty order changes with the attribute level.",
            key_iio_serious = "Invariant item ordering: %d item(s) with serious violations (%s).",
            key_iio_doubtful = "Invariant item ordering: %d item(s) with doubtful violations.",
            iio_removed = "The backward selection removed %d item(s), in this order: %s. With them removed, the procedure detects no further violations under these settings; that is not proof that the remaining items have an invariant ordering.",
            key_iio_removed = "Invariant item ordering: %d item(s) removed by the backward selection.",
            iio_ht = "HT = %s: %s. HT tells how far apart the item response functions are, and so how reliably the items are ordered.",
            key_ht = "Accuracy of the item ordering: HT = %s (%s).",
            iio_failed = "Invariant item ordering could not be checked: %s.",
            rest_ok = "Rest-score method: no item shows a serious violation of nonintersection.",
            key_rest_ok = "Rest-score method: no serious violations.",
            rest_serious = "Rest-score method: items with serious violations of nonintersection (crit > 80) (%d): %s. Their item step response functions cross those of other items.",
            key_rest_serious = "Rest-score method: %d item(s) with serious violations (%s).",
            key_rest_doubtful = "Rest-score method: %d item(s) with doubtful violations.",
            rest_failed = "The rest-score method could not be computed: %s.",
            pmat_ok = "P-matrix method: no item shows a serious violation of nonintersection.",
            key_pmat_ok = "P-matrix method: no serious violations.",
            pmat_serious = "P-matrix method: items with serious violations (crit > 80) (%d): %s.",
            key_pmat_serious = "P-matrix method: %d item(s) with serious violations (%s).",
            key_pmat_doubtful = "P-matrix method: %d item(s) with doubtful violations.",
            pmat_failed = "The P-matrix method could not be computed: %s.",
            ca_ok = "Conditional association: no item was flagged for local dependence with these settings, which is consistent with local independence (not a proof of it).",
            key_ca_ok = "Local independence: no item flagged.",
            ca_removed = "Conditional association: %d item(s) were removed for local dependence: %s. Check whether they share content, wording or a stimulus with other items; locally dependent items inflate scalability and reliability.",
            key_ca_removed = "Local independence: %d item(s) removed (%s).",
            ca_failed = "Conditional association could not be computed: %s.",
            section_mono = "Monotonicity",
            section_iio = "Invariant item ordering",
            section_rest = "Rest-score method",
            section_pmat = "P-matrix method",
            section_ca = "Conditional association",
            key_not_run = "Not checked in this run: %s. Their absence from the warnings says nothing about them.",
            interp_nothing = "No check was selected.",
            interp_next = "Next step: revise or remove items with serious violations, re-run Mokken Scale Analysis and these checks on the remaining items, and report crit, the removed items and HT (if invariant item ordering matters for your use)."
        ),

        aisp = list(
            intro = c(
                "Automated item selection partitions a set of items into Mokken scales: groups of items in which every item has scalability Hi of at least a lower bound c, plus a set of unscalable items (Mokken, 1971; Sijtsma & Molenaar, 2002; see references).",
                "Use it when you do not yet know whether the items form one scale, several scales or none, for example when developing a questionnaire or re-examining an existing one."
            ),
            bad_sequence = "The sequence of lower bounds is empty: From must be smaller than To. Correct the values in Selection criterion.",
            boot_failed = "The bootstrap could not be computed: %s.",
            boot_note = "%d bootstrap samples, seed %d (the same seed and data reproduce the same values). The whole partition was reproduced exactly in a proportion of %s of the samples.",
            boot_failed_some = "%d of %d bootstrap samples could not be analyzed (for example, an item without variation in the resample) and were left out.",
            key_boot_weak = "Bootstrap stability: %d item(s) keep their scale-mates in fewer than half of the samples (%s); treat their assignment as uncertain.",
            key_boot_ok = "Bootstrap stability: every scaled item keeps its scale-mates in at least half of the samples; the whole partition reappears in a proportion of %s.",
            failed = "MokkenLab could not run the item selection: %s. Check that the items are item scores with variation, and with the genetic algorithm try a larger population or more generations.",
            z_changed = "The original Z test was replaced by the Wald-based test, because it cannot test Hi against c or account for nested data.",
            start_ga = "The genetic algorithm does not use a start set; the start set was ignored. Use Mokken's procedure to start the first scale from chosen items.",
            coef_failed = "The scalability of this scale could not be computed: %s.",
            scales_note = "Lower and Upper: 95% Wald-based confidence interval of H. Run Mokken Scale Analysis on each scale's items for intervals, tests and item-level detail.",
            membership_note = "Scale numbers are labels in order of formation, not of quality. Hi is the scalability of the item within its own scale.",
            sequence_note = "Each cell is the scale an item joins at that lower bound; 0 means unscalable.",
            interp_none = "No scale could be formed with c = %s: no pair of items has a significant Hij of at least that value. The items do not share enough common ordering to form a Mokken scale. Lower c to explore weaker structures, review whether negatively worded items need reversing, or reconsider whether the items measure a common attribute.",
            key_none = "Scale membership: no Mokken scale could be formed.",
            interp_one_all = "Every item joined one single scale with c = %s. This supports treating the items as one scale; check its strength in the table above and its assumptions in Model Assumptions.",
            key_one_all = "Scale membership: all items form one scale.",
            interp_many = "The selection formed %d scales with c = %s. The items order respondents consistently within each group but not across groups, which suggests related but distinct attributes, or method effects such as item wording.",
            key_many = "Scale membership: %d scales formed.",
            interp_scale = "%s: %d items, H = %s (%s).",
            interp_unscalable = "Unscalable items (%d): %s. They did not reach Hi of c with any scale. Review their content: they may measure a different attribute, be poorly worded, or need reversing.",
            key_unscalable = "Scale membership: %d unscalable item(s) (%s).",
            interp_caution = "The selection is exploratory and capitalizes on chance: with another sample the borderline items may move. Confirm the partition in new data, with the genetic algorithm, or with a sequence of lower bounds.",
            interp_next = "Next step: run Mokken Scale Analysis on the items of each scale to obtain intervals and tests, then Model Assumptions to check monotonicity and, if needed, invariant item ordering.",
            interp_seq_intro = "The selection was run for %d lower bounds from %s to %s.",
            interp_seq_split = "From c = %s on, the items split into more than one scale (up to %d). This is the pattern of several related attributes: interpret the groupings that remain stable over neighbouring c values.",
            key_seq_split = "Partition across lower bounds: the items split into several scales from c = %s (up to %d).",
            interp_seq_one = "At no lower bound did a second scale appear: as c rises items only become unscalable. This is the pattern of one attribute measured by items of different quality.",
            key_seq_one = "Partition across lower bounds: never more than one scale (one-attribute pattern).",
            interp_seq_30 = "At c = .30, Mokken's usual criterion, the selection gives %d scale(s) and %d unscalable item(s).",
            interp_seq_next = "Next step: choose the lower bound at which the partition is stable and interpretable, run the selection with that single value, and evaluate each scale with Mokken Scale Analysis and Model Assumptions."
        ),

        msa = list(
            intro = c(
                "Mokken scale analysis evaluates whether a set of items forms a scale that orders respondents on one attribute, using nonparametric item response theory: it assumes only that the probability of a higher item score increases with the attribute, not a particular response function (Mokken, 1971; Sijtsma & Molenaar, 2002; see references).",
                "The scalability coefficient H compares the observed Guttman errors (a respondent passing a harder item step but failing an easier one) with the number expected if items were unrelated. Hij describes an item pair, Hi an item and H the whole scale. Mokken's rule of thumb: a scale needs H and every Hi of at least .30; .30 to .40 is weak, .40 to .50 moderate, .50 or higher strong.",
                "Read the results in this order: the scale H with its interval and test, the item Hi values, item pairs with negative Hij, and then reliability, outliers and norms."
            ),
            key_z_failed = "Z tests (Scalability of the scale): the tests could not be computed: %s.",
            ci_note = "Confidence intervals: %s%%, %s.",
            hij_note = "A negative Hij means the two items order respondents in opposite directions: usually an unreversed negatively worded item or an item that measures a different attribute.",
            sets_note = "Each row is one data set completed by two-way imputation with a different random error. Every other table of this analysis comes from the first set only; this table is a sensitivity check, not pooled multiple-imputation inference: no combination rule is applied, because their validity for Mokken statistics has not been established.",
            group_small = "groups with fewer than two cases: %s",
            group_failed = "The coefficients could not be computed by group: %s. Each group needs enough cases with variation on every item.",
            key_groups = "Scalability by group: H ranges from %2$s to %3$s across %1$d groups.",
            rel_none = "No reliability coefficient was selected.",
            rel_failed = "Reliability could not be computed: %s.",
            rel_value_na = "This coefficient could not be estimated for these data.",
            lcrc_note = "Estimated with %d latent classes (%s). LCRC can change with the number of classes and with the starting values of the latent class model; re-run it with another number of classes and report the range if the values differ.",
            lcrc_auto = "number of items minus one, mokken's default",
            lcrc_manual = "fixed by you",
            errors_none = "Select G+ or O+ to compute outlier statistics.",
            errors_failed = "The outlier statistics could not be computed: %s.",
            errors_note = "G+ counts each respondent's Guttman errors, weighted by item-step popularity; O+ counts infrequent item-score patterns (Zijlstra et al., 2007; see references). U1 is Tukey's upper fence (Q3 + 1.5 IQR) and U2 the medcouple-adjusted fence for skewed distributions (Hubert & Vandervieren, 2008; see references). Respondents above U2 are the clearest outlier candidates.",
            bounds_polytomous = "Correlation bounds (Ellis, 2014) are defined only for dichotomous items; these items have more than two score categories.",
            bounds_none = "Select lower bounds, upper bounds or both.",
            bounds_failed = "The correlation bounds could not be computed: %s.",
            bounds_note = "In a unidimensional monotone model, each inter-item correlation must lie between bounds derived from the other items (Ellis, 2014; see references). L1 and U1 overestimate the lower and upper bound, L2 and U2 underestimate them, so a correlation below L2 or above U1 is a clear violation and one below L1 or above U2 only a possible one.",
            bounds_invalid_upper = "For %d item pair(s), mokken returned an upper bound outside the range of a correlation (-1 to 1). Such a bound cannot be crossed or respected in any meaningful way, so it is shown but not used in the verdict, which then rests on the lower bounds.",
            key_bounds = "Correlation bounds: %d item pair(s) violate the bounds expected under one monotone latent variable.",
            norms_failed = "The norm statistics could not be computed: %s.",
            norms_note = "Total score = sum of the %d item scores (negatively worded items reversed). These are norm statistics of this sample, not norms for a population; they become norms only if the sample represents the population you compare people with. Standard errors follow Oosterhuis et al. (2017; see references).",
            z_note_original = "z: original Z test of each coefficient against zero (Mokken, 1971); p is one-sided.",
            z_note_bound = "z: %2$s test of each coefficient against the lower bound %1$s; p is one-sided (coefficient larger than the bound).",
            interp_h_na = "The scale H could not be estimated, so the scale cannot be evaluated. Check the item scores and the messages in Notes.",
            interp_h_low = "The scale H is %s, below Mokken's minimum of .30. The items do not order respondents consistently enough to be summed into one scale score: there are almost as many Guttman errors as would be expected if the items were unrelated. This usually happens when the items measure more than one attribute or include items unrelated to the rest. Run Automated Item Selection to see whether the items split into separate scales.",
            interp_h_ok = "The scale H is %s: a %s. Taken together, the items order respondents more consistently than unrelated items would. This is necessary but not sufficient for using their sum score: a single H does not show that the items measure one attribute (a set made of two strong subscales can still reach it; check Automated Item Selection) nor that the model assumptions hold (check Model Assumptions).",
            interp_ci_low = "The lower limit of the confidence interval of H (%s) is below .30, so this sample does not rule out that the scale is unscalable. Interpret its strength with caution, and prefer a test against .30 (Wald-based or range-preserving) to the point estimate.",
            key_h = "Scalability of the scale: H = %s, %s.",
            key_ci_low = "Scalability of the scale: the confidence interval of H reaches below .30.",
            interp_low_items = "Items with Hi below .30 (%d): %s. In this item set, each of them orders respondents less consistently with the rest than Mokken's criterion requires. Before removing any, check whether it needs reversing and whether the set mixes subscales: an item can fit its own subscale well and still have a low Hi in a mixed set. If you remove items, do it one at a time and re-run the analysis, because every Hi changes when an item is removed.",
            key_low_items = "Item scalability: %d item(s) with Hi below .30 (%s).",
            interp_items_ok = "Every item has Hi of at least .30, so each item contributes to ordering respondents.",
            interp_z_nonsig = "Items whose Hi is not significantly larger than %s (%d): %s. Their point estimates may meet the criterion, but the sample does not provide enough evidence; a larger sample or the removal of weaker items would clarify it.",
            key_z_nonsig = "Item scalability: %d item(s) not significantly above %s.",
            interp_neg_pairs = "Item pairs with a negative estimated Hij (%d): %s. In the population the monotone homogeneity model requires every Hij to be non-negative. A clearly negative estimate usually signals an item that has not been reversed or that measures a different attribute; an estimate close to zero may be sampling error, so judge its size and standard error before drawing conclusions.",
            key_neg_pairs = "Item-pair scalability: %d pair(s) with negative Hij.",
            interp_sets = "Across the completed data sets, H ranges from %s to %s. A narrow range means the conclusions do not depend on the imputed scores.",
            interp_rel = "The Molenaar-Sijtsma reliability of the total score is %s. MS uses the item-step response functions estimated under the double monotonicity model, so it is most trustworthy when invariant item ordering holds (check it in Model Assumptions). High reliability does not show that the total score measures a single attribute: a sum of two correlated subscales can be very reliable.",
            key_rel = "Reliability: MS = %s.",
            interp_errors = "%d of %d respondents have more Guttman errors than the skew-adjusted fence U2. Their response patterns are unusually inconsistent with the item ordering: possible careless responding, misunderstanding or a different response process. Inspect them before deciding; do not remove them automatically.",
            key_errors = "Guttman errors: %d respondent(s) above the skew-adjusted fence.",
            interp_errors_none = "No respondent has more Guttman errors than the skew-adjusted fence U2, so no response pattern is extreme once the skewness of the error counts is taken into account. %d respondent(s) pass only Tukey's fence U1, which assumes a symmetric distribution and over-flags skewed counts; inspect them only if you have a substantive reason.",
            norms_perc_range = "At the extremes of the score range, the Wald-based interval of a percentile rank can pass 0 or 100 (Oosterhuis et al., 2017). The values are shown as mokken computes them; read such limits as 0 or 100. The plot draws the band within the possible range.",
            interp_next_ok = "Next step: check the model assumptions (monotonicity, and invariant item ordering if you need items ordered the same way for every respondent) in Model Assumptions before using the sum score.",
            interp_next_aisp = "Next step: run Automated Item Selection with the same items to find the subsets that do form Mokken scales, then check the model assumptions for each scale.",
            interp_imputed = "Missing scores were completed by two-way imputation; report the number of imputed scores and the seed with the results."
        )
    ),

    es = list(

        common = list(
            no_data = "No hay datos disponibles.",
            too_few_items = "Seleccione al menos dos ítems. Los coeficientes de escalabilidad describen con qué consistencia los ítems ordenan a las personas, lo que requiere al menos un par de ítems.",
            too_few_items_3 = "Seleccione al menos tres ítems. Esta verificación compara cada ítem con el resto de la escala, lo que requiere al menos tres ítems.",
            empty_item = "Los siguientes ítems tienen menos de dos puntajes observados y no pueden analizarse: %s. Quítelos del análisis.",
            constant_item = "Los siguientes ítems tienen varianza cero en los casos analizados (todas las personas dieron el mismo puntaje): %s. Un ítem constante no puede ordenar a las personas, así que su escalabilidad no está definida. Quítelo del análisis.",
            non_integer = "Los siguientes ítems tienen puntajes no enteros: %s. El análisis de escalamiento de Mokken trabaja con puntajes de ítem ordenados (0, 1, 2, ...). Verifique que estas variables sean puntajes de ítem y no medias de escala u otros valores derivados.",
            bad_range = "El rango de puntajes declarado (%s a %s) no contiene todos los puntajes observados (rango observado %s a %s), o su valor más bajo no es menor que el más alto. Corrija el rango declarado o use el rango observado en los datos.",
            twoway_failed = "La imputación two-way no pudo completar los datos: %s. Cambie Valores perdidos a eliminación por lista o revise el patrón de datos perdidos.",
            too_few_cases = "Quedan menos de dos casos después de eliminar los casos incompletos. Cambie Valores perdidos a imputación two-way o revise el patrón de datos perdidos.",
            estimation_failed = "MokkenLab no pudo calcular los coeficientes de escalabilidad: %s. Esto suele significar que un ítem tiene puntajes en una sola categoría para cada par de ítems, o que se seleccionó una variable que no es un puntaje de ítem. Revise los ítems seleccionados y sus puntajes.",
            warnings_title = "Mensajes de MokkenLab",
            interp_title = "Interpretación aplicada",
            key_failed = "%s: no pudo calcularse, vea la nota al pie de esa tabla. Los resultados que no pudieron calcularse no son evidencia a favor de la escala.",
            not_computable = "No computable",
            h_unscalable = "no escalable (H < .30)",
            h_weak = "escala débil (.30 ≤ H < .40)",
            h_moderate = "escala moderada (.40 ≤ H < .50)",
            h_strong = "escala fuerte (H ≥ .50)"
        ),

        notices = list(
            varying_categories = "No todos los ítems usan el mismo número de categorías de puntaje en estos datos. O algunas categorías nunca se eligieron en esta muestra, o los ítems tienen distinto número de categorías por diseño; en ese caso la puntuación total no puede usarse como medida ordinal (aviso de mokken).",
            typez_wb = "Con datos anidados, mokken contrasta los coeficientes con la prueba Z de Wald; esta reemplazó a la prueba Z original.",
            typez_wb_testhi = "Contrastar Hi contra la cota inferior requiere la prueba Z de Wald, así que mokken la usó en lugar de la Z original."
        ),

        key = list(
            title = "Hallazgos clave",
            intro = "Un resumen de una línea de los resultados de abajo; cada línea nombra la sección de la que proviene. Lea las secciones para ver los detalles y las razones.",
            h_findings = "Hallazgos",
            h_uncertain = "Puntos de incertidumbre",
            h_warnings = "Advertencias",
            none_findings = "No hay hallazgos que informar.",
            none_uncertain = "No se detectaron puntos de incertidumbre.",
            none_warnings = "No hay advertencias."
        ),

        choices = list(
            title = "Acerca de sus elecciones",
            intro = "Cada decisión de abajo se explica para la opción seleccionada, con orientación sobre cuándo otra alternativa se ajustaría mejor a sus datos.",
            h_na = "Valores perdidos",
            na_listwise = "La eliminación por lista conserva solo a las personas que respondieron todos los ítems seleccionados, porque mokken no acepta puntajes perdidos. Es adecuada cuando pocos casos están incompletos y la pérdida no se relaciona con el atributo. Con muchos casos incompletos desperdicia información y puede sesgar los resultados; entonces es preferible la imputación two-way.",
            na_twoway = "La imputación two-way completa cada puntaje perdido a partir de la media de la persona, la media del ítem y la media general, más un error aleatorio (van Ginkel et al., 2007; ver referencias). En simulaciones preservó bien los coeficientes de escalabilidad y la selección de ítems con hasta cerca de 10%% de puntajes perdidos al azar. MokkenLab generó %d conjunto(s) completado(s) con la semilla %d; la misma semilla y los mismos datos siempre reproducen la misma imputación. Con varios conjuntos, los resultados principales provienen del primero y una tabla muestra cuánto varía H entre ellos.",
            h_range = "Rango de puntajes",
            range_observed = "Los puntajes mínimo y máximo observados en los datos definen el rango usado para invertir los ítems redactados en negativo y para acotar los puntajes imputados. Si una categoría extrema nunca se eligió en esta muestra, declare el rango, para que la inversión lleve cada puntaje a su espejo verdadero.",
            range_manual = "El rango que usted declaró define cómo se invierten los ítems redactados en negativo (mínimo + máximo - puntaje) y acota los puntajes imputados. Declare el rango del formato de respuesta, por ejemplo 0 a 4 para un ítem de cinco puntos puntuado desde 0.",
            h_neg = "Ítems redactados en negativo",
            neg_body = "Estos ítems se invirtieron antes del análisis (mínimo + máximo - puntaje, como mokken::recode()), para que un puntaje más alto signifique más del atributo en todos los ítems. Los ítems negativos sin invertir producen coeficientes de escalabilidad negativos y rompen la escala.",
            h_ci = "Intervalos de confianza",
            ci_WB = "Los intervalos de Wald son la estimación más y menos un cuantil normal por el error estándar, con errores estándar del enfoque de modelos marginales (Kuijpers et al., 2013; ver referencias). Son simétricos y pueden superar 1 para coeficientes cerca del techo; en ese caso prefiera los intervalos preservadores de rango.",
            ci_RP = "Los intervalos preservadores de rango se construyen en la escala log(1 - H) y se transforman de vuelta, así que nunca superan 1 y son asimétricos cerca del techo (Koopman et al., 2021; ver referencias). Son preferibles para escalas fuertes o muestras pequeñas, donde los intervalos de Wald pueden cruzar 1.",
            h_z = "Prueba Z",
            z_Z = "La prueba Z original (Mokken, 1971; ver referencias) contrasta si cada coeficiente es mayor que cero. No puede contrastar contra una cota inferior como .30 (el valor %s que escribió solo lo usan las otras pruebas); elija la prueba de Wald o la preservadora de rango para verificar si los coeficientes superan una cota inferior.",
            z_WB = "La prueba de Wald divide la diferencia entre cada coeficiente y la cota inferior %s por su error estándar. Un resultado unilateral significativo indica que el coeficiente es mayor que la cota, lo que es evidencia más fuerte que una estimación puntual por encima de ella (Koopman et al., 2022; ver referencias).",
            z_RP = "La prueba preservadora de rango compara log(1 - H) con log(1 - %s), con un error estándar en esa escala, así que sigue siendo precisa cerca del techo (Koopman et al., 2021; ver referencias). Prefiérala a la prueba de Wald para escalas fuertes o muestras pequeñas.",
            h_cluster = "Datos anidados",
            cluster_body = "Las personas están anidadas en los conglomerados de esta variable. Los coeficientes se estiman como siempre, pero sus errores estándar, intervalos y pruebas consideran la dependencia entre personas del mismo conglomerado (Koopman et al., 2020, 2022; ver referencias). Ignorar el anidamiento da errores estándar demasiado pequeños y pruebas demasiado liberales.",
            h_search = "Procedimiento de búsqueda",
            search_normal = "El procedimiento de Mokken inicia la primera escala con el par de ítems de mayor Hij y luego agrega, de a uno, el ítem que más aumenta H, siempre que su Hi se mantenga en c o más y su escalabilidad sea significativa. Cuando ningún ítem puede agregarse inicia una segunda escala con los ítems restantes, y así sucesivamente. Es rápido y transparente, pero voraz: una elección temprana no puede deshacerse, así que puede pasar por alto la mejor partición.",
            search_ga = "El algoritmo genético explora muchas particiones candidatas a la vez, recombinándolas y mutándolas a lo largo de generaciones, y conserva la partición con las escalas más grandes que cumplen los criterios (Straat et al., 2013; ver referencias). Es menos propenso que el procedimiento de Mokken a quedar atrapado por una elección temprana, a cambio de tiempo de cómputo y algo de azar; la semilla hace que el resultado sea reproducible. Compare ambos: el acuerdo fortalece la conclusión.",
            h_lb = "Cota inferior c",
            lb_single = "Cada ítem admitido en una escala debe tener Hi de al menos c (y la escala, H de al menos c). El valor habitual es .30, el mínimo de Mokken para una escala; valores más altos dan escalas menos numerosas, más fuertes y más cortas.",
            lb_sequence = "Ejecutar la selección para una secuencia de valores de c, en lugar de uno, muestra cómo cambia la partición al endurecer el criterio. Es la exploración recomendada por Hemker et al. (1995; ver referencias) y por el tutorial de Sijtsma y van der Ark (2017; ver referencias), porque un solo valor de c puede ocultar si los ítems miden uno o varios atributos.",
            testhi_false = "Los ítems se admiten cuando su estimación puntual Hi alcanza c y Hi es significativamente mayor que cero. Es la regla original de Mokken.",
            testhi_true = "Los ítems se admiten solo cuando Hi es significativamente mayor que c, no solo cuando su estimación puntual alcanza c (Koopman et al., 2022; ver referencias). Es más estricto y protege contra admitir ítems cuyo Hi supera c solo por error muestral, sobre todo en muestras pequeñas.",
            h_start = "Conjunto inicial",
            start_body = "La primera escala parte de los ítems que usted eligió en lugar del par de mayor Hij. Úselo para construir una escala alrededor de ítems que definen teóricamente el atributo, y verifique que el resultado no dependa de esa elección ejecutando la selección sin conjunto inicial.",
            h_weigh = "Proporciones de grupo",
            weigh_true = "Las proporciones de puntajes de cada sujeto se ponderan por su número de evaluadores, así que los sujetos con muchos evaluadores pesan más. Es el valor por defecto actual de mokken, que corrige un sesgo de las proporciones promediadas cuando los tamaños de grupo difieren (Koopman et al., 2022; ver referencias).",
            weigh_false = "Las proporciones de puntajes de cada sujeto se promedian, así que cada sujeto pesa igual cualquiera sea su número de evaluadores. Reproduce el estimador original de Snijders (2001) y de Koopman et al. (2020); con tamaños de grupo desiguales se prefiere la versión ponderada.",
            h_minsize = "Tamaño mínimo de grupo de rest-score",
            minsize_body = "Las personas se agrupan por su rest score (puntuación total sin el ítem que se verifica), y los rest scores vecinos se unen hasta que cada grupo tenga al menos este número de personas. Grupos más grandes dan proporciones más estables pero menos puntos en cada curva. El valor por defecto de mokken crece con la muestra: N/10 desde 500 personas, N/5 desde 250 y max(N/3, 50) por debajo de 250.",
            h_minvi = "Violación mínima",
            minvi_body = "Una disminución de la función de respuesta menor que este valor se ignora como despreciable; las mayores se cuentan y se contrastan. El valor .03 sigue a Molenaar y Sijtsma (2000; ver referencias). Bajarlo hace la verificación más sensible y marca más violaciones, en su mayoría triviales.",
            h_iio_method = "Método de orden invariante",
            iio_MIIO = "El orden invariante manifiesto (MIIO) compara las medias de cada par de ítems en cada grupo de rest-score: si el ítem A es más fácil que el B en toda la muestra, su media debería ser mayor en cada grupo (Ligtvoet et al., 2010; ver referencias). Es el método estándar para ítems politómicos.",
            iio_MSCPM = "El modelo manifiesto de probabilidad acumulada de escala (MSCPM) compara las funciones de respuesta de los pasos de ítem en lugar de las medias, así que verifica una propiedad más fuerte: que cada paso de ítem conserve su orden (Ligtvoet et al., 2011; ver referencias). Úselo cuando necesite el orden a nivel de las categorías de respuesta.",
            iio_IT = "La creciente por transposición (IT) verifica el orden mediante la distribución conjunta de cada par de ítems (Ligtvoet et al., 2011; ver referencias). Es la propiedad más exigente y requiere muestras grandes.",
            h_iio_order = "Orden de los ítems",
            order_estimated = "El orden de los ítems se estima a partir de sus medias en esta muestra, la elección habitual cuando no se plantea un orden hipotético.",
            order_listed = "El orden de los ítems se fija como el orden de la lista, como hipótesis a contrastar (por ejemplo, ítems diseñados de fácil a difícil). Se espera que los primeros de la lista sean los más populares.",
            order_clusters = "Los ítems consecutivos forman conglomerados con una posición empatada en el orden, y el análisis verifica el orden entre conglomerados además de entre ítems, como en el análisis de ítems agrupados de los datos TROG (Koopman y Braeken). Úselo cuando los ítems vienen en bloques que comparten un estímulo o nivel de dificultad.",
            order_custom = "Los códigos de orden que escribió fijan la posición de cada ítem; códigos iguales forman conglomerados de ítems empatados. Úselo para contrastar un orden hipotético distinto del de la lista.",
            h_ca = "Asociación condicional",
            ca_body = "La independencia local se verifica con tres casos especiales de asociación condicional (Holland y Rosenbaum, 1986; ver referencias): las covarianzas entre ítems no deben ser negativas condicionadas a puntuaciones de otros ítems. El ítem con peor infracción se elimina y la verificación se repite, hasta que ningún ítem quede marcado (Straat et al., 2016; ver referencias)."
        ),

        guide_titles = list(
            scale = "Cómo leer la escalabilidad", hij = "Cómo leer los pares de ítems", reliability = "Cómo leer la fiabilidad",
            errors = "Cómo leer los errores de Guttman", bounds = "Cómo leer las cotas de correlación",
            norms = "Cómo leer los estadísticos normativos", aisp = "Cómo funciona la selección automática de ítems",
            sequence = "Cómo leer la secuencia de cotas inferiores", mono = "Cómo leer la monotonicidad",
            iio = "Cómo leer el orden invariante de los ítems", rest = "Cómo leer el método rest-score",
            pmat = "Cómo leer el método P-matrix", ca = "Cómo leer la asociación condicional",
            ml = "Cómo leer la escalabilidad de dos niveles",
            boot = "Cómo leer la estabilidad bootstrap"
        ),

        guide = list(
            scale = c(
                "Qué mide H: qué tan bien los ítems ordenan a las personas de la misma manera. Piense en una escala de Guttman: si los ítems se ordenan del más fácil al más difícil, quien aprueba un ítem difícil también debería aprobar todos los más fáciles. Cada vez que esto falla es un error de Guttman.",
                "Cómo funciona: H = 1 - (errores de Guttman observados / errores esperados si los ítems no estuvieran relacionados). H = 1 significa ningún error (una escala de Guttman perfecta); H = 0, tantos errores como el azar; valores negativos, más errores que el azar.",
                "Hi es la misma idea para un ítem frente a todos los demás, y H para el conjunto completo. Criterios de Mokken: cada Hi y H de al menos .30; .30-.40 débil, .40-.50 moderada, .50 o más fuerte. Son convenciones, no leyes: léalas siempre junto con sus intervalos de confianza.",
                "Los errores estándar muestran cuánto variaría H de una muestra a otra. La prueba z indica si un coeficiente está con seguridad por encima de la cota inferior, lo que es evidencia más fuerte que una estimación puntual que casualmente supera .30.",
                "Lectura errónea común: H no es un coeficiente de fiabilidad ni una correlación. Una escala larga puede ser muy fiable (alfa alto) y aun así tener escalabilidad débil, porque la fiabilidad premia el número de ítems mientras que H mide con qué consistencia ordenan a las personas."
            ),
            hij = c(
                "Hij es la escalabilidad de un par de ítems: los errores de Guttman de ese par en relación con los esperados por azar. Bajo el modelo, todo Hij es no negativo en la población; en una muestra pueden aparecer estimaciones negativas pequeñas por azar, así que valore su tamaño e incertidumbre.",
                "Lea la matriz por filas: el ítem cuya fila tiene muchos valores bajos o negativos es el que no encaja. Un solo Hij negativo suele ser la huella de un ítem que aún necesita invertirse.",
                "Lectura errónea común: algunos Hij por debajo de .30 no son un problema por sí mismos; el criterio de .30 se aplica a Hi y a H. Lo que importa a nivel de pares es el signo."
            ),
            reliability = c(
                "La fiabilidad es la proporción de la varianza de la puntuación total que no es error de medida: con qué consistencia la suma ordenaría a las personas si se repitiera la prueba.",
                "MS (Molenaar-Sijtsma) es la estimación de fiabilidad propia del análisis de Mokken: usa las funciones de respuesta estimadas de los pasos de ítem y tiene menos sesgo que alfa cuando se cumple el orden invariante de los ítems. Lambda-2 y alfa son cotas inferiores de la fiabilidad; lambda-2 nunca es menor que alfa. LCRC estima la fiabilidad con un modelo de clases latentes y suele ser la menos sesgada cuando se cumplen sus supuestos.",
                "Suelen esperarse valores de .80 o más para investigación sobre grupos, y de .90 o más para decisiones sobre individuos.",
                "La correlación ítem-resto es la correlación de cada ítem con la suma de los demás. Un valor bajo señala un ítem que aporta poco a la puntuación total."
            ),
            errors = c(
                "Los errores de Guttman de una persona (G+) cuentan los pares de pasos de ítem que respondió en el orden inesperado: aprobar un paso que la mayoría falla mientras falla uno que la mayoría aprueba. Las personas con muchos errores responden de forma inconsistente con el orden de los ítems del resto de la muestra.",
                "O+ cuenta cuántas de las combinaciones de puntajes de la persona son raras en la muestra. Ambos son estadísticos de ajuste de la persona: evalúan a la persona, no al ítem.",
                "Las vallas son umbrales de atípicos. La valla de Tukey (U1) supone una distribución aproximadamente simétrica; los conteos de errores suelen ser asimétricos, así que la valla ajustada por medcouple (U2) es mejor guía.",
                "Lectura errónea común: un atípico es una señal para examinar, no una razón para eliminar. Los patrones inusuales pueden venir de respuestas descuidadas, pero también de personas para quienes el atributo funciona distinto, y eliminarlas cambia lo que la escala significa."
            ),
            bounds = c(
                "Ellis (2014) mostró que, si ítems dicotómicos miden un atributo con funciones de respuesta monótonas, la correlación entre dos ítems no puede ser arbitrariamente pequeña ni grande: debe quedar entre cotas calculadas a partir de sus asociaciones con los demás ítems.",
                "Una correlación fuera de sus cotas es evidencia contra una sola variable latente monótona para ese par, aun cuando los coeficientes de escalabilidad parezcan aceptables.",
                "L2 y U1 son las cotas conservadoras: solo cruzarlas es una violación clara. L1 y U2 son indulgentes: cruzar solo estas es una posible violación que merece una segunda mirada."
            ),
            norms = c(
                "Las normas traducen una puntuación total en una posición dentro de una población de referencia: una puntuación típica (z), un estanino (nueve bandas de la distribución normal) o un rango percentil (porcentaje del grupo normativo que puntúa más bajo, contando la mitad de quienes tienen el mismo puntaje).",
                "Las normas son estimaciones a partir de una muestra, así que también tienen errores estándar (Oosterhuis et al., 2017; ver referencias). Intervalos amplios indican que la muestra normativa es demasiado pequeña para decisiones precisas en ese puntaje.",
                "Use estas normas solo si esta muestra representa a la población con la que quiere comparar a las personas; de lo contrario, describen solo a esta muestra."
            ),
            aisp = c(
                "La selección automática de ítems responde una pregunta previa a evaluar una escala: ¿cuáles de estos ítems van juntos en una o más escalas de Mokken? Es una herramienta exploratoria, comparable en propósito al análisis factorial exploratorio pero basada en la escalabilidad en lugar de las correlaciones.",
                "Paso 1: el par de ítems con el mayor Hij significativo inicia la primera escala. Paso 2: se agrega el ítem que más aumenta H, si su Hi con los ítems seleccionados es al menos la cota inferior c y es significativo. Paso 3: el paso 2 se repite hasta que ningún ítem califique. Paso 4: los ítems restantes inician una segunda escala, y así sucesivamente. Los ítems que nunca califican son no escalables (escala 0).",
                "Los números de escala son etiquetas en orden de formación, no de calidad: la escala 1 es simplemente la primera construida, por lo general la más grande.",
                "Lectura errónea común: una escala seleccionada todavía no es una escala validada. La selección solo garantiza la escalabilidad; aún hay que verificar la monotonicidad, el orden invariante de los ítems y la coherencia teórica de los ítems."
            ),
            sequence = c(
                "Cada columna muestra la partición obtenida con una cota inferior c; cada celda es la escala a la que se une un ítem (0 = no escalable).",
                "Patrón de un atributo: con c bajo todos los ítems forman una escala, y al subir c los ítems salen de ella uno por uno y quedan no escalables, pero no aparece una segunda escala.",
                "Patrón de varios atributos: al subir c, la escala única se divide en dos o más escalas que conservan sus ítems a lo largo de un rango de valores de c. Las agrupaciones estables entre valores vecinos de c son las que conviene interpretar.",
                "Elija el c en el que la partición es estable y teóricamente interpretable, por lo general entre .30 y .45, y justifíquelo en el informe."
            ),
            mono = c(
                "La monotonicidad significa que la probabilidad de un puntaje de ítem más alto nunca disminuye cuando el atributo aumenta. Es el supuesto central de los modelos de Mokken: sin él, una puntuación total más alta no significaría más del atributo.",
                "El atributo no se observa, así que se reemplaza por el rest score: la puntuación total en los demás ítems. Las personas se agrupan por rest score y, para cada paso de ítem, la proporción P(X ≥ k) se compara entre grupos. Cada disminución mayor que la violación mínima (#vi) se contrasta; #zsig cuenta las significativas.",
                "Crit combina todos los estadísticos de violación en un solo valor (Molenaar y Sijtsma, 2000): menos de 40 sin violación seria, 40 a 80 dudosa, más de 80 una violación seria. Es una heurística, no una prueba.",
                "En el gráfico, las curvas deberían subir de izquierda a derecha. Pequeñas caídas dentro de la banda de confianza son ruido muestral; una caída clara y sostenida es una violación real."
            ),
            iio = c(
                "El orden invariante de los ítems (IIO) significa que los ítems se ordenan por dificultad de la misma manera para cada persona, cualquiera sea su nivel del atributo. Se necesita cuando se interpreta el orden de los ítems en sí, por ejemplo en escalas evolutivas o jerárquicas, o cuando se compara a las personas en ítems individuales.",
                "MIIO lo verifica par por par: dentro de cada grupo de rest-score, el ítem más fácil debe tener la media más alta. Los ítems con demasiadas violaciones se eliminan de a uno (selección hacia atrás) hasta que el conjunto restante tenga un orden invariante.",
                "HT resume con qué precisión están ordenados los ítems restantes: menos de .30 el orden es demasiado impreciso para usarlo, .30 a .40 débil, .40 a .50 medio, .50 o más alto (Ligtvoet et al., 2010). Informe HT solo después de que la selección hacia atrás haya quitado los ítems con violaciones.",
                "Lectura errónea común: una escala puede ser perfectamente útil sin IIO. Para ordenar a las personas por la puntuación total importa el modelo de homogeneidad monótona (un atributo, independencia local y monotonicidad); el IIO es un requisito adicional solo cuando se interpreta el orden de los ítems en sí."
            ),
            rest = c(
                "El método rest-score verifica la no intersección de las funciones de respuesta de los pasos de ítem, que exige el modelo de doble monotonicidad: para dos ítems cualesquiera, las curvas de sus pasos no deben cruzarse.",
                "Para cada par de ítems, las personas se agrupan por el rest score en los ítems restantes, y el orden de las proporciones de los pasos de ambos ítems se compara entre grupos. Una inversión mayor que la violación mínima se cuenta y se contrasta, y crit resume la evidencia como en la monotonicidad.",
                "En el gráfico, las dos curvas de cada par deberían avanzar lado a lado sin cruzarse."
            ),
            pmat = c(
                "El método P-matrix verifica la no intersección con dos matrices: P(++) contiene la proporción de personas que superan cada par de pasos de ítem, y P(--) la proporción que falla ambos.",
                "Bajo el modelo de doble monotonicidad, cuando las columnas se ordenan del paso de ítem más popular al menos popular, cada fila de P(++) debe disminuir y cada fila de P(--) aumentar. Una inversión es una violación.",
                "En el gráfico, cada línea es un paso de ítem: las líneas de P(++) solo deberían bajar y las de P(--) solo subir."
            ),
            ca = c(
                "La independencia local significa que, una vez considerado el atributo, las respuestas a los ítems no están relacionadas. Los ítems localmente dependientes (por ejemplo, ítems que comparten un estímulo o una redacción casi idéntica) inflan la escalabilidad y la fiabilidad.",
                "La asociación condicional (CA) da consecuencias contrastables de la independencia local. Para cada par de ítems se examinan covarianzas condicionadas a puntuaciones de otros ítems: W1 y W3 marcan dependencia local positiva y W2 dependencia local negativa, con reglas de atípicos sobre estos índices.",
                "El procedimiento elimina el ítem con peores marcas y repite la verificación con los ítems restantes, hasta que ninguno quede marcado. Al ser paso a paso, no garantiza el mayor conjunto posible sin ítems marcados.",
                "Los ítems eliminados son candidatos a dependencia local, no ítems malos automáticamente: examine su contenido antes de borrarlos."
            ),
            ml = c(
                "En datos de dos niveles varios evaluadores puntúan al mismo sujeto: alumnos evalúan a su docente, enfermeras a un paciente, observadores a un aula. La escala busca ordenar a los sujetos, no a los evaluadores.",
                "HW (dentro) es la escalabilidad habitual calculada con los puntajes de cada evaluador: indica si cada evaluador usa los ítems de forma consistente. HB (entre) usa puntajes de distintos evaluadores del mismo sujeto: indica si los ítems ordenan a los sujetos de forma consistente entre evaluadores. Para una escala que mide sujetos, HB es el coeficiente que más importa.",
                "BW = HB / HW compara ambos. Cerca de 1, los evaluadores concuerdan entre sí casi tanto como consigo mismos; cerca de 0, el orden existe dentro de cada evaluador pero no entre ellos, y las puntuaciones de los sujetos dependen de quién los evaluó.",
                "Los errores estándar consideran el anidamiento (Koopman et al., 2020; ver referencias). Como HB suele ser mucho menor que HW, contraste si HB es mayor que cero antes de interpretar la escala como medida de los sujetos."
            ),
            boot = c(
                "Una partición encontrada en una muestra puede no replicarse: la selección automática es voraz y capitaliza el azar, así que los ítems límite pueden cambiar de escala con pequeños cambios en los datos.",
                "El bootstrap repite la selección, con las mismas opciones, en muchas muestras extraídas con reemplazo de sus personas (conglomerados completos, con datos anidados), como podría verse una nueva muestra de la misma población.",
                "Con sus compañeros de escala: para cada ítem, la proporción media de muestras en que cae en la misma escala que cada ítem con el que fue agrupado. Misma escala que la encontrada: la proporción de muestras en que su conjunto de compañeros es exactamente el encontrado. No escalable: con qué frecuencia quedó fuera.",
                "No hay un punto de corte acordado. Valores cercanos a 1 indican que la agrupación apenas depende de la muestra; los ítems que cambian de escala en una parte grande de las muestras deben interpretarse, e informarse, como inciertos. La estabilidad bootstrap no es replicación: confirme la partición con datos nuevos."
            )
        ),

        ml = list(
            intro = c(
                "El análisis de escalamiento de Mokken de dos niveles evalúa una escala cuyos ítems son puntuados por varios evaluadores por sujeto, como alumnos que evalúan a su docente (Snijders, 2001; Crisan et al., 2016; ver referencias).",
                "Divide la escalabilidad en una parte dentro de evaluadores (HW), una entre evaluadores (HB) y su razón (BW). Seleccione la variable que identifica al sujeto evaluado y luego los ítems."
            ),
            too_few_subjects = "La variable de sujeto tiene menos de dos sujetos. La escalabilidad de dos niveles compara evaluadores de distintos sujetos; seleccione la variable que identifica a la persona u objeto evaluado.",
            failed = "MokkenLab no pudo calcular los coeficientes de dos niveles: %s. Verifique que cada sujeto tenga varios evaluadores y que los ítems varíen dentro de los sujetos.",
            key_z_failed = "Pruebas Z (escalabilidad de dos niveles): las pruebas no pudieron calcularse: %s.",
            single_rater = "%d sujeto(s) tienen un solo evaluador; no aportan nada a los coeficientes entre evaluadores.",
            design_note = "Diseño: cada fila es un evaluador, y la variable de sujeto identifica a la persona u objeto que esos evaluadores puntuaron (alumnos que evalúan a su docente, observadores que evalúan un aula). Una agrupación como el sexo, el grado escolar o el país no es un sujeto: el análisis se ejecuta, pero HB y BW pierden sentido. Para comparar grupos, use la variable de agrupación del Análisis de escalamiento de Mokken.",
            few_subjects = "Solo %d sujetos. Los coeficientes entre evaluadores describen cómo los ítems ordenan a los sujetos, así que su precisión depende sobre todo del número de sujetos, no de evaluadores; con tan pocos, lea HB y BW como muy imprecisos y confirme que la variable de sujeto es realmente la persona u objeto evaluado.",
            key_few_subjects = "Diseño de dos niveles: solo %d sujetos; los coeficientes entre evaluadores son muy imprecisos.",
            z_note = "z: prueba %2$s de cada coeficiente contra %1$s; p es unilateral. mokken no contrasta la razón BW.",
            read_hw = "Consistencia del orden de los ítems de cada evaluador",
            read_hb = "Consistencia del orden de los sujetos entre evaluadores",
            read_bw = "Acuerdo entre evaluadores en relación con su consistencia",
            icc_failed = "Las correlaciones intraclase no pudieron calcularse: %s.",
            icc_note = "ICC: proporción de la varianza que está entre sujetos (Snijders y Bosker, 2012). La prueba F de la puntuación total verifica si los sujetos difieren en absoluto.",
            key_icc = "Correlaciones intraclase: ICC de la puntuación total = %s.",
            weights_failed = "Los pesos no pudieron calcularse: %s.",
            weights_ties = "No computable para este par: dos o más de sus pasos de ítem son igual de populares, y mokken 3.1.2 falla al promediar los pesos sobre los órdenes empatados (un error interno de MLweight, no un problema de sus datos). Los demás pares no se ven afectados.",
            pairs_mismatch = "mokken devolvió un número de pares de ítems que no coincide con los ítems seleccionados, así que los pares no pueden rotularse con seguridad.",
            weights_note = "Cada celda es el peso de un patrón de puntajes del par de ítems en los errores de Guttman de dos niveles (Koopman et al., 2017): 0 para patrones consistentes con el orden de los pasos de ítem, mayor para errores más serios. Los empates en la popularidad de los pasos se promedian entre los órdenes posibles.",
            interp_values = "Dentro de evaluadores, H = %s; entre evaluadores, H = %s; su razón BW = %s.",
            key_values = "Escalabilidad de dos niveles: HW = %s, HB = %s, BW = %s.",
            interp_hb_zero = "HB es cero o negativo: los ítems no ordenan a los sujetos de forma consistente entre evaluadores. La escala puede describir las percepciones de cada evaluador (HW), pero no mide diferencias entre sujetos. Revise la capacitación de los evaluadores, la redacción de los ítems o si el atributo es realmente una propiedad del sujeto.",
            key_hb_zero = "Escalabilidad de dos niveles: HB no es positivo; la escala no ordena a los sujetos entre evaluadores.",
            interp_hb_ci = "HB es positivo pero su intervalo de confianza incluye el cero, así que esta muestra no muestra que los ítems ordenen a los sujetos de forma consistente entre evaluadores. Más sujetos (no solo más evaluadores por sujeto) harían más preciso a HB.",
            key_hb_ci = "Escalabilidad de dos niveles: el intervalo de HB incluye el cero.",
            interp_hb_pos = "HB es positivo y su intervalo de confianza excluye el cero: los ítems ordenan a los sujetos de forma consistente entre evaluadores, lo que respalda usar la escala para comparar sujetos.",
            interp_bw = "BW = %s: cuanto más cerca de 1, más concuerdan entre sí los evaluadores del mismo sujeto en relación con su propia consistencia; valores bajos indican que la puntuación del sujeto depende bastante de quién lo evaluó, así que conviene promediar varios evaluadores por sujeto.",
            interp_items_hb = "Ítems cuya escalabilidad entre evaluadores es cero o negativa (%d): %s. Los evaluadores no concuerdan en cómo estos ítems ordenan a los sujetos; aportan poco a medir a los sujetos.",
            key_items_hb = "Escalabilidad de dos niveles de los ítems: %d ítem(s) con HBi no positivo.",
            interp_next = "Siguiente paso: informe HW, HB y BW con sus errores estándar, el número de sujetos y de evaluadores por sujeto, y verifique la monotonicidad en Supuestos del modelo con el sujeto como variable de conglomerado."
        ),

        check = list(
            intro = c(
                "La escalabilidad muestra que los ítems ordenan a las personas de forma consistente; este análisis verifica si se cumplen los supuestos que justifican usar su puntuación total (Sijtsma y Molenaar, 2002; Sijtsma y van der Ark, 2017; ver referencias).",
                "El modelo de homogeneidad monótona (un atributo, independencia local, monotonicidad) justifica ordenar a las personas por su puntuación total. El modelo de doble monotonicidad agrega funciones de respuesta de los pasos de ítem que no se cruzan, lo que también justifica ordenar los ítems de la misma manera para todos (orden invariante).",
                "Verifique primero la monotonicidad. Verifique el orden invariante, la no intersección y la independencia local cuando el uso que hará de la escala lo requiera."
            ),
            failed = "La verificación no pudo calcularse: %s.",
            verdict_none = "Sin violaciones",
            verdict_na = "No evaluable",
            not_evaluable = "%s: la verificación no pudo evaluarse para %d ítem(s): %s. Esto no es evidencia de que cumplan el supuesto; suele indicar muy pocas personas en los grupos de rest-score. Baje el tamaño mínimo de grupo o reúna más datos.",
            key_not_evaluable = "%s: %d ítem(s) no pudieron evaluarse.",
            verdict_minor = "Sin violación seria",
            verdict_doubtful = "Dudosa",
            verdict_serious = "Violación seria",
            crit_note = "Crit (Molenaar y Sijtsma, 2000): menos de 40 sin violación seria, 40-80 dudosa, más de 80 seria. #ac: comparaciones activas; #vi: violaciones mayores que la mínima; #zsig (o #tsig): violaciones significativas.",
            levels_note = "Dentro: los puntajes de los evaluadores dentro de cada conglomerado; Entre: las medias de los conglomerados (estadísticos de ajuste de dos niveles de Koopman et al.).",
            clusters_note = "Cada fila es un conglomerado de ítems con una posición empatada en el orden fijo; los estadísticos verifican si los conglomerados conservan su orden en cada grupo de rest-score.",
            order_note = "Orden fijo de los ítems (códigos en el orden de la lista de ítems): %s.",
            order_bad = "Los códigos de orden deben ser %d números enteros, uno por ítem, separados por comas o espacios.",
            ht_note = "HT se calcula con los ítems que quedan después de la selección hacia atrás (Ligtvoet et al., 2010). Menos de .30 impreciso, .30-.40 débil, .40-.50 medio, .50 o más alto.",
            ht_low = "Orden impreciso (HT < .30)",
            ht_weak = "Orden débil (.30 ≤ HT < .40)",
            ht_medium = "Orden medio (.40 ≤ HT < .50)",
            ht_high = "Alta precisión del orden (HT ≥ .50)",
            htb_meaning = "Precisión del orden entre conglomerados",
            htratio_meaning = "Parte de la precisión del orden que aportan los conglomerados",
            ca_note = "Cada paso elimina el ítem con peores marcas de dependencia local y vuelve a verificar el resto (Straat et al., 2016). Conservado: ítems del conjunto final, donde el procedimiento no marca más dependencia local; un procedimiento paso a paso no garantiza el mayor conjunto de ese tipo.",
            w_note = "Índices W del primer paso (todos los ítems). W1 y W3: dependencia local positiva con otro ítem (valor mayor y número de pares marcados); W2: dependencia local negativa. Las marcas usan reglas de atípicos sobre cada índice (Straat et al., 2016).",
            doubtful = "%s: ítems con violaciones dudosas (crit 40-80) (%d): %s. Examine sus curvas antes de decidir; con muestras grandes, violaciones pequeñas e irrelevantes se vuelven significativas.",
            mono_ok = "Monotonicidad: no se detectó ninguna violación seria con esta configuración. Esto respalda, pero no establece por sí solo, ordenar a las personas por la puntuación total: el modelo de homogeneidad monótona también exige un solo atributo e independencia local, y con ítems politómicos la puntuación total ordena a las personas solo en un sentido más débil, promedio (Hemker et al., 1997; ver referencias).",
            key_mono_ok = "Monotonicidad: sin violaciones serias.",
            mono_serious = "Monotonicidad: ítems con violaciones serias (crit > 80) (%d): %s. Para estos ítems un rest score más alto no significa de forma consistente un puntaje más alto en el ítem. Examine sus funciones de respuesta: una caída sostenida sugiere un ítem ambiguo o multidimensional, que conviene revisar o quitar.",
            key_mono_serious = "Monotonicidad: %d ítem(s) con violaciones serias (%s).",
            key_mono_doubtful = "Monotonicidad: %d ítem(s) con violaciones dudosas.",
            mono_failed = "La monotonicidad no pudo verificarse: %s.",
            iio_ok = "Orden invariante: ningún ítem muestra una violación seria, así que los ítems conservan el mismo orden de dificultad a lo largo del rango del atributo.",
            key_iio_ok = "Orden invariante: sin violaciones serias.",
            iio_serious = "Orden invariante: ítems con violaciones serias (crit > 80) (%d): %s. Su posición en el orden de dificultad cambia con el nivel del atributo.",
            key_iio_serious = "Orden invariante: %d ítem(s) con violaciones serias (%s).",
            key_iio_doubtful = "Orden invariante: %d ítem(s) con violaciones dudosas.",
            iio_removed = "La selección hacia atrás eliminó %d ítem(s), en este orden: %s. Sin ellos, el procedimiento no detecta más violaciones con esta configuración; eso no prueba que los ítems restantes tengan un orden invariante.",
            key_iio_removed = "Orden invariante: %d ítem(s) eliminados por la selección hacia atrás.",
            iio_ht = "HT = %s: %s. HT indica qué tan separadas están las funciones de respuesta de los ítems y, por lo tanto, con qué confiabilidad se ordenan los ítems.",
            key_ht = "Precisión del orden de los ítems: HT = %s (%s).",
            iio_failed = "El orden invariante no pudo verificarse: %s.",
            rest_ok = "Método rest-score: ningún ítem muestra una violación seria de la no intersección.",
            key_rest_ok = "Método rest-score: sin violaciones serias.",
            rest_serious = "Método rest-score: ítems con violaciones serias de la no intersección (crit > 80) (%d): %s. Las funciones de respuesta de sus pasos se cruzan con las de otros ítems.",
            key_rest_serious = "Método rest-score: %d ítem(s) con violaciones serias (%s).",
            key_rest_doubtful = "Método rest-score: %d ítem(s) con violaciones dudosas.",
            rest_failed = "El método rest-score no pudo calcularse: %s.",
            pmat_ok = "Método P-matrix: ningún ítem muestra una violación seria de la no intersección.",
            key_pmat_ok = "Método P-matrix: sin violaciones serias.",
            pmat_serious = "Método P-matrix: ítems con violaciones serias (crit > 80) (%d): %s.",
            key_pmat_serious = "Método P-matrix: %d ítem(s) con violaciones serias (%s).",
            key_pmat_doubtful = "Método P-matrix: %d ítem(s) con violaciones dudosas.",
            pmat_failed = "El método P-matrix no pudo calcularse: %s.",
            ca_ok = "Asociación condicional: ningún ítem fue marcado por dependencia local con esta configuración, lo que es coherente con la independencia local (no una prueba de ella).",
            key_ca_ok = "Independencia local: ningún ítem marcado.",
            ca_removed = "Asociación condicional: se eliminaron %d ítem(s) por dependencia local: %s. Revise si comparten contenido, redacción o un estímulo con otros ítems; los ítems localmente dependientes inflan la escalabilidad y la fiabilidad.",
            key_ca_removed = "Independencia local: %d ítem(s) eliminados (%s).",
            ca_failed = "La asociación condicional no pudo calcularse: %s.",
            section_mono = "Monotonicidad",
            section_iio = "Orden invariante",
            section_rest = "Método rest-score",
            section_pmat = "Método P-matrix",
            section_ca = "Asociación condicional",
            key_not_run = "No verificado en esta ejecución: %s. Su ausencia de las advertencias no dice nada sobre ellos.",
            interp_nothing = "No se seleccionó ninguna verificación.",
            interp_next = "Siguiente paso: revise o quite los ítems con violaciones serias, vuelva a ejecutar el Análisis de escalamiento de Mokken y estas verificaciones con los ítems restantes, e informe crit, los ítems eliminados y HT (si el orden invariante importa para su uso)."
        ),

        aisp = list(
            intro = c(
                "La selección automática de ítems divide un conjunto de ítems en escalas de Mokken: grupos de ítems en los que cada ítem tiene escalabilidad Hi de al menos una cota inferior c, más un conjunto de ítems no escalables (Mokken, 1971; Sijtsma y Molenaar, 2002; ver referencias).",
                "Úsela cuando aún no sabe si los ítems forman una escala, varias o ninguna, por ejemplo al desarrollar un cuestionario o al reexaminar uno existente."
            ),
            bad_sequence = "La secuencia de cotas inferiores está vacía: Desde debe ser menor que Hasta. Corrija los valores en Criterio de selección.",
            boot_failed = "El bootstrap no pudo calcularse: %s.",
            boot_note = "%d muestras bootstrap, semilla %d (la misma semilla y los mismos datos reproducen los mismos valores). La partición completa se reprodujo exactamente en una proporción de %s de las muestras.",
            boot_failed_some = "%d de %d muestras bootstrap no pudieron analizarse (por ejemplo, un ítem sin variación en la remuestra) y se dejaron fuera.",
            key_boot_weak = "Estabilidad bootstrap: %d ítem(s) conservan sus compañeros de escala en menos de la mitad de las muestras (%s); trate su asignación como incierta.",
            key_boot_ok = "Estabilidad bootstrap: cada ítem escalado conserva sus compañeros de escala en al menos la mitad de las muestras; la partición completa reaparece en una proporción de %s.",
            failed = "MokkenLab no pudo ejecutar la selección de ítems: %s. Verifique que los ítems sean puntajes con variación y, con el algoritmo genético, pruebe una población mayor o más generaciones.",
            z_changed = "La prueba Z original se reemplazó por la prueba de Wald, porque no puede contrastar Hi contra c ni considerar datos anidados.",
            start_ga = "El algoritmo genético no usa un conjunto inicial; el conjunto inicial se ignoró. Use el procedimiento de Mokken para iniciar la primera escala con ítems elegidos.",
            coef_failed = "La escalabilidad de esta escala no pudo calcularse: %s.",
            scales_note = "Inferior y Superior: intervalo de confianza de Wald al 95% de H. Ejecute el Análisis de escalamiento de Mokken sobre los ítems de cada escala para obtener intervalos, pruebas y detalle por ítem.",
            membership_note = "Los números de escala son etiquetas en orden de formación, no de calidad. Hi es la escalabilidad del ítem dentro de su propia escala.",
            sequence_note = "Cada celda es la escala a la que se une un ítem con esa cota inferior; 0 significa no escalable.",
            interp_none = "No pudo formarse ninguna escala con c = %s: ningún par de ítems tiene un Hij significativo de al menos ese valor. Los ítems no comparten suficiente orden común para formar una escala de Mokken. Baje c para explorar estructuras más débiles, revise si hay ítems negativos sin invertir o reconsidere si los ítems miden un atributo común.",
            key_none = "Pertenencia a las escalas: no pudo formarse ninguna escala de Mokken.",
            interp_one_all = "Todos los ítems se unieron en una sola escala con c = %s. Esto respalda tratarlos como una escala; revise su fuerza en la tabla de arriba y sus supuestos en Supuestos del modelo.",
            key_one_all = "Pertenencia a las escalas: todos los ítems forman una escala.",
            interp_many = "La selección formó %d escalas con c = %s. Los ítems ordenan a las personas de forma consistente dentro de cada grupo pero no entre grupos, lo que sugiere atributos relacionados pero distintos, o efectos de método como la redacción de los ítems.",
            key_many = "Pertenencia a las escalas: se formaron %d escalas.",
            interp_scale = "%s: %d ítems, H = %s (%s).",
            interp_unscalable = "Ítems no escalables (%d): %s. No alcanzaron un Hi de c con ninguna escala. Revise su contenido: pueden medir otro atributo, estar mal redactados o necesitar inversión.",
            key_unscalable = "Pertenencia a las escalas: %d ítem(s) no escalable(s) (%s).",
            interp_caution = "La selección es exploratoria y capitaliza el azar: con otra muestra los ítems límite pueden cambiar de escala. Confirme la partición con datos nuevos, con el algoritmo genético o con una secuencia de cotas inferiores.",
            interp_next = "Siguiente paso: ejecute el Análisis de escalamiento de Mokken sobre los ítems de cada escala para obtener intervalos y pruebas, y luego Supuestos del modelo para verificar la monotonicidad y, si hace falta, el orden invariante de los ítems.",
            interp_seq_intro = "La selección se ejecutó para %d cotas inferiores, de %s a %s.",
            interp_seq_split = "Desde c = %s, los ítems se dividen en más de una escala (hasta %d). Es el patrón de varios atributos relacionados: interprete las agrupaciones que se mantienen estables en valores vecinos de c.",
            key_seq_split = "Partición según la cota inferior: los ítems se dividen en varias escalas desde c = %s (hasta %d).",
            interp_seq_one = "En ninguna cota apareció una segunda escala: al subir c los ítems solo quedan no escalables. Es el patrón de un atributo medido por ítems de distinta calidad.",
            key_seq_one = "Partición según la cota inferior: nunca más de una escala (patrón de un atributo).",
            interp_seq_30 = "Con c = .30, el criterio habitual de Mokken, la selección da %d escala(s) y %d ítem(s) no escalable(s).",
            interp_seq_next = "Siguiente paso: elija la cota inferior en la que la partición es estable e interpretable, ejecute la selección con ese único valor y evalúe cada escala con el Análisis de escalamiento de Mokken y Supuestos del modelo."
        ),

        msa = list(
            intro = c(
                "El análisis de escalamiento de Mokken evalúa si un conjunto de ítems forma una escala que ordena a las personas en un atributo, con teoría de respuesta al ítem no paramétrica: solo supone que la probabilidad de un puntaje más alto en el ítem aumenta con el atributo, no una función de respuesta particular (Mokken, 1971; Sijtsma y Molenaar, 2002; ver referencias).",
                "El coeficiente de escalabilidad H compara los errores de Guttman observados (una persona que supera un paso de ítem más difícil pero falla uno más fácil) con los esperados si los ítems no estuvieran relacionados. Hij describe un par de ítems, Hi un ítem y H la escala completa. Regla de Mokken: una escala necesita H y cada Hi de al menos .30; .30 a .40 es débil, .40 a .50 moderada y .50 o más fuerte.",
                "Lea los resultados en este orden: el H de la escala con su intervalo y su prueba, los valores Hi de los ítems, los pares con Hij negativo y luego fiabilidad, atípicos y normas."
            ),
            key_z_failed = "Pruebas Z (Escalabilidad de la escala): las pruebas no pudieron calcularse: %s.",
            ci_note = "Intervalos de confianza: %s%%, %s.",
            hij_note = "Un Hij negativo indica que los dos ítems ordenan a las personas en direcciones opuestas: suele ser un ítem negativo sin invertir o un ítem que mide otro atributo.",
            sets_note = "Cada fila es un conjunto completado por imputación two-way con un error aleatorio distinto. Todas las demás tablas de este análisis provienen solo del primer conjunto; esta tabla es una verificación de sensibilidad, no una inferencia combinada de imputación múltiple: no se aplica ninguna regla de combinación, porque su validez para los estadísticos de Mokken no está establecida.",
            group_small = "grupos con menos de dos casos: %s",
            group_failed = "Los coeficientes no pudieron calcularse por grupo: %s. Cada grupo necesita suficientes casos con variación en cada ítem.",
            key_groups = "Escalabilidad por grupo: H va de %2$s a %3$s en %1$d grupos.",
            rel_none = "No se seleccionó ningún coeficiente de fiabilidad.",
            rel_failed = "La fiabilidad no pudo calcularse: %s.",
            rel_value_na = "Este coeficiente no pudo estimarse con estos datos.",
            lcrc_note = "Estimado con %d clases latentes (%s). LCRC puede cambiar con el número de clases y con los valores iniciales del modelo de clases latentes; vuelva a calcularlo con otro número de clases e informe el rango si los valores difieren.",
            lcrc_auto = "número de ítems menos uno, el valor por defecto de mokken",
            lcrc_manual = "fijado por usted",
            errors_none = "Seleccione G+ u O+ para calcular estadísticos de atípicos.",
            errors_failed = "Los estadísticos de atípicos no pudieron calcularse: %s.",
            errors_note = "G+ cuenta los errores de Guttman de cada persona, ponderados por la popularidad de los pasos de ítem; O+ cuenta patrones de puntaje infrecuentes (Zijlstra et al., 2007; ver referencias). U1 es la valla superior de Tukey (Q3 + 1.5 RIC) y U2 la valla ajustada por medcouple para distribuciones asimétricas (Hubert y Vandervieren, 2008; ver referencias). Las personas por encima de U2 son los candidatos a atípicos más claros.",
            bounds_polytomous = "Las cotas de correlación (Ellis, 2014) solo están definidas para ítems dicotómicos; estos ítems tienen más de dos categorías de puntaje.",
            bounds_none = "Seleccione cotas inferiores, superiores o ambas.",
            bounds_failed = "Las cotas de correlación no pudieron calcularse: %s.",
            bounds_note = "En un modelo monótono unidimensional, cada correlación entre ítems debe quedar entre cotas derivadas de los demás ítems (Ellis, 2014; ver referencias). L1 y U1 sobreestiman la cota inferior y la superior, L2 y U2 las subestiman, así que una correlación por debajo de L2 o por encima de U1 es una violación clara, y una por debajo de L1 o por encima de U2 solo una posible.",
            bounds_invalid_upper = "Para %d par(es) de ítems, mokken devolvió una cota superior fuera del rango de una correlación (-1 a 1). Una cota así no puede cruzarse ni respetarse de forma significativa, así que se muestra pero no se usa en el veredicto, que entonces descansa en las cotas inferiores.",
            key_bounds = "Cotas de correlación: %d par(es) de ítems violan las cotas esperadas bajo una sola variable latente monótona.",
            norms_failed = "Los estadísticos normativos no pudieron calcularse: %s.",
            norms_note = "Puntuación total = suma de los %d puntajes de ítem (ítems negativos invertidos). Son estadísticos normativos de esta muestra, no baremos de una población; solo se vuelven baremos si la muestra representa a la población con la que compara a las personas. Los errores estándar siguen a Oosterhuis et al. (2017; ver referencias).",
            z_note_original = "z: prueba Z original de cada coeficiente contra cero (Mokken, 1971); p es unilateral.",
            z_note_bound = "z: prueba %2$s de cada coeficiente contra la cota inferior %1$s; p es unilateral (coeficiente mayor que la cota).",
            interp_h_na = "El H de la escala no pudo estimarse, así que la escala no puede evaluarse. Revise los puntajes de los ítems y los mensajes en Notas.",
            interp_h_low = "El H de la escala es %s, por debajo del mínimo de Mokken de .30. Los ítems no ordenan a las personas con suficiente consistencia para sumarse en una puntuación de escala: hay casi tantos errores de Guttman como se esperarían si los ítems no estuvieran relacionados. Esto suele ocurrir cuando los ítems miden más de un atributo o incluyen ítems no relacionados con el resto. Ejecute la Selección automática de ítems para ver si los ítems se dividen en escalas separadas.",
            interp_h_ok = "El H de la escala es %s: una %s. En conjunto, los ítems ordenan a las personas con más consistencia que ítems no relacionados. Es necesario pero no suficiente para usar su suma: un único H no muestra que los ítems midan un solo atributo (un conjunto formado por dos subescalas fuertes también puede alcanzarlo; revise la Selección automática de ítems) ni que se cumplan los supuestos del modelo (revise Supuestos del modelo).",
            interp_ci_low = "El límite inferior del intervalo de confianza de H (%s) está por debajo de .30, así que esta muestra no descarta que la escala sea no escalable. Interprete su fuerza con cautela y prefiera una prueba contra .30 (de Wald o preservadora de rango) a la estimación puntual.",
            key_h = "Escalabilidad de la escala: H = %s, %s.",
            key_ci_low = "Escalabilidad de la escala: el intervalo de confianza de H llega por debajo de .30.",
            interp_low_items = "Ítems con Hi por debajo de .30 (%d): %s. En este conjunto de ítems, cada uno ordena a las personas con menos consistencia respecto del resto de lo que exige el criterio de Mokken. Antes de quitar alguno, verifique si necesita invertirse y si el conjunto mezcla subescalas: un ítem puede ajustar bien en su propia subescala y aun así tener un Hi bajo en un conjunto mezclado. Si quita ítems, hágalo de a uno y repita el análisis, porque cada Hi cambia al quitar un ítem.",
            key_low_items = "Escalabilidad de los ítems: %d ítem(s) con Hi por debajo de .30 (%s).",
            interp_items_ok = "Todos los ítems tienen Hi de al menos .30, así que cada ítem contribuye a ordenar a las personas.",
            interp_z_nonsig = "Ítems cuyo Hi no es significativamente mayor que %s (%d): %s. Sus estimaciones puntuales pueden cumplir el criterio, pero la muestra no aporta evidencia suficiente; una muestra mayor o quitar los ítems más débiles lo aclararía.",
            key_z_nonsig = "Escalabilidad de los ítems: %d ítem(s) no significativamente por encima de %s.",
            interp_neg_pairs = "Pares de ítems con un Hij estimado negativo (%d): %s. En la población, el modelo de homogeneidad monótona exige que todo Hij sea no negativo. Una estimación claramente negativa suele indicar un ítem sin invertir o que mide otro atributo; una estimación cercana a cero puede ser error muestral, así que valore su tamaño y su error estándar antes de concluir.",
            key_neg_pairs = "Escalabilidad de los pares: %d par(es) con Hij negativo.",
            interp_sets = "Entre los conjuntos completados, H va de %s a %s. Un rango estrecho indica que las conclusiones no dependen de los puntajes imputados.",
            interp_rel = "La fiabilidad Molenaar-Sijtsma de la puntuación total es %s. MS usa las funciones de respuesta de los pasos de ítem estimadas bajo el modelo de doble monotonicidad, así que es más confiable cuando se cumple el orden invariante de los ítems (verifíquelo en Supuestos del modelo). Una fiabilidad alta no muestra que la puntuación total mida un solo atributo: la suma de dos subescalas correlacionadas puede ser muy fiable.",
            key_rel = "Fiabilidad: MS = %s.",
            interp_errors = "%d de %d personas tienen más errores de Guttman que la valla ajustada U2. Sus patrones de respuesta son inusualmente inconsistentes con el orden de los ítems: posible respuesta descuidada, malentendido o un proceso de respuesta distinto. Examínelas antes de decidir; no las elimine automáticamente.",
            key_errors = "Errores de Guttman: %d persona(s) por encima de la valla ajustada por asimetría.",
            interp_errors_none = "Ninguna persona tiene más errores de Guttman que la valla ajustada U2, así que ningún patrón de respuesta es extremo una vez considerada la asimetría de los conteos de errores. %d persona(s) superan solo la valla de Tukey U1, que supone una distribución simétrica y marca de más con conteos asimétricos; examínelas solo si tiene una razón sustantiva.",
            norms_perc_range = "En los extremos del rango de puntajes, el intervalo de Wald de un rango percentil puede pasar de 0 o de 100 (Oosterhuis et al., 2017). Los valores se muestran tal como los calcula mokken; lea esos límites como 0 o 100. El gráfico dibuja la banda dentro del rango posible.",
            interp_next_ok = "Siguiente paso: verifique los supuestos del modelo (monotonicidad, y orden invariante de los ítems si necesita que los ítems se ordenen igual para cada persona) en Supuestos del modelo antes de usar la puntuación total.",
            interp_next_aisp = "Siguiente paso: ejecute la Selección automática de ítems con los mismos ítems para encontrar los subconjuntos que sí forman escalas de Mokken, y luego verifique los supuestos del modelo en cada escala.",
            interp_imputed = "Los puntajes perdidos se completaron por imputación two-way; informe el número de puntajes imputados y la semilla junto con los resultados."
        )
    )
)

# -----------------------------------------------------------------------------
# Lookup helpers.
# ES: Funciones de búsqueda.
#
# .mk_normalize_lang() maps whatever jamovi passes to "en" or "es".
# .mk_text() resolves (lang, section, key), falling back to English and then
# to a visible "[missing text: ...]" marker, so a missing string is caught in
# review instead of rendering as an empty paragraph.
#
# ES: .mk_normalize_lang() convierte lo que entregue jamovi en "en" o "es".
# .mk_text() resuelve (idioma, sección, clave), recurre al inglés y luego a
# una marca visible "[missing text: ...]", para que un texto faltante se
# detecte en revisión en lugar de mostrarse como un párrafo vacío.
# -----------------------------------------------------------------------------
.mk_normalize_lang <- function(lang) {
    if (is.null(lang) || length(lang) == 0 || is.na(lang[1]))
        return("en")
    lang_low <- tolower(as.character(lang)[1])
    if (lang_low %in% c("es", "esp", "español", "espanol", "spanish"))
        return("es")
    "en"
}

.mk_text <- function(lang, section, key) {
    lang <- .mk_normalize_lang(lang)
    value <- .mk_texts[[lang]][[section]][[key]]
    if (is.null(value))
        value <- .mk_texts[["en"]][[section]][[key]]
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

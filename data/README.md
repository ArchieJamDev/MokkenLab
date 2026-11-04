# MokkenLab example datasets

Four datasets ship with the module. In jamovi they appear under
**File › Open › Data Library › Mokken Lab** once the module is installed.
All four come from the mokken R package (version 3.1.2, GPL ≥ 2, compatible
with MokkenLab's GPL-3); [`data-raw/prepare_dataset.R`](../data-raw/prepare_dataset.R)
rebuilds every file from it. Nothing is recoded, imputed or removed.

| File | Rows × columns | Use it for |
|---|---|---|
| `mokkenlab_ds14.csv` | 541 × 16 | Polytomous items (0-4), real missing data (two-way imputation), negatively worded items (Si1, Si3), two subscales (automated item selection), groups by sex |
| `mokkenlab_transreas.csv` | 425 × 13 | Dichotomous items (correlation bounds), groups by school grade |
| `mokkenlab_autonomy.csv` | 259 × 8 | Two-level data: students (raters) nested in 14 teachers (subjects) |
| `mokkenlab_trog.csv` | 210 × 80 | Clustered items: 20 clusters of four (a1-a4 ... t1-t4); invariant ordering of item clusters |

## `mokkenlab_ds14.csv`

The Type D Scale (DS14; Denollet, 2005): 14 items scored 0-4 measuring
negative affectivity (Na) and social inhibition (Si), plus `Male` (sex) and
`Age`. mokken marks the negatively worded items as `Si1*` and `Si3*`; the
asterisk is removed from the names here, so declare `Si1` and `Si3` under
**Negatively worded items**.

## `mokkenlab_transreas.csv`

Transitive reasoning tasks (Verweij et al., 1996; Sijtsma & Molenaar, 2002):
12 dichotomous items and `Grade` (school grade 2 to 6).

## `mokkenlab_autonomy.csv`

Autonomy support (Koopman et al., 2020): 259 students rated the autonomy
support of their teacher on seven items scored 1-5. `Teacher` identifies the
rated subject; use it as **Subject** in Two-Level Mokken Scale Analysis or as
the cluster variable elsewhere.

## `mokkenlab_trog.csv`

The Norwegian adaptation of the Test for Reception of Grammar (TROG; Bishop,
1979), first measurement of a developmental study (Brinchmann et al., 2019):
210 children, 80 dichotomous items in 20 clusters of four. It is the
clustered-item example of Koopman and Braeken for investigating the ordering
structure of a test: in **Model Assumptions**, tick Invariant item ordering,
untick Backward item selection and choose Item order **Fixed, clusters of
consecutive items** with 4 items per cluster.

## References

- Bishop, D. V. M. (1979). Comprehension in developmental language disorders. *Developmental Medicine & Child Neurology, 21*(2), 225–238. https://doi.org/10.1111/j.1469-8749.1979.tb01605.x
- Brinchmann, E. I., Braeken, J., & Lyster, S.-A. H. (2019). Is there a direct relation between the development of vocabulary and grammar? *Developmental Science, 22*(1), e12709. https://doi.org/10.1111/desc.12709
- Denollet, J. (2005). DS14: Standard assessment of negative affectivity, social inhibition, and Type D personality. *Psychosomatic Medicine, 67*(1), 89–97. https://doi.org/10.1097/01.psy.0000149256.81953.49
- Koopman, L., Zijlstra, B. J. H., & van der Ark, L. A. (2020). Standard errors of two-level scalability coefficients. *British Journal of Mathematical and Statistical Psychology, 73*(2), 213–236. https://doi.org/10.1111/bmsp.12174
- Sijtsma, K., & Molenaar, I. W. (2002). *Introduction to nonparametric item response theory*. Sage. https://doi.org/10.4135/9781412984676
- van der Ark, L. A., & Koopman, L. (2024). *mokken: Conducts Mokken scale analysis* (Version 3.1.2) [R package]. https://CRAN.R-project.org/package=mokken
- Verweij, A. C., Sijtsma, K., & Koops, W. (1996). A Mokken scale for transitive reasoning suited for longitudinal research. *International Journal of Behavioral Development, 19*(1), 219–238. https://doi.org/10.1177/016502549601900115

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
# Build the bundled example datasets.
# ES: Construir los conjuntos de datos de ejemplo incluidos.
#
# Source: four data sets shipped with the mokken R package (GPL >= 2), so
# MokkenLab's results can be checked against the package's own published
# examples. Run from the package root: Rscript data-raw/prepare_dataset.R
#
# ES: Fuente: cuatro conjuntos incluidos en el paquete de R mokken (GPL >=
# 2), para que los resultados de MokkenLab puedan contrastarse con los
# ejemplos publicados del paquete. Ejecutar desde la raíz del paquete:
# Rscript data-raw/prepare_dataset.R
#
# Nothing is recoded, imputed or removed. The only changes are column names
# that jamovi would otherwise mangle (DS14 marks its negatively worded items
# with "*", which becomes part of the name) and factor labels for codes.
#
# ES: No se recodifica, imputa ni elimina nada. Los únicos cambios son
# nombres de columna que jamovi alteraría (DS14 marca sus ítems negativos con
# "*", que pasa a formar parte del nombre) y etiquetas de factor para
# códigos.
# -----------------------------------------------------------------------------

utils::data("DS14", "transreas", "autonomySupport", "trog", package = "mokken", envir = environment())

# Type D Scale (DS14): 541 respondents, 14 items scored 0-4, with real missing
# values. Si1 and Si3 (marked "*" in mokken) are negatively worded: declare
# them under Negatively worded items.
# ES: Escala tipo D (DS14): 541 personas, 14 ítems puntuados 0-4, con valores
# perdidos reales. Si1 y Si3 (marcados con "*" en mokken) están redactados en
# negativo: declárelos en Ítems redactados en negativo.
ds14 <- as.data.frame(DS14)
names(ds14) <- sub("\\*$", "", names(ds14))
ds14$Male <- factor(ds14$Male, levels = c(0, 1), labels = c("Female", "Male"))
utils::write.csv(ds14, file.path("data", "mokkenlab_ds14.csv"), row.names = FALSE, na = "")

# Transitive reasoning: 425 pupils, 12 dichotomous tasks, school grade.
# ES: Razonamiento transitivo: 425 alumnos, 12 tareas dicotómicas, grado
# escolar.
transreas <- as.data.frame(transreas)
names(transreas)[1] <- "Grade"
transreas$Grade <- factor(transreas$Grade)
utils::write.csv(transreas, file.path("data", "mokkenlab_transreas.csv"), row.names = FALSE, na = "")

# Autonomy support: 259 students (raters) nested in 14 teachers (subjects),
# seven items scored 1-5.
# ES: Apoyo a la autonomía: 259 estudiantes (evaluadores) anidados en 14
# docentes (sujetos), siete ítems puntuados 1-5.
autonomy <- as.data.frame(autonomySupport)
autonomy$Teacher <- factor(autonomy$Teacher)
utils::write.csv(autonomy, file.path("data", "mokkenlab_autonomy.csv"), row.names = FALSE, na = "")

# TROG: 210 children, 80 dichotomous items in 20 clusters of four (a1-a4 ...
# t1-t4), the clustered-item example of Koopman and Braeken.
# ES: TROG: 210 niños, 80 ítems dicotómicos en 20 conglomerados de cuatro
# (a1-a4 ... t1-t4), el ejemplo de ítems agrupados de Koopman y Braeken.
utils::write.csv(as.data.frame(trog), file.path("data", "mokkenlab_trog.csv"), row.names = FALSE, na = "")

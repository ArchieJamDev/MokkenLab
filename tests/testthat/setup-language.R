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

# The tests expect English output, so they run in English whatever the
# system locale is: jmvcore takes the results language from LANGUAGE before
# LC_MESSAGES. runAnalysisIn() overrides it for the tests of translations.
# ES: Las pruebas esperan salida en inglés, así que se ejecutan en inglés sea
# cual sea el idioma del sistema: jmvcore toma el idioma de LANGUAGE antes que
# de LC_MESSAGES. runAnalysisIn() lo cambia en las pruebas de traducción.
withr::local_envvar(c(LANGUAGE = "en"), .local_envir = testthat::teardown_env())

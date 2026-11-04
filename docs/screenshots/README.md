# Screenshots

Images used by the main README. They are real jamovi screenshots of
MokkenLab (English report and menu), taken with the bundled datasets.

| File | Content |
|---|---|
| `demo.gif` | Slideshow of the five screenshots below, in this order: item scalability, item-pair heat map, Guttman errors, norms, two-level scalability. 1000 × 540 px, 1.8 s for the first frame and 2.2 s for the rest, endless loop, as in AssumptionsLab and EGAnetLab |
| `scalability.png` | Mokken Scale Analysis of the Type D Scale DS14 (Si1 and Si3 reversed): item scalability plot with its confidence intervals |
| `hij-heatmap.png` | Item-pair scalability (Hij) heat map; the Na and Si subscales show as two blocks |
| `guttman-errors.png` | Distribution of Guttman errors (G+) and infrequent scores (O+) with the Tukey and skew-adjusted fences |
| `norms.png` | Percentile ranks of the total score with their band, and the applied interpretation |
| `two-level.png` | Two-Level Mokken Scale Analysis of Autonomy support (students rating their teacher; Teacher as subject): within and between scalability |

To rebuild `demo.gif`, crop the jamovi title bar and ribbon (top 154 px) from
each screenshot, fit it on a white 1000 × 540 canvas, and save the frames in
order as an animated GIF (ImageMagick: `convert -loop 0 -delay 180 f1.png
-delay 220 f2.png ... demo.gif`).

---

Imágenes que usa el README principal: capturas reales de MokkenLab en jamovi,
hechas con los conjuntos de datos incluidos. `demo.gif` recorre las cinco
capturas de la tabla con los mismos tiempos que el de AssumptionsLab y
EGAnetLab.

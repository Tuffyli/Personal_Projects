# Combined

This folder contains code to build a **single, combined graph** overlaying the major modern DiD/DrDiD estimators—so you can **visually compare** how each method behaves on the same data.

I couldn’t find an R implementation that brought all these estimators together in one place, so I aimed to make this as **accessible and plug-and-play** as possible.

## Estimators included

- **Callaway & Sant’Anna (2021)** — [“Difference-in-Differences with Multiple Time Periods”](https://www.sciencedirect.com/science/article/abs/pii/S0304407620303948)
- **Sun & Abraham (2021)** — [“Estimating Dynamic Treatment Effects in Event Studies with Heterogeneous Treatment Effects”](https://www.sciencedirect.com/science/article/abs/pii/S030440762030378X)
- **Borusyak, Jaravel & Spiess (2024)** — [“Revisiting Event Study Designs” (working paper)](https://www.econstor.eu/bitstream/10419/260392/1/1800643624.pdf)
- **de Chaisemartin & d’Haultfoeuille (2024)** — [“Two-Way Fixed Effects and Differences-in-Differences with Heterogeneous Treatment Effects” (NBER w29873)](https://www.nber.org/system/files/working_papers/w29873/w29873.pdf)

## What you get

- A combined plot stacking estimates from all methods above.
- A consistent interface in R to run each estimator and format results for side-by-side comparison.

## Quick start

1. Load your panel data (unit × time), with treatment timing.
2. Run the provided scripts to compute each estimator.
3. Call the plotting function to generate the combined figure.

> Tip: keep your treatment indicator/event time naming consistent across scripts for a painless run.

## Notes

- This repo focuses on **comparability of outputs** (estimands, CI conventions, and plotting scales).
- If you spot edge cases or want a new estimator added, feel free to open an issue or PR.

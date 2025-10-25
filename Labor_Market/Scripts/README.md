## Code Overview & Run Order

Each script covers a stage of the workflow. **Execution priority is indicated by the numeric prefix** in the filename:

1. **[2.data.R](./2.data.R)** — data loading, cleaning, and construction of panel/event-time variables.  
2. **[3.regression.R](./3.regression.R)** — main estimations (DiD/DrDiD wrappers) and tidy outputs.  
3. **[4.robust.R](./4.robust.R)** — robustness checks and alternative specs.  
4. **[coding_sample.R](./coding_sample.R)** — the combination of all current code files into a single one (incomplete).

> Tip: If you use restricted data, point paths in `2.data.R` to your local files and keep them out of Git.

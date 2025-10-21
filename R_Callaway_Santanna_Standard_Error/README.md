### Pre-Average ATT and SE with `did` (R)

Many R users find it non-trivial to extract the **pre-treatment average effect** (the “Pre-Average” across negative event times) **with a correct standard error** when using the `did` package.  
In Stata, `csdid` reports these quantities out of the box; in R, `aggte()` focuses on
event-time estimates and common aggregations, but the **variance of the pre-average aggregator** is not directly exposed after aggregation.

This note replicates that calculation in R by:
- **Identifying pre-treatment event times** from `aggte(type = "dynamic")`.
- **Averaging the corresponding ATT estimates** to form the Pre-Average.
- **Reconstructing its standard error** via the relevant covariance among those
  event-time estimates (delta-method style), yielding an estimate, SE, CI, and a simple
  pre-trend test.

**What you get**
- A reproducible routine to report the **Pre-Average ATT** and its **standard error** for
  the **Doubly Robust Difference-in-Differences** setup.
- Minimal example code in `Standard_Error_Pre_Avg.Rmd` and a rendered PDF.

> If your workflow differs (e.g., custom clustering or weighting), adapt the selection of event times and the covariance extraction accordingly.

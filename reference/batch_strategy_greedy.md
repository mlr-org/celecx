# Greedy Batch Strategy

Simple strategy that selects the k candidates with the best acquisition
scores. No diversification; may select clustered points.

## Usage

``` r
batch_strategy_greedy()
```

## Value

A batch strategy function.

## Details

This is the baseline strategy. Use when:

- batch_size = 1 (single point selection)

- Candidates are already diverse (e.g., from LHS)

- Speed is critical

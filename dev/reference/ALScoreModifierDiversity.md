# Diversity Active Learning Score Modifier

Blends acquisition utility with distance to points already selected for
the current batch.

Creates a diversity score modifier.

## Arguments

- diversity_weight:

  (`numeric(1)`)  
  Weight of the diversity term. `0` keeps pure acquisition utility, `1`
  uses only distance to already-selected batch points.

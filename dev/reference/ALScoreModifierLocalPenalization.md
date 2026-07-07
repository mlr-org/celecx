# Local-Penalization Active Learning Score Modifier

Penalizes candidates close to points already selected for the current
batch.

Creates a local-penalization score modifier.

## Arguments

- bandwidth:

  (`numeric(1)`)  
  Gower-distance radius in which utilities are penalized.

- penalization:

  (`numeric(1)`)  
  Amount subtracted from utilities. `Inf` removes nearby points.

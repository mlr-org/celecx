# Portfolio Active Learning Proposer

Combines child proposers by querying them round-robin until enough
unique points have been proposed or all children fail to produce another
point.

Creates a portfolio proposer.

## Arguments

- proposers:

  (named [`list()`](https://rdrr.io/r/base/list.html) of
  [ALProposer](https://celecx.mlr-org.com/reference/ALProposer.md))  
  Child proposers.

## Fields

- `proposers`:

  (named [`list()`](https://rdrr.io/r/base/list.html) of
  [ALProposer](https://celecx.mlr-org.com/reference/ALProposer.md))
  Child proposers.

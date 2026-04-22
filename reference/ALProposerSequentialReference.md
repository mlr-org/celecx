# Sequential Reference Active Learning Proposer

Builds a batch by repeatedly rescoring after treating already selected
batch points as temporary distance references.

This is exact for GSx-style acquisition functions whose scores only
depend on input-space references. It is a heuristic for acquisitions
that also depend on labels or model predictions.

It may be necessary to use ALProposerSequentialReference even when
proposing only a single point with this proposer itself if the proposer
is part of a portfolio of proposers.

Creates a sequential-reference proposer.

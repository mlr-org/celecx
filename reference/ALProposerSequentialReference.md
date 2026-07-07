# Sequential Reference Active Learning Proposer

Builds a batch by repeatedly rescoring after treating already selected
batch points as temporary distance references.

This requires a *label-free*
[AcqFunctionDist](https://mlr-org.github.io/celecx/reference/AcqFunctionDist.md)
(such as GSx), whose scores only depend on the input-space reference
points: the proposer overrides the reference points with a mix of
unlabeled batch candidates and archive rows, which breaks acquisitions
that align distance columns with archive labels positionally (such as
iGS or IDEAL).

It may be necessary to use ALProposerSequentialReference even when
proposing only a single point with this proposer itself if the proposer
is part of a portfolio of proposers.

Creates a sequential-reference proposer.

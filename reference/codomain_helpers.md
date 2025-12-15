# Codomain Helpers for Active Learning

Helper functions for working with codomains that include the "learn" tag
for active learning, in addition to bbotk's standard
"minimize"/"maximize" tags.

## Details

As of bbotk \>= 0.9.0, the
[bbotk::Codomain](https://bbotk.mlr-org.com/reference/Codomain.html)
class natively supports "learn" tags. These helper functions provide
convenient accessors for filtering targets by their purpose
(optimization vs learning).

## Tag Semantics

- **"minimize"**: Lower values are better; we seek x that minimizes f(x)

- **"maximize"**: Higher values are better; we seek x that maximizes
  f(x)

- **"learn"**: We want to learn f(x) accurately across the domain; no
  optimization direction

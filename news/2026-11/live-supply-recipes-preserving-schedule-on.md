# Stages over preserving and `schedule-on` supplies, and `unique(:expires)`, are per-tap recipes

ADR-18443 slice 2c. A combinator over a `Supplier::Preserving` supply or a
`schedule-on` supply, and `unique(:expires)` over any live supply, used to
keep the eager stage registered when the method ran. They are now per-tap
recipes like the other live stages: an untapped stage runs nothing, the
first tap of a stage over a preserving supply gets the backlog through the
stage at tap time (rakudo's order), closing the tap releases the source, and
every `unique(:expires)` tap keeps its own expiry clock. A `react` over such
a recipe keeps the preserving backlog for its `whenever`.

The seeded-value sources (`Supply.merge` of a live and a cold supply) keep
their eager stage until `merge` itself becomes a recipe.

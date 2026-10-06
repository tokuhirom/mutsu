use v6;

# A package-less module file: its top-level enum is declared in GLOBAL, so its
# bare keys are visible to whoever loads it, an EVAL'd snippet included.
enum EvalFixtureSettings (:EvalFixtureSA(1), :EvalFixtureSB(2));

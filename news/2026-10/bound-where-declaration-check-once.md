# Bound declarations check where once

A scalar declaration using `:=` now passes the result of its binding type check to the store. The store no longer evaluates a side-effecting `where` predicate a second time. This also applies when the bound scalar has an explicit type constraint.

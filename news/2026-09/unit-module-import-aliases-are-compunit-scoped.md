---
title: unit module imports are scoped to their compilation unit
---

Imported variables and types from a `unit module` no longer leak into the
scope that loaded the module. The module's routines and nested blocks continue
to resolve those imports, while a direct `use` still installs its exports in the
caller.

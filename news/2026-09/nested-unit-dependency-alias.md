# Nested unit modules retain dependency aliases

Package-less aliases imported by a dependency remain visible while a nested
unit module is loading. This preserves helper calls in modules that load the
same dependency chain more than once.

Pinned by `t/modules/import-export/nested-unit-dependency-export.t`.

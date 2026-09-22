---
title: "Restore P5substr lvalue writeback"
category: bugfix
---

Imported `substr` routines that return a `Proxy` can now update their original
string through lvalue assignment. The native `substr` fallback no longer
overrides an imported routine, and `substr-rw` preserves captured scalar
containers and reports out-of-range offsets instead of appending.

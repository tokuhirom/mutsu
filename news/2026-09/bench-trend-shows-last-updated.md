# The bench-trend dashboard shows when its data was last updated

`bench-trend.html` now carries an `updated YYYY-MM-DD HH:MM UTC` stamp in its header, next to the
commit range. It is the newest row date across both the wall-clock and the deterministic series
from `bench-data`, not the last commit's first-seen date (a re-run of an already-recorded commit
would leave that behind). The stamp is a `<time datetime=...>` element, and hovering it shows the
same instant in the viewer's local time. A deploy that has fallen behind `bench-data` is now
visible at a glance instead of requiring a look at the branch.

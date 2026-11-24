# A class split by the BEGIN prologue keeps one routine scope

When a class body declares a class inside one of its routines, the BEGIN
prologue splits the body in two: the declarations move ahead, and the run-time
statements follow later as a separate part. The undeclared-routine check walked
that run-time part as a scope of its own, so a call there to a routine the body
declares (`sub isleap` called from an anonymous sub assigned in the body) was
reported as "Undeclared routine". Both parts are one lexical scope again for
the check.

This had been hidden: before the check stopped bailing on any `use` below the
top level (#18994), Timezones::ZoneInfo's nested `use` skipped the check for
the whole module. Once it no longer did, Timezones::ZoneInfo and the three
distributions that depend on it (DateTime::Timezones, Timezone::Simple and
DateTime::React) went from green to `blocked_load`. All four load and pass
their suites again (#19137).

# Ecosystem lock board rotated from #15044 to #16103

The weekly triage found the lock board #15044 at 442 comments, well past the ~250 rotation
threshold. It was rotated to #16103 (v8) on 2026-11-03.

No locks were carried. All 18 locks still open on the old board were stale: older than 65 hours
with no branch on `origin`. Most had been released under the fix branch's name instead of the
`eco/<dist>` branch they were locked with, so the log script still counted them live. A stale
`Unlocking:` for each was posted on #15044 first: MVC::Keayl, IRC::Log::Colabti, P5chr, P5substr,
P5seek, P5reset, Selkie::UI, sourcery, IP::Addr, Native::FindVersion, Shell::DSL, PDF,
Cro::WebSocket, Data::Dump::Tree, Unix::errno, Event::Emitter, head-skip-tail, String::Splice.

# Methods with `is raw` parameters no longer build a deferral frame on every call

In a program that mentions `callsame`, `nextsame`, `callwith` or `nextwith`
anywhere, every method call keeps a deferral frame for those builtins. Since
#10108 most frames are only recorded at call time and built when a deferral
builtin actually reads one. A method whose candidates have an `is raw` or
sigilless parameter was the exception: its frame was always built at call
time. Building one costs about 9k instructions. It expands the deferral
candidates, matches each candidate's signature against the arguments and
fingerprints the winner. Nearly every frame was then popped without being read.

The early build existed because matching an `is rw` parameter reads the call
site: the argument has to be a writable variable, which the call's source names
and the source's readonly mark decide. An `is raw` or sigilless parameter
accepts any argument, value or container, so its match depends only on the
argument values the recorded frame keeps. The early build is now limited to
`is rw`.

AttrX::Mooish's attribute methods (`build-attr`, `attr-var`, `store-with-cb`,
`invoke-opt`, ...) all take `Mu $instance is raw`, so they built 15 frames per
lazy-attribute round. On the t/090-concurrent probe (callgrind, R=120→360) a
round went from 5,030,262 to 4,904,219 instructions (-2.5%). CPU for 1,500
rounds fell from 4.34 s to 4.09 s (mean of 5 interleaved runs).

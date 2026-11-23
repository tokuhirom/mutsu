# A byte-order mark before `use v6.x;` no longer makes it "too late"

#18872 accepts a language-version pragma only as a unit's first statement,
which is what rakudo does. Its check read a file's leading UTF-8 byte-order
mark as something before the pragma, so `\u{FEFF}use v6.d;` died with "Too
late to switch language version". That blocked every module of HTTP::Core and
turned Math::Constants' `t/01-basic.t` red.

A leading BOM is now skipped before both checks: the first-statement test and
the spot where a `-M` prologue is inserted.

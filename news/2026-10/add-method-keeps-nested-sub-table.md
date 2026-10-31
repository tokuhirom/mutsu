# `^add_method` keeps the table of subs nested in the added routine

A routine handed to `^add_method` (the way `OO::Monitors` wraps every method of
a `monitor`) was registered with its bytecode but without the table of the subs
nested in its body. The table is now carried along, so a missing table means the
body nests no subs everywhere, and the callable `.^find_method` returns for any
such method (a role method, a `LoggingHOW.redirect` closure) runs its compiled
body instead of the tree-walk carrier. The earlier restriction that kept every
table-less method on the carrier is gone. A slice of #14572.

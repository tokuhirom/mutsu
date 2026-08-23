# IO path permission smart-match uses effective access

`IO::Path ~~ :r`, `:w`, and `:x` now use the same effective-access check as
the corresponding methods, rather than treating any permission bit set for
any user as sufficient.

use v6;
use lib 't/lib';
use Test;

plan 4;

# A statement-form `require Mod <&name>` of a routine the module exports only
# as `multi` candidates makes the whole family callable (#15149,
# Crypt::AnyPasswordHash's `crypt`).
require RequireMultiOnlyExport <&crypt &plain-one>;

is plain-one(), 'plain', 'a plain sub export is callable';
is crypt("a", "b"), 'two', 'two-argument multi candidate';
is crypt("a"), 'one', 'one-argument multi candidate';
ok &crypt.defined, '&crypt is defined';

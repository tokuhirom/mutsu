use v6;
use Test;
use nqp;

# `nqp::gethostname` (a 0-arg nqp op used with no parens) was unimplemented, so
# Sys::Hostname's `hostname` sub died with "Could not find symbol '&gethostname'
# in 'nqp'". T-030 (Sys::Hostname).

plan 4;

my $h = nqp::gethostname;
isa-ok $h, Str, 'nqp::gethostname returns a Str';
ok $h.chars, 'nqp::gethostname is non-empty';

# The exact expression Sys::Hostname uses: strip whitespace and NULs.
my $clean = nqp::gethostname.subst(/ \s | \0 /, '', :g);
isa-ok $clean, Str, 'method-chained on the nqp::gethostname result works';
ok $clean.chars, 'cleaned hostname still has characters';

use Test;

plan 6;

multi baz(Int $i) { 1 }
multi baz(Int $i;; Str :$s) { 2 }
multi foo(;; Numeric $a) { 1 }
multi foo(;; Int $a) { 2 }

try baz(42);
isa-ok $!, X::Multi::Ambiguous, 'baz(42) is ambiguous';
my @lines = $!.message.lines;
is @lines[0], "Ambiguous call to 'baz(Int)'; these signatures all match:", 'header';
like @lines[1], /^ '  (Int $i) from ' .* ' line ' \d+ $/, 'first candidate: signature, file, line';
like @lines[2], /^ '  (Int $i;; Str :$s) from ' .* ' line ' \d+ $/, 'second candidate keeps the ;;';

try foo(42);
@lines = $!.message.lines;
is @lines[1].subst(/' from ' .*/, ''), '  (;; Numeric $a)', 'leading ;; candidate';
is @lines[2].subst(/' from ' .*/, ''), '  (;; Int $a)', 'second ;; candidate';

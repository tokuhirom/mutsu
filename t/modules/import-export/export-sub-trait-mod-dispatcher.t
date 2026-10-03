use Test;

# A `sub EXPORT` that returns `'&trait_mod:<is>' => $candidate.dispatcher`
# (upstream NativeCall's `is native` installation) hands the importer a
# dispatcher whose `multi` is lexical to EXPORT. The importer sees the
# candidate, and a routine trait it declares is applied through it (#11530).

plan 6;

use lib 't/lib';
use ExportSubTraitDispatcher;

ok &trait_mod:<is>.candidates.map(*.signature.raku).grep(/xdtrait/),
    'the imported dispatcher lists the candidate';

sub declared() is xdtrait { }
my $anon = sub () is xdtrait { };

ok 'declared' ∈ @ExportSubTraitDispatcher::APPLIED,
    'the trait is applied to a declared sub';
is @ExportSubTraitDispatcher::APPLIED.elems, 2,
    'and to an anonymous sub';

trait_mod:<is>(sub direct() { }, :xdtrait);
ok 'direct' ∈ @ExportSubTraitDispatcher::APPLIED,
    'a direct call of the imported trait_mod:<is> reaches the candidate';

sub plain() is export { }
pass 'a core trait still applies alongside the imported dispatcher';

# Re-exporting an operator's dispatcher the same way used to recurse until the
# stack overflowed.
{
    my $d = do { my $t := multi infix:<xdop>($a, $b) { "xdop" }; $t.dispatcher };
    is $d(1, 2), 'xdop', 'a dispatcher outliving its multi\'s scope still dispatches';
}

use Test;
use lib 't/lib';
use IndirectTermHook;
use IndirectTermRawHook <raw_lower RawUpper>;

plan 18;

is aaa, 7, 'lowercase Proxy export resolves as a bare term';
is Bbb, 7, 'uppercase Proxy export resolves as a bare term';
is UPPER_X, 2, 'uppercase plain export resolves as a bare term';
is lower_x, 1, 'lowercase plain export resolves as a bare term';
is ABC, 5, 'second uppercase plain export resolves as a bare term';

is ::('aaa'), 7, 'indirect lookup finds a lowercase Proxy export';
is ::('Bbb'), 7, 'indirect lookup finds an uppercase Proxy export';
is ::('UPPER_X'), 2, 'indirect lookup finds an uppercase plain export';
is ::('lower_x'), 1, 'indirect lookup finds a lowercase plain export';
is ::('ABC'), 5, 'indirect lookup finds another uppercase plain export';

is raw_lower, 7, 'a dynamic raw Proxy export resolves as a bare lowercase term';
is RawUpper, 7, 'a dynamic raw Proxy export resolves as a bare uppercase term';
is ::('raw_lower'), 7, 'indirect lookup finds a dynamic raw lowercase Proxy export';
is ::('RawUpper'), 7, 'indirect lookup finds a dynamic raw uppercase Proxy export';

sub outside-scope() { ::('scoped') }
is outside-scope().^name, 'Failure', 'a term is absent before its block import';
{
    use IndirectTermRawHook <scoped>;
    is ::('scoped'), 7, 'indirect lookup sees a block-scoped hook import';
    is outside-scope().^name, 'Failure', 'a caller import does not enter an outside routine';
}
is ::('scoped').^name, 'Failure', 'the term is hidden after the import block';

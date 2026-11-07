use Test;

plan 5;

my $c = EVAL "#| lead\nsub bar \{ \}; &bar";
is $c.WHY.^name, 'Pod::Block::Declarator', 'a named sub returned by EVAL keeps its WHY';
is $c.WHY.leading, 'lead', '... with the leading doc';
ok $c.WHY === $c.WHY, '... and every .WHY is the same object';

my $d = EVAL "#| lead2\nanon sub \{ \}";
is $d.WHY.leading, 'lead2', 'an anonymous sub keeps its WHY';

sub bar { }
is &bar.WHY.defined, False, 'a same-named sub of the caller is not documented by the EVAL';

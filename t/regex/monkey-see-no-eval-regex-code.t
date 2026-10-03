use Test;

# A string interpolated into a regex assertion may carry a code block only
# under `use MONKEY-SEE-NO-EVAL`; `no MONKEY-SEE-NO-EVAL` lexically turns it
# off again. (A module's top-level pragma also covers its routines when
# another compunit calls them -- Router::Right builds its route regexes so.)

plan 3;

my $code = '{ 1 } a';

{
    use MONKEY-SEE-NO-EVAL;
    ok so('a' ~~ /<$code>/), 'allowed under use MONKEY-SEE-NO-EVAL';
    my token T { ^ <{$code}> }
    ok so('a' ~~ &T), 'a token interpolating a code-bearing string';
    {
        no MONKEY-SEE-NO-EVAL;
        throws-like { 'a' ~~ /<$code>/ }, X::SecurityPolicy, 'no MONKEY-SEE-NO-EVAL turns it off again';
    }
}

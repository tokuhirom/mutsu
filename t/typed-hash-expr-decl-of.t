use Test;

plan 10;

# A typed-hash declaration used in EXPRESSION position (e.g. passed as a sub
# argument, `gen my Int %i`) must keep its value-type metadata, so `.of` and the
# missing-key default reflect the value type. Previously the expression-position
# VarDecl tagged typed *arrays* but not typed *hashes*, leaving `%i.of` = Mu and
# a missing key returning `Any` instead of the value-type default.

sub gen(\h) { h<a> = 1; h<b> = 2 }

# my $x = my Int %i  (assignment context)
{
    my $x = my Int %i;
    is %i.of.^name, 'Int', 'my $x = my Int %i keeps .of';
}

# gen my Int %i  (sub-argument context, raw \h param)
{
    gen my Int %i;
    is %i.of.^name, 'Int', 'gen my Int %i keeps .of';
    is %i<B>.^name, 'Int', 'missing key returns the value-type default';
    is %i<B>:!v.^name, 'Int', 'missing key :!v returns the value-type default';
    is-deeply (%i<B>:!p), (B => Int), 'missing key :!p pairs key with the value-type default';
    is %i<a>, 1, 'existing key value still reads correctly';
    is-deeply (%i<a>:k), 'a', 'existing key :k still works';
}

# Statement-position declaration is unchanged (control).
{
    my Int %j;
    %j<x> = 5;
    is %j.of.^name, 'Int', 'statement-position my Int %j keeps .of';
    is %j<Q>.^name, 'Int', 'statement-position missing key default';
}

# An object hash (`%c{Cool}`) in expression position keeps working string-key
# subscripts — re-tagging its key type would break `%c<b>:k`.
{
    gen my %c{Cool};
    is-deeply (%c<b>:k), 'b', 'object hash string-key subscript survives expr decl';
}

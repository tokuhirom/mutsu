use Test;

plan 24;

my $source = 1;
{
    my @a;
    throws-like { @a[0][1] := $source }, X::Multi::NoMatch,
        'a positional bind cannot create a missing row';
    is @a.elems, 0, 'the failed bind leaves the array empty';
}
{
    my %h;
    throws-like { %h<a>[0] := $source }, X::Multi::NoMatch,
        'a hash entry is not vivified for BIND-POS';
    is %h.elems, 0, 'the failed bind leaves the hash empty';
}
{
    my $a;
    throws-like { $a[0][1] := $source }, X::Multi::NoMatch,
        'an undefined scalar does not become a positional bind receiver';
    nok $a.defined, 'the scalar remains undefined';
}
{
    my @a;
    throws-like { @a[0][1][2] := 42 }, X::Multi::NoMatch,
        'literal binds also refuse a missing deep row';
    is @a.elems, 0, 'no intermediate row is committed';
}
{
    my @a = [],;
    throws-like { @a[0][1][2] := $source }, X::Multi::NoMatch,
        'an existing first row does not permit a missing next row';
    is @a[0].elems, 0, 'the existing row stays empty';
}
{
    my %h;
    throws-like { %h<a><b>[0] := $source }, X::Multi::NoMatch,
        'associative prefixes do not commit a missing positional receiver';
    is %h.elems, 0, 'the entire missing associative prefix remains absent';
}
{
    my Array @a;
    throws-like { @a[0][1] := $source }, X::Multi::NoMatch,
        'an Array type object is not a concrete row';
    is @a.elems, 0, 'typed row failure does not grow the array';
    throws-like { @a[0][1][2] := $source }, X::Multi::NoMatch,
        'a typed missing prefix also refuses a deeper positional bind';
    is @a.elems, 0, 'the typed prefix stays absent at greater depth';
}
{
    my @a;
    @a[0][1] = 7;
    is @a[0][1], 7, 'ordinary assignment still vivifies positional rows';
    @a[0][1] := $source;
    $source = 2;
    is @a[0][1], 2, 'an existing positional row still binds to the source';
}
{
    my %h;
    %h<a>[0]<b> := 5;
    is %h<a>[0]<b>, 5, 'an associative leaf still permits vivification';
}
{
    my $calls = 0;
    my $seen;
    class CustomRow {
        method BIND-POS($index, \value) {
            $calls++;
            value = 9;
            $seen = $source;
            value
        }
    }
    my CustomRow @a;
    lives-ok { @a[0][1] := $source }, 'a type object may supply its own BIND-POS';
    is $calls, 1, 'the custom BIND-POS runs exactly once';
    is @a.elems, 0, 'the custom binder controls whether a row is created';
    is $source, 9, 'the custom binder receives the original source container';
    is $seen, 9, 'the source alias is visible while the binder runs';
}

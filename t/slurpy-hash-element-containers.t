use Test;

plan 11;

# A slurpy named hash keeps values arriving from the call raw.  Values written
# into that same hash are real Hash element stores and acquire their own
# Scalar container.  The distinction is per entry, not a property of the
# whole Hash.
sub named(*%h) { %h.raku }

is named(:a, :!b, :c(1)), '{:a, :!b, :c(1)}',
    'adverbial named arguments keep raw Boolean values';
is named(a => True, b => False, c => 1), '{:a, :!b, :c(1)}',
    'explicit Pair arguments keep raw Boolean values';

sub inspect(*%h) {
    %h<new> = True;
    %h<a>.VAR.^name ~ '/' ~ %h<new>.VAR.^name ~ '/' ~ %h.raku
}
is inspect(:a), 'Bool/Scalar/{:a, :new(Bool::True)}',
    'an assignment adds a Scalar container without changing the raw entry';

sub replace(*%h) {
    %h<new> = False;
    %h.raku ~ '/' ~ %h<new>.VAR.^name
}
is replace(:a), '{:a, :new(Bool::False)}/Scalar',
    'inserting a new value creates a Scalar container';

sub inspect-views(*%h) {
    %h.raku ~ '|' ~ %h.gist ~ '|' ~ %h.pairs.sort.raku ~ '|'
        ~ %h.kv.map({ .^name }).sort.join(',') ~ '|'
        ~ %h.values.map({ .^name }).sort.join(',') ~ '|' ~ %h.Map.raku
}
is inspect-views(:a, :!b, :c(1)),
    '{:a, :!b, :c(1)}|{a => True, b => False, c => 1}|(:a, :!b, :c(1)).Seq|'
        ~ 'Bool,Bool,Int,Str,Str,Str|Bool,Bool,Int|Map.new((:a,:!b,:c(1)))',
    'raw slurpy values stay raw through views and Map coercion';

{
    my %h = a => True, b => False, c => 1;
    is %h.raku, '{:a(Bool::True), :b(Bool::False), :c(1)}',
        'ordinary Hash values retain the long Boolean form';
    is %h.pairs.sort.raku, '(:a(Bool::True), :b(Bool::False), :c(1)).Seq',
        'ordinary Hash pairs retain their Scalar Boolean values';
    is %h.Map.raku, 'Map.new((:a,:!b,:c(1)))',
        'Map coercion removes ordinary Hash element containers';
}

sub copy-and-bind(*%h) {
    my %copy = %h;
    my %bound := %h;
    %copy<a>.VAR.^name ~ '/' ~ %bound<a>.VAR.^name ~ '/'
        ~ %copy.raku ~ '/' ~ %bound.raku
}
is copy-and-bind(:a, :b(1)),
    'Scalar/Bool/{:a(Bool::True), :b(1)}/{:a, :b(1)}',
    'assignment copy containerizes while binding preserves raw entries';

sub aggregates(*%h) {
    %h<a>.raku ~ '/' ~ %h<b>.raku ~ '/' ~ %h.Map.raku
}
is aggregates(a => (1, 2), b => [3, 4]),
    '(1, 2)/[3, 4]/Map.new((:a((1, 2)),:b([3, 4])))',
    'raw aggregate values stay un-itemized in a slurpy hash and its Map';

sub nil-value(*%h) { %h.raku ~ '/' ~ %h.Map.raku }
is nil-value(:a(Nil)), '{:a(Nil)}/Map.new((:a(Nil)))',
    'Nil remains a raw slurpy value';

done-testing;

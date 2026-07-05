use Test;

plan 10;

# classify/categorize always return `Hash[Any,Mu]` in Raku, and object-hash
# `.raku` renders each pair per its key: a Str identifier key becomes a
# colonpair `:key(value)`, everything else becomes `key => value`.

# --- classify / categorize type object + .raku ---

is (1,2,3,4).classify(* %% 2).WHAT.^name, 'Hash[Any,Mu]',
    'classify with Bool keys is Hash[Any,Mu]';

is <apple banana avocado>.classify(*.substr(0,1)).WHAT.^name, 'Hash[Any,Mu]',
    'classify with Str keys is Hash[Any,Mu]';

is (1,2,3,4).categorize(* %% 2).WHAT.^name, 'Hash[Any,Mu]',
    'categorize is Hash[Any,Mu]';

is (1,2,3,4).classify(* %% 2).raku,
    '(my Any %{Mu} = Bool::False => $[1, 3], Bool::True => $[2, 4])',
    'classify Bool-key .raku uses arrow for non-Str keys';

is <apple banana avocado>.classify(*.substr(0,1)).raku,
    '(my Any %{Mu} = :a($["apple", "avocado"]), :b($["banana"]))',
    'classify Str-key .raku uses colonpairs';

# lookups still work after the metadata tag
{
    my %c = <apple banana avocado>.classify(*.substr(0,1));
    is-deeply %c<a>, ['apple', 'avocado'], 'Str-key classify bucket lookup works';
}

# --- object-hash .raku pair rendering rules ---

my %h{Mu};
%h<a> = 1;
%h{"a b"} = 2;
%h<foo-bar> = 3;
%h{1} = 4;
is %h.raku,
    '(my Any %{Mu} = 1 => 4, :a(1), "a b" => 2, :foo-bar(3))',
    'per-key colonpair vs arrow: Str idents colonpair, others arrow';

my %b{Mu};
%b<a> = True;
is %b.raku, '(my Any %{Mu} = :a(Bool::True))',
    'a Bool value renders in full (:a(Bool::True)), not the :a shorthand';

my Int %i{Int};
%i{5} = 10;
is %i.raku, '(my Int %{Int} = 5 => 10)', 'Int-keyed hash uses arrow';

my Int %s{Str};
%s<x> = 1;
is %s.raku, '(my Int %{Str} = :x(1))', 'Str-keyed hash uses colonpair';

use Test;

# `Str`'s and `Cool`'s text methods take every argument form as rows, and a
# call no receiver-specific answer took reaches the first row of the
# receiver's MRO that binds it (ADR-11276 §9.129).

plan 20;

class CC is Cool { method Str { "cool str" } }
class Plain {}
enum E <alpha beta>;

# Every argument form on a Str.
ok 'abcabc'.contains(/b/), 'contains with a regex';
is 'abcabc'.index('b', 2), 4, 'index with a position';
is 'abcabc'.substr(1..2), 'bc', 'substr with a range';
is 'abcabc'.comb(/b./).join(','), 'bc,bc', 'comb with a regex';
is 'abc def'.wordcase(:filter(&uc)), 'ABC DEF', 'wordcase with :filter';
is 'abcabc'.trans(('a' => 'z').Seq), 'zbczbc', 'trans with a Seq of pairs';

# Cool receivers: an Int, an IO::Path, a Range, an enum member.
ok 42.contains(/4/), 'Int.contains with a regex is Cool.contains';
ok 'ab'.IO.contains(/b/), 'IO::Path.contains with a regex';
ok (1..5).contains('3'), 'Range.contains with a Str';
ok (1..5).contains(/3/), 'Range.contains with a regex is Cool.contains';
ok alpha.contains('lp'), 'an enum member reads its key';

# A Cool instance with a Str of its own is read through it.
ok CC.new.contains(/str/), 'Cool instance through its Str: contains';
is CC.new.IO.Str, 'cool str', 'Cool instance through its Str: IO';
isa-ok CC.new.match(/oo/), Match, 'Cool instance: match is Any.match';

# A receiver that is no Cool has none of Cool's methods, as in Rakudo.
throws-like { Plain.new.contains('b') }, X::Method::NotFound, 'Plain.contains';
throws-like { Plain.new.substr(1) }, X::Method::NotFound, 'Plain.substr';
throws-like { (a => 1).IO }, X::Method::NotFound, 'Pair.IO';

# Any.match answers a type object; Str.IO is an IO::Path.
nok Str.match(/b/), 'Str.match on the type object';
isa-ok Int.IO, IO::Path, 'Int.IO';

# No candidate takes no argument.
throws-like { ''.subst }, X::Multi::NoMatch, '.subst with no argument';

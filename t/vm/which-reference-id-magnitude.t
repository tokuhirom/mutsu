use Test;

# A reference object's `.WHICH` carries an address-sized number, as rakudo's
# does (`Array|3168270547864`): consumers such as Data::Dump::Tree tell a real
# object address from a fallback by `/ \d ** 4 /` (#15626).

plan 9;

my @a = 1, 2;
my %h = a => 1;
my class C { }
my $c = C.new;

for :array(@a.WHICH), :hash(%h.WHICH), :instance($c.WHICH), :list((1, 2).WHICH),
    :seq((1, 2).Seq.WHICH), :block({ 1 }.WHICH) -> (:key($kind), :value($which)) {
    like ~$which, / '|' \d ** 10 /, "$kind .WHICH has a 10+ digit id";
}
like ~C.WHICH, / '|U' \d ** 10 /, 'a type object .WHICH has a 10+ digit id';

ok @a.WHICH eq @a.WHICH, 'the id is stable across reads';
ok @a.WHICH ne [1, 2].WHICH, 'distinct arrays still differ';

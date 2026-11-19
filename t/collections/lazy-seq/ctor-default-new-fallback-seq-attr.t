use Test;

# A class with its own `multi method new` whose candidates do not take the
# call's named arguments falls back to the default constructor
# (`Mu.new(*%attrinit)`). A `.map`/`.grep`/gather Seq passed as a named
# argument must fill an `@`/`%` attribute from its elements there, exactly
# as it does for a class without its own `new`.
# From ML::TriesWithFrequencies t/08-overall-from-JavaTrie (its `clone`
# builds `Trie.new(children => self.children.map({ ... }))`, and the role
# declares only positional `new` candidates).

plan 9;

class Plain { has @.a; has %.h }
class Own {
    has @.a;
    has %.h;
    has $.s;
    multi method new(Int $x) { self.bless(a => [$x]) }
}

is-deeply Own.new(a => (1, 2).map({ $_ })).a, [1, 2], '.map Seq fills @ attribute';
is-deeply Own.new(a => (1, 2, 3).grep(* > 1)).a, [2, 3], '.grep Seq fills @ attribute';
is-deeply Own.new(a => gather { take 1; take 2 }).a, [1, 2], 'gather fills @ attribute';
is-deeply Own.new(h => <a b>.map({ $_ => 1 })).h, %(a => 1, b => 1), '.map of Pairs fills % attribute';
is-deeply Own.new(a => (1, 2).map({ $_ }), s => 7).s, 7, 'other named arguments still bind';
is-deeply Own.new(5).a, [5], 'a matching user candidate still runs';
is-deeply Plain.new(a => (1, 2).map({ $_ })).a, Own.new(a => (1, 2).map({ $_ })).a,
    'same as a class without its own new';

role R {
    has R %.children;
    has Str $.key;
    multi method new(Str $key) { self.bless(:$key) }
}
class Node does R {
    method copy() {
        Node.new(key => self.key, children => self.children.map({ .key => .value.copy }))
    }
}
my $tree = Node.new(key => 'root', children => %(x => Node.new(key => 'x')));
my $copy = $tree.copy;
is-deeply $copy.children.keys.sort, ('x',), 'a recursive copy keeps its children';
is $copy.children<x>.key, 'x', 'the copied child is intact';

use Test;

plan 5;

# A `for ... -> %v` / `-> @v` pointy parameter is a per-iteration binding whose
# scope ends with the loop, exactly like `-> $v`. It used to be exempt from the
# loop's save/restore of the parameter name, so re-entering the SAME loop from
# a nested call left the inner frame's last element bound: the outer iteration
# resumed with the inner element. (Template::Nest::Fast's recursive `render`
# then spliced a nested component's offsets into the parent template.)

class Recurse {
    method walk(@vars, $depth --> Str) {
        my Str $acc = '';
        for @vars -> %v {
            my Str $inner = $depth == 0 ?? self.walk([%(n => 'I1')], 1) !! 'x';
            $acc ~= "[{%v<n>}:$inner]";
        }
        return $acc;
    }
}
is Recurse.new.walk([%(n => 'O1'), %(n => 'O2')], 0),
   '[O1:[I1:x]][O2:[I1:x]]',
   'a %-sigil loop parameter survives recursion into the same loop';

class RecurseArray {
    method walk(@vars, $depth --> Str) {
        my Str $acc = '';
        for @vars -> @v {
            my Str $inner = $depth == 0 ?? self.walk([['I1'],], 1) !! 'x';
            $acc ~= "[{@v[0]}:$inner]";
        }
        return $acc;
    }
}
is RecurseArray.new.walk([['O1'], ['O2']], 0),
   '[O1:[I1:x]][O2:[I1:x]]',
   'an @-sigil loop parameter survives recursion into the same loop';

sub walk-sub(@vars, $depth --> Str) {
    my Str $acc = '';
    for @vars -> %v {
        my Str $inner = $depth == 0 ?? walk-sub([%(n => 'I1')], 1) !! 'x';
        $acc ~= "[{%v<n>}:$inner]";
    }
    return $acc;
}
is walk-sub([%(n => 'O1'), %(n => 'O2')], 0),
   '[O1:[I1:x]][O2:[I1:x]]',
   'the same holds for a recursive sub';

# The container binding itself is still shared, not copied: mutating through
# the parameter must reach the source element.
my @aoa = [1, 2], [3, 4];
for @aoa -> @row { @row.push(9) }
is @aoa.raku, '[[1, 2, 9], [3, 4, 9]]', 'an @-sigil loop parameter still aliases its element';

my @aoh = %(n => 1), %(n => 2);
for @aoh -> %row { %row<m> = 5 }
is @aoh.map(*<m>).join(','), '5,5', 'a %-sigil loop parameter still aliases its element';

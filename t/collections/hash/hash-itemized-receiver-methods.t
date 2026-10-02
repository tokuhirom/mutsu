use Test;

plan 30;

# A method call decontainerizes its invocant, so a method on an itemized Hash
# (a `$`-held hash, an element read out of an Array or Hash, `.item`, `$(...)`)
# operates on the hash's Pairs. mutsu keeps the holder's itemization as a flag
# on the value (the same `HashData`), and a method that decomposed its RECEIVER
# through `value_to_list` -- which answers "does this flatten as an ELEMENT of
# another container" -- saw the whole hash as ONE opaque item. `call_method_
# with_values` already decontainerized an itemized Array receiver; its Hash half
# only redirected `.VAR`.

my $s = {a => 1};
my $t = {a => 1, b => 2};

# --- the methods that used to see the hash as one item -------------------------
is $s.tail.raku,            ':a(1)',           '.tail';
is $s.rotor(1).raku,        '((:a(1),),).Seq', '.rotor';
is $t.tail(2).sort.raku,    '(:a(1), :b(2)).Seq', '.tail(2)';
is $s.cache.raku,           '(:a(1),)',        '.cache';
is $s.permutations.raku,    '((:a(1),),).Seq', '.permutations';
is $s.head.raku,            ':a(1)',           '.head (already right)';
is $s.first.raku,           ':a(1)',           '.first (already right)';
is $s.list.raku,            '(:a(1),)',        '.list (already right)';
is $s.pairs.raku,           '(:a(1),).Seq',    '.pairs (already right)';
is $s.kv.raku,              '("a", 1).Seq',    '.kv (already right)';
is $s.keys.raku,            '("a",).Seq',      '.keys (already right)';
is $s.elems,                1,                 '.elems';

# --- every itemized holder ----------------------------------------------------
{
    my %h = a => 1;
    is $(%h).tail.raku, ':a(1)', '$(%h).tail';
    is %h.item.cache.raku, '(:a(1),)', '%h.item.cache';
    my %nested = k => {z => 1};
    is %nested<k>.tail.raku, ':z(1)', '.tail on a Hash element read out of a Hash';
    my @aoh = {q => 1},;
    is @aoh[0].tail.raku, ':q(1)', '.tail on a Hash element read out of an Array';
    sub take($x) { $x.tail.raku }
    is take(%h), ':a(1)', '.tail on a hash bound to a plain `$` parameter';
}

# --- the receiver still shows its itemization to the methods that observe it ---
{
    my $h = {a => 1};
    is $h.raku,           '${:a(1)}', '.raku shows the container';
    is $h.item.raku,      '${:a(1)}', '.item keeps the container';
    is $h.self.raku,      '{:a(1)}',  '.self is the decontainerized hash';
    is $h.VAR.^name,      'Scalar',   '.VAR is the Scalar';
    is $h.gist,           '{a => 1}', '.gist never shows the sigil';
    is ($h,).raku,        '(${:a(1)},)', 'a List holding it renders the container';
    is [$h, $h].elems,    2,          'and it is ONE element of an array literal';
}

# --- the receiver is the SAME hash, so mutators still write through ------------
{
    my $h = {a => 1};
    $h.push((b => 2));
    is $h.raku, '${:a(1), :b(2)}', '.push writes through and keeps the container';
    $h.append((c => 3));
    is $h.raku, '${:a(1), :b(2), :c(3)}', '.append writes through';
    $h<a>:delete;
    is $h.raku, '${:b(2), :c(3)}', ':delete writes through';
    my %src = id => 1;
    my $alias = %src.item;
    $alias.push((x => 9));
    is %src.raku, '{:id(1), :x(9)}', '.push on a `.item` alias reaches the source hash';
}

# --- an itemized Array receiver, for the symmetry this fix restores ------------
{
    my $a = $[1, 2, 3];
    is $a.tail, 3, 'an itemized Array: .tail';
    is $a.cache.raku, '[1, 2, 3]', 'an itemized Array: .cache';
}

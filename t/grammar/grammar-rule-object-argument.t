use Test;

# A rule parameter bound to an object (or a closure) must be *that* value
# inside the rule's code blocks and assertions, not a re-parse of its `.raku`.
# Reduced from DSL::Shared's `entity-name(ResourceAccess $resources, $class)`,
# whose `<?{ $resources.known-name(...) }>` saw a type object once the resource
# object's variable was captured by a sub (it then reads as a shared cell).

plan 10;

class Res {
    has Set %!known{Str} = %();
    method add($class, *@words) { %!known{$class} = Set(@words) }
    multi method known(Str:D $class, Str:D $p) { %!known{$class}{$p}:exists }
    multi method known(Whatever, Str:D $p) { so %!known.values.first({ $_{$p}:exists }) }
}

my Res $res .= new;
$res.add('X', 'ab');

grammar G {
    regex ename(Res $r, $class) { ( \w+ ) <?{ $r.known($class, $0.Str) }> }
    rule TOP($obj, $class) { <ename($obj, $class)> }
}

# Capturing `$res` in a list inside a routine turns it into a shared cell.
sub via-sub(Str:D $spec, $class) { G.parse($spec, rule => 'TOP', args => ($res, $class)).so }

ok G.parse('ab', args => ($res, 'X')), 'object argument reaches a code assertion';
nok G.parse('zz', args => ($res, 'X')), 'the assertion consults the real object';
ok G.parse('ab', args => ($res, Whatever)), 'multi dispatch on the object still works';
ok via-sub('ab', 'X'), 'captured object variable passed as a rule argument';
nok via-sub('zz', 'X'), 'captured object variable keeps its state';

{
    my @seen;
    grammar H {
        regex e($o) { \w+ { @seen.push: $o } }
        token TOP { <e(Res.new)> }
    }
    H.parse('ab');
    isa-ok @seen[0], Res, 'a code block sees the bound object, not a coercion type';
}

{
    my $called = 0;
    grammar K {
        regex e($f) { \w+ <?{ $f() }> }
        token TOP { <e(-> { $called++; True })> }
    }
    ok K.parse('ab'), 'a positional closure argument is callable in an assertion';
    ok $called > 0, 'the closure really ran';
}

{
    # Plain data still round-trips through the pattern text.
    grammar L {
        regex e($n, $s) { \w+ <?{ $n == 3 && $s eq 'q"x' }> }
        token TOP { <e(3, 'q"x')> }
    }
    ok L.parse('ab'), 'literal arguments still reach an assertion';
}

{
    # A container holding an object is opaque too.
    grammar M {
        regex e(@objs) { \w+ <?{ @objs[0].known('X', 'ab') }> }
        rule TOP($list) { <e($list)> }
    }
    ok M.parse('ab', args => ([$res],)), 'an array of objects reaches an assertion';
}

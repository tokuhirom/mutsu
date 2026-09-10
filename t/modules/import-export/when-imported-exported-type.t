use lib 't/lib';
use Test;
use ExportedTypeIndex;

# `when SomeType { … }` needs the parser to know that `SomeType` is a type; an
# undeclared bareword there really does gobble the block in Raku, and mutsu
# diagnoses that. The parse-time type index is built by scanning each `use`d
# module — but a trait on a declarator (`class Cond is export { }`) makes the
# parser wrap it in a bare `Stmt::Block`, and the scan's collector never walked
# into one. So EVERY `is export`ed class in a `use`d module was invisible, and
# `when Cond { }` died at compile time with
# "Function 'Cond' needs parens to avoid gobbling block". Reduced from
# `Template::Jinja2`, whose `Renderer.rakumod` dispatches on its AST node types
# exactly this way and could not be loaded at all.

plan 9;

sub classify($node) {
    given $node {
        when Cond { 'cond' }
        when Loop { 'loop' }
        default   { 'other' }
    }
}

is classify(Cond.new), 'cond', 'when on an imported exported class matches';
is classify(Loop.new), 'loop', 'the second such when matches too';
is classify(42), 'other', 'a non-match still falls through to default';

# The same name works everywhere else a type is expected, and inside a nested
# package block (which is how the module itself is written).
module Consumer {
    our sub check($n) {
        given $n {
            when Cond { 'nested-cond' }
            default   { 'nested-other' }
        }
    }
}
is Consumer::check(Cond.new), 'nested-cond', 'when works inside a module block';
is Consumer::check(1), 'nested-other', 'and still falls through there';

# An exported role, enum and constant come through the same scan.
ok Cond.new ~~ Node, 'an exported role is composed';
is classify(Cond.new), 'cond', 'the type index survives later statements';
my $c = ExpColour::Green;
is $c.key, 'Green', 'an exported enum value is still usable';
is EXP_MARK, 'mark', 'an exported constant is still usable';

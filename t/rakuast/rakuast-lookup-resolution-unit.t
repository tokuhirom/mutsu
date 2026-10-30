use Test;

plan 6;

# Lookups of routines and packages the unit declares,
# carry their resolution (#15610 slice 2), as in rakudo's CHECK phase. Sourced
# from `static::CLI`'s analysis. (Imported routines are not resolved yet.) Read from a CHECK-time closure.
my %seen;
BEGIN {
    my $cu = $*CU;
    $cu.add-check-code({
        my sub walk($node) {
            if $node ~~ RakuAST::Call::Name && $node.name.canonicalize eq 'foo' {
                %seen<foo> //= $node;
            }
            if $node ~~ RakuAST::Type::Simple && $node.name.canonicalize eq 'Bar' {
                %seen<bar> //= $node;
            }
            $node.visit-children(&walk);
        }
        walk($cu.statement-list);
    });
}
foo(1) if False;
sub foo($a) { $a }
class Bar { }
Bar.new if False;

ok %seen<foo>.is-resolved, 'a call of a unit sub is resolved, even before the declaration';
ok %seen<foo>.resolution ~~ RakuAST::Sub, 'to the sub';
is %seen<foo>.resolution.name.canonicalize, 'foo', 'which is named like the call';
ok %seen<bar>.is-resolved, 'a unit class is resolved';
ok %seen<bar>.resolution ~~ RakuAST::Declaration::LexicalPackage, 'to a lexical package';
is %seen<bar>.resolution.compile-time-value.^name, 'Bar', 'whose compile-time-value is the type';

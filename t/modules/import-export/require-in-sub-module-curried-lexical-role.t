use lib 't/lib';
use Test;

# A module is loaded by a `require` inside a sub, and a method of its own
# parameterizes a class whose `^parameterize` curries a lexical role by its
# bare name. The bare name has to find the role itself, not one of the
# specializations (`Typed[Str]`) the earlier currying registered beside it.
#
# Every expectation was verified against Rakudo.

plan 6;

sub load-maker() {
    require ::('CurriedLexicalRoleHost');
    ::('CurriedLexicalRoleHost::Maker')
}

my $maker = load-maker();
my $str = $maker.make-str;
is $str.^name, 'CurriedLexicalRoleHost::Box[Str]', 'the first specialization is named';
my $int = $maker.make(Int);
is $int.^name, 'CurriedLexicalRoleHost::Box[Int]', 'a second one beside it';
my $str2 = $maker.make(Str);
is $str2.^name, 'CurriedLexicalRoleHost::Box[Str]', 'the first again, now that others exist';
ok $str2 === $str, 'and it is the same type object';
isa-ok $int.new, $int, 'an instance of a specialization';
is $int.new.of-type, Int, 'the curried role sees its argument';

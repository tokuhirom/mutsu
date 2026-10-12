use Test;

# Grammar action methods are resolved and called as compiled user methods
# (`call_grammar_action_method`), not through the general by-value method
# funnel. The answer must be the one the funnel gave: the user's method wins
# even when its name is one a native interceptor recognizes, and inherited,
# role-supplied, multi, proto-variant and type-object actions all dispatch.

plan 9;

grammar Names {
    token TOP  { <sum> ',' <emit> ',' <hash> ',' <self> }
    token sum  { \d+ }
    token emit { \w+ }
    token hash { \w+ }
    token self { \w+ }
}
class NameActions {
    method sum($/)  { make "sum:" ~ $/ }
    method emit($/) { make "emit:" ~ $/ }
    method hash($/) { make "hash:" ~ $/ }
    method self($/) { make "self:" ~ $/ }
    method TOP($/)  { make ($<sum>, $<emit>, $<hash>, $<self>)».made.join('|') }
}
is Names.parse('12,ab,cd,ef', :actions(NameActions)).made,
    'sum:12|emit:ab|hash:cd|self:ef',
    'action methods named like native interceptors run as user methods';

grammar Words {
    token TOP  { <word>+ % ' ' }
    token word { \w+ }
}
class BaseActions { method word($/) { make $/.Str.uc } }
class DerivedActions is BaseActions {
    method TOP($/) { make $<word>».made.join('-') }
}
is Words.parse('a bc', :actions(DerivedActions)).made, 'A-BC',
    'an inherited action method dispatches';

role WordRole { method word($/) { make $/.Str.flip } }
class RoleActions does WordRole {
    method TOP($/) { make $<word>».made.join('-') }
}
is Words.parse('ab cd', :actions(RoleActions)).made, 'ba-dc',
    'a role-supplied action method dispatches';

class TypeObjectActions {
    method word($/) { make $/.chars }
    method TOP($/)  { make [+] $<word>».made }
}
is Words.parse('abc de', :actions(TypeObjectActions)).made, 5,
    'a type-object actions class dispatches';

class Counter {
    has $.count = 0;
    method word($/) { $!count++; make +$!count }
    method TOP($/)  { make $<word>».made.join(',') }
}
my $counter = Counter.new;
is Words.parse('x y z', :actions($counter)).made, '1,2,3',
    'an instance action method sees and updates its attributes';
is $counter.count, 3, 'the attribute update is visible after the parse';

grammar Proto {
    token TOP { <item>+ }
    proto token item {*}
    token item:sym<num> { \d }
    token item:sym<let> { <[a..z]> }
}
class ProtoActions {
    method item:sym<num>($/) { make 'N' }
    method item:sym<let>($/) { make 'L' }
    method TOP($/) { make $<item>».made.join }
}
is Proto.parse('1a2', :actions(ProtoActions)).made, 'NLN',
    'proto variant action methods dispatch by variant';

class MultiActions {
    multi method word($/ where *.chars > 1) { make 'long' }
    multi method word($/) { make 'short' }
    method TOP($/) { make $<word>».made.join(' ') }
}
is Words.parse('a bc', :actions(MultiActions)).made, 'short long',
    'a multi action method picks its candidate';

class Failing {
    method word($/) { die "boom in action" }
}
throws-like { Words.parse('a', :actions(Failing)) }, Exception,
    message => /'boom in action'/,
    'an exception from an action method propagates out of .parse';

use Test;

# A parametric role's type parameter is bound while an instance of a class
# composing it is constructed. The class's own TWEAK frame saw it, and the
# frame's exit merged it back over a caller variable of the same name that
# had been passed as a named argument (`:$qos`), so the next `.new` got the
# role parameter's default instead of the value (#16987).
plan 6;

enum Qos (:A(0), :B(1));
role Pk[Qos $qos = A] { }
class P does Pk[] {
    has Qos:D $.qos = B;
    has Bool:D $.dup = False;
    has Str:D $.topic is required;
    submethod TWEAK(:$!qos = A) {
        die 'x' unless $!topic ~~ Str;
    }
}

my $qos   = B;
my $topic = 'a/b';
is P.new(:$topic, :$qos).qos, B, 'first construction keeps the passed value';
is $qos, B, 'the caller variable is untouched';
is P.new(:$topic, :$qos, :dup).qos, B, 'a second construction keeps it too';
is $qos, B, 'and the caller variable is still untouched';
is P.new(:$topic, :$qos).qos, B, 'a third one as well';
is P.new(:$topic).qos, A, 'the TWEAK default applies when nothing is passed';

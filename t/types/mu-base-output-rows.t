use Test;

# Mu.say, Mu.print, Mu.put and Mu.note are the deferral base of a user override
# (ADR-11276 section 9.57).

plan 5;

sub capture-out(&code) {
    my $out = '';
    {
        my $*OUT = class { method print(*@a) { $out ~= @a.join; True }; method say(*@a) { $out ~= @a.join ~ "\n"; True } }.new;
        code();
    }
    $out
}

class G { method gist { "custom-gist" }; method say { callsame } }
class T { method Str { "custom-str" }; method print { callsame }; method put { callsame } }
class S { has $.x = 1; method say { callsame } }

is capture-out({ G.new.say }), "custom-gist\n", 'Mu.say writes the receiver\'s gist';
is capture-out({ S.new.say }), "S.new(x => 1)\n", '... the default gist of a plain object';
is capture-out({ T.new.print }), "custom-str", 'Mu.print writes the receiver\'s Str';
is capture-out({ T.new.put }), "custom-str\n", 'Mu.put adds the newline';

my $err = '';
{
    my $*ERR = class { method print(*@a) { $err ~= @a.join; True } }.new;
    class N { method gist { "noted" }; method note { callsame } }.new.note;
}
is $err, "noted\n", 'Mu.note writes the gist and a newline to $*ERR';

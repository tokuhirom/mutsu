use Test;

# `Mu.say` / `.print` / `.put` are `say(self)` and friends: their value is what
# `$*OUT`'s own `print` answered, as for the `say` routine. A REPL sandbox's
# `$*OUT` (Jupyter::Kernel's `Std`) returns a marker it later tests for to
# hide a line's output (#17087). Identical under rakudo.

plan 7;

class Capture {
    has $.text = '';
    method print(*@a) { $!text ~= @a.join; 42 }
    method say(*@a) { self.print(@a.map: * ~ "\n") }
    method flush { }
}

my $out = Capture.new;
{
    my $*OUT = $out;
    is 1.say, 42, '.say answers $*OUT.print';
    is 'a'.print, 42, '.print answers $*OUT.print';
    is 2.put, 42, '.put answers $*OUT.print';
    is [1, 2].say, 42, 'a list receiver too';
    is (say 3), 42, 'as the say routine does';
    is ($( .say for 1..2 )).List, (42, 42), 'each .say of a loop';
}
is $out.text, "1\na2\n[1 2]\n3\n1\n2\n", 'everything went to the handle';

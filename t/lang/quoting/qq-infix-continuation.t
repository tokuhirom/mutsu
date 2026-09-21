use Test;

# Pod::To::Man 1.2.1 uses this multiline concatenation after a qq literal.
plan 1;

my sub render(Str:D $body) {
    qq{\n.RS 4m\n.EX\n}
        ~ $body
        ~ "\n.EE\n.RE"
}

is render("body"), "\n.RS 4m\n.EX\nbody\n.EE\n.RE", 'qq literal continues infix concatenation across a newline';

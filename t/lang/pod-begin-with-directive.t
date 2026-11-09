use Test;

# A Pod block named after a directive (`=begin finish`, `=for end`) is a compile
# error, X::Syntax::Pod::BeginWithDirective.

my @cases = <begin end for config alias finish>;
plan 2 * @cases + 2;

for @cases -> $word {
    try EVAL "=begin $word\nx\n=end $word\n";
    is $!.^name, 'X::Syntax::Pod::BeginWithDirective', "=begin $word";
    try EVAL "=for $word\nx\n\n";
    is $!.message, "=for may not be followed by directive '$word'", "=for $word message";
}

lives-ok { EVAL "=begin pod\nx\n=end pod\n" }, 'an ordinary block name is fine';
lives-ok { EVAL "=begin END\nx\n=end END\n" }, 'END is fine';

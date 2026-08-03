use Test;

plan 16;

# A typed exception must carry the attributes rakudo declares for it, not just
# a message that happens to name the class: `throws-like` matches on them.

# X::Obsolete: `.old` / `.replacement` on every obsolete-syntax rejection.
throws-like 'qr/a/', X::Obsolete,
    old => 'qr for regex quoting', replacement => 'rx//', 'qr// is obsolete';
throws-like 'y/a/b/', X::Obsolete,
    old => 'y///', replacement => 'tr///', 'y/// is obsolete';
throws-like 'my $a; my $b; $a . $b', X::Obsolete,
    old => '. to concatenate strings', replacement => '~', '. concatenation is obsolete';
throws-like 'foreach 1, 2 { }', X::Obsolete,
    old => "'foreach'", replacement => "'for'", 'foreach is obsolete';
throws-like 'undef', X::Obsolete,
    old => 'undef as a value', 'undef is obsolete';
throws-like 'new Foo()', X::Obsolete,
    old => 'C++ constructor syntax', replacement => 'method call syntax',
    'indirect construction is obsolete';
throws-like 'rand()', X::Obsolete,
    old => 'rand()', replacement => 'rand', 'rand() is obsolete';
throws-like '<>', X::Obsolete, old => '<>', 'the diamond is obsolete';
throws-like 'my $a; $a =~ /x/', X::Obsolete,
    old => '=~ to do pattern matching', replacement => '~~', '=~ is obsolete';

# X::Syntax::Variable::MissingInitializer: `.type`, `.what`, and `.implicit`
# when the `:D` came from a pragma rather than the source.
throws-like 'my Int:D $a', X::Syntax::Variable::MissingInitializer,
    type => 'Int:D', what => 'variable', ':D declaration needs an initializer';
throws-like 'use variables :D; my Int $a', X::Syntax::Variable::MissingInitializer,
    type => 'Int:D', implicit => ':D by pragma',
    'a pragma-implied :D says so';

# X::Syntax::WithoutElse: `.keyword`.
throws-like 'without 1 {} else {}', X::Syntax::WithoutElse,
    keyword => 'else', 'without/else names the keyword';
throws-like 'without 1 {} orwith 1 {}', X::Syntax::WithoutElse,
    keyword => 'orwith', 'without/orwith names the keyword';

# X::Comp::Trait::Scope: the trait, what carries it, and the scopes that work.
throws-like 'module H { my $x is export = 42 }', X::Comp::Trait::Scope,
    type => 'is', subtype => 'export', declaring => 'variable', scope => 'my',
    'is export on a my-scoped variable';

# X::Adverb: `.unexpected` is a list, `.what` the routine, `.source` the invocant.
{
    my @list = 1, 2, 3;
    throws-like { @list.grep(Mu, :asdfblargs) }, X::Adverb,
        unexpected => *.contains('asdfblargs'), what => 'grep',
        'an unexpected adverb names itself';
    throws-like { @list.grep(Mu, :!v) }, X::Adverb,
        what => 'grep', 'a negated :v is unexpected too';
}

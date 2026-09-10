use Test;

plan 21;

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

# X::InvalidType: `.typename` for a `does`/`hides` parent that isn't declared.
throws-like 'my class C hides Baz { }', X::InvalidType,
    typename => 'Baz', 'hides names the missing typename';
throws-like 'my class C does InNoWayExist { }', X::InvalidType,
    typename => 'InNoWayExist', 'does names the missing typename';

# X::Syntax::Adverb: `.what` on a variable declaration and on an operator
# declarator, two independent raise sites for the same class.
throws-like 'my $x :a', X::Syntax::Adverb,
    what => '$x', 'a colonpair adverb on a declaration names the variable';
{
    use MONKEY;
    throws-like { EVAL 'infix:(&)' }, X::Syntax::Adverb,
        what => ':(&)', 'a signature-literal adverb on an operator names itself';
}

# X::Syntax::Missing: `.pre`/`.post` (source text around the eject point),
# not just `.what`. rakudo itself gets the eject position wrong for this
# construct (https://github.com/rakudo/rakudo/issues/4431, `#?rakudo todo` in
# roast/S32-exceptions/misc.t) and reports `pre => 'if True if '`, `post =>
# '{ };'` instead — this pin asserts mutsu's own (correct) eject point rather
# than reproducing rakudo's bug.
throws-like 'if True if { };', X::Syntax::Missing,
    what => 'block', pre => 'if True ', post => 'if { };',
    'a missing block carries pre/post context';

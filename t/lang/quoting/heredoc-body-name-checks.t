use Test;

# Rakudo 2026.11's heredoc body name checks (#19284): a heredoc body binds its
# names where the heredoc starts, and a name must mean the same declaration
# where the body is written, or the unit fails to compile.

plan 14;

sub refused(Str $code, $class, Str $desc, :$line) {
    subtest $desc => {
        plan $line.defined ?? 3 !! 2;
        my $ex;
        try { EVAL $code; CATCH { default { $ex = $_ } } }
        isa-ok $ex, $class, 'the error class';
        ok $ex ~~ X::Comp, 'a compile-time error';
        is $ex.line, $line, 'reported where rakudo reports it' if $line.defined;
    }
}

refused q:to/CODE/, X::Syntax::Heredoc::HiddenName, 'a name of a block closed on the heredoc line', :line(2);
    my $out; { my $x = 42; $out = qq:to/END/; }
      value: $x
      END
    CODE

refused q:to/CODE/, X::Syntax::Heredoc::AmbiguousName, 'a later declaration on the heredoc line', :line(4);
    my $x = 1;
    {
    my $out = qq:to/END/; my $x = 2;
      value: $x
      END
    }
    CODE

refused q:to/CODE/, X::Syntax::Heredoc::AmbiguousName, 'a closed block name with an outer namesake', :line(3);
    my $o; my $x = 1;
    { my $x = 2; $o = qq:to/END/; }
      v: $x
      END
    CODE

refused q:to/CODE/, X::Syntax::Heredoc::AmbiguousName, 'declared after the body in a scope opened on the line', :line(5);
    my $x = 1;
    my $s = qq:to/END/ ~ do {
      v: $x
      END
      my $x = 2; "z" };
    CODE

refused q:to/CODE/, X::Syntax::Heredoc::HiddenName, 'a parameter of a sub closed on the line';
    sub f($p) { qq:to/END/ }
      p: $p
      END
    CODE

refused q:to/CODE/, X::Syntax::Heredoc::HiddenName, 'a hash, through a second heredoc on the line';
    my $o; { my $a = 1; my %h = a => 1; $o = qq:to/A/ ~ qq:to/B/; }
      fine
      A
      %h<a>
      B
    CODE

my $ex = do { try EVAL q:to/CODE/; $! };
    { my $v = 1; say qq:to/END/; }
      $v
      END
    CODE
is $ex.message, "Name '\$v' in a heredoc body is declared only inside a block that closes\n"
    ~ "on the heredoc's line, before the body. Move the closing brace of the\n"
    ~ "block after the body to use it", 'the HiddenName message';
is $ex.symbol, '$v', 'the symbol';

# What still compiles.
is EVAL(q:to/CODE/), "v: 42\n", 'a routine declared later on the line is the one called';
    my $s = qq:to/END/; sub foo { 42 }
      v: {foo()}
      END
    $s
    CODE

is EVAL(q:to/CODE/), "1\n2\n", 'the topic is exempt';
    my $s = ''; for 1..2 { $s ~= qq:to/END/ }
      $_
      END
    $s
    CODE

is EVAL(q:to/CODE/), "v: 8\n", 'a block in the body declares its own names';
    my $x = 5;
    qq:to/END/;
      v: { my $y = 3; $y + $x }
      END
    CODE

is EVAL(q:to/CODE/), "foo\n", 'a name declared outside the closed block';
    my $a;
    sub f() { $a = 'foo'; qq:to/END/ }
      $a
      END
    f()
    CODE

is EVAL(q:to/CODE/), "x: 1\n", 'a heredoc statement inside a block sees its locals';
    sub g() {
        my $x = 1;
        return qq:to/END/;
        x: $x
        END
    }
    g()
    CODE

lives-ok { EVAL q:to/CODE/ }, 'a closed block with no names in the body';
    my $x;
    if $x { say q:to/END/ }
      This is alright
      END
    CODE

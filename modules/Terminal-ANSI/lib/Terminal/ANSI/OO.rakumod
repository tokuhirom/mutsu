class Terminal::ANSI::OO {
use Terminal::ANSI;

method pod { $=pod }

has Bool $.get-codes is rw = False;

my @colors = <black red green yellow blue magenta cyan white>;
my %colornums = (|@colors, |(@colors.map({"bright-$_"}))).kv.reverse;
my %bg = %colornums.map: -> (:$key, :$value) { "bg-$key" => $value}

method FALLBACK ($name, |c) {
  with %colornums{$name} -> $n {
    return tget { set-fg-color($n) }
  }
  with %bg{$name} -> $n {
    return tget { set-bg-color($n) }
  }
  my $sub = &::("$name") or die "no such method $name";
  return tget { $sub(|c) } if $!get-codes;
  $sub(|c)
}

sub parse-color($c) {
  $c.substr(1).comb(2).map(*.parse-base(16));
}

multi method color($c) {
  if $c.starts-with('#') {
    return self.color(|parse-color($c));
  }
  tget { set-fg-color($c) }
}

multi method bg-color($c) {
  if $c.starts-with('#') {
    return self.bg-color(|parse-color($c));
  }
  tget { set-bg-color($c) }
}

multi method color($r,$g,$b) {
  tget { set-fg-rgb-color($r,$g,$b) }
}

multi method bg-color($r,$g,$b) {
  tget { set-bg-rgb-color($r,$g,$b) }
}

sub t is export(:t) {
  Terminal::ANSI::OO.new(:get-codes);
}
}

sub EXPORT($t = 't') {
  %( $t => Terminal::ANSI::OO.new(:get-codes) )
}

=NAME Terminal::ANSI::OO -- Object-oriented interface to Terminal::ANSI

=begin SYNOPSIS

=begin code

   use Terminal::ANSI::OO :t;

   put t.clear-screen;
   put t.red ~ "red" ~ t.text-reset;
   put t.bright-blue ~ "bright blue" ~ t.text-reset;
   put t.color(1) ~ 'color 1';
   put t.color(0,0,255) ~ 'blue';
   put t.bg-color(255,0,0) ~ 'red bg';
   put t.color('#00ff00') ~ 'green';

=end code

=end SYNOPSIS

=begin DESCRIPTION

Terminal::ANSI::OO provides an OO interface to Terminal::ANSI.
An object can return all of the escape codes, rather then printing
them to stdout.  Passing ":t" will create and store such an object
in the "t" variable.

All of the functions in Terminal::ANSI are available as methods, as
well as t.$color, t.bright-$color, and t.bg-$color, where $color
is one of: black, red, green, yellow, blue, magenta, cyan, or white.

Also t.color(N) and t.bg-color(N) (where N is between 0 and 255), will
return the escape code for changing to color N.

Also t.color(R,G,B) and t.bg-color(R,G,B) (where R,G,B are between 0
and 255), will return the escape code for changing to rgb color R,G,B.

=end DESCRIPTION

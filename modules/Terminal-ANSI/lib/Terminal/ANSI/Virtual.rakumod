use OO::Monitors;
unit monitor Terminal::ANSI::Virtual;

has @.chars;
has Bool $.only is rw;
has Int $.scroll-top is rw;
has Int $.scroll-bottom is rw;
has $.state;

method print-at($r,$c,$str) {
  @.chars[$r] //= [];
  @.chars[$r][$c..^( $c + $str.chars )] = $str.comb;
}

method scroll-down($n) {
  return @!chars.unshift([]) if $.state.scroll-top == -1;
  my $top = $.state.scroll-top max 0;
  my $bot = $.state.scroll-bottom min (@!chars.elems - 1);
  @!chars[$top..$bot] = (Nil, |@!chars[$top..^$bot]);
}

method scroll-up($n) {
  return @!chars.shift if $.state.scroll-bottom == -1;
  my $top = $.state.scroll-top max 0;
  my $bot = $.state.scroll-bottom min (@!chars.elems - 1);
  @!chars[$top..$bot] = (|@!chars[$top^..$bot],Nil);
}

method at($r,$c) {
  @.chars[$r][$c];
}

#| Render the screen into a string
method render(:$w = %*ENV<TERMINAL_UI_COLS>, :$h = %*ENV<TERMINAL_UI_ROWS>) {
  my $out = '';
  for 1..$h -> $row {
     $out ~= join '', self.chars[$row][1..$w].map: { .defined ?? $_ !! " " }
     $out ~= "\n";
  }
  $out;
}

method clear-screen {
  @.chars = ();
}

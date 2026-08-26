unit module Terminal::ANSI:ver<0.0.25>;
#= ANSI Escape Sequences
#= ──
#= This module exports functions for emitting ANSI escape sequences.
#= ──
#= It also maintains some state about the screen, such as a currently
#= active scroll region.
#= ──
#= It can optionally maintain the state of all of the characters on
#= the screen, by writing to a virtual screen.

use Terminal::ANSI::Virtual;
use Terminal::ANSI::State;

constant CSI = "\e[";

my $virtual;
my $state = Terminal::ANSI::State.new;
my $terminal-ansi-delay;
my Lock $printing .= new;

sub nums(*@n) {
 # trim trailing nils?
 @n.map({$_ // ''}).join(';')
}


sub csi($str) {
  print-or-buffer CSI ~ $str;
}

sub esc($str) {
  print-or-buffer "\e" ~ $str;
}

sub print-or-buffer($str) {
  $printing.protect: {
    if $*terminal-ansi-buffer {
      $*terminal-ansi-out ~= $str;
    } else {
      sleep $_ with $terminal-ansi-delay;
      print $str unless $virtual.?only;
    }
  }
}


#| Clear the screen.
sub clear-screen is export {
  .clear-screen with $virtual;
  csi "H\e[J";
}

#| Move the cursor to line, column.
sub move-to($l,$c = 1) is export {
  csi nums($l,$c) ~ "H";
}

#| Move to home (0,0).
sub home is export {
  csi 'H';
}

#| Set scroll region to top, bottom.
sub set-scroll-region($top,$bottom) is export {
  $state.scroll-bottom = $bottom;
  $state.scroll-top = $top;
  csi nums($top,$bottom) ~ "r";
}

#| Reset the scroll region.
sub reset-scroll-region is export {
  $state.scroll-bottom = -1;
  $state.scroll-top = -1;
  csi 'r';
}

#| Scroll down by an amount.
sub scroll-down($n = 1) is export {
  .scroll-down($n) with $virtual;
  move-to $state.scroll-top, 1 unless $state.scroll-top == -1;
  for 1..$n {
    esc "M";
  }
  # csi $n ~ 'T';
}

#| Scroll up by an amount.
sub scroll-up($n = 1) is export {
  .scroll-up($n) with $virtual;
  move-to $state.scroll-bottom, 10 unless $state.scroll-bottom == -1;
  for 1..$n {
    esc "D";
  }
  # csi $n ~ 'S';
}

#| Hide the cursor.
sub hide-cursor is export {
  csi '?25l';
}

#| Show the cursor.
sub show-cursor is export {
  csi "?25h";
}

#| Save screen state (smcup).
sub save-screen is export {
  csi '?1049h'
}

#| Restore screen (rmcup).
sub restore-screen is export {
  csi '?1049l'
}

#| Turn off cursor (civis).
sub cursor-off is export {
  csi '?25l'
}

#| Turn on cursor (cnorm).
sub cursor-on is export {
  csi '?25h';
}

#| Move cursor up.
sub cursor-up($amt = 1) is export {
  csi $amt ~ 'A';
}

#| Move cursor down.
sub cursor-down($amt = 1) is export {
  csi $amt ~ 'B';
}

#| Move cursor right.
sub cursor-right($amt = 1) is export {
  csi $amt ~ 'C';
}

#| Move cursor left.
sub cursor-left($amt = 1) is export {
  csi $amt ~ 'D';
}

#| Move cursor to the beginning of the next line, n lines down.
sub cursor-next-line($n = 1) is export {
  csi $n ~ 'E';
}

#| Move cursor to the beginning of the previous line, n lines up.
sub cursor-prev-line($n = 1) is export {
  csi $n ~ 'F';
}

#| Atomic move + print.
sub print-at($r,$c,$str) is export {
  .print-at($r,$c,$str) with $virtual;
  csi nums($r,$c) ~ "H" ~ $str;
}

#| set fg color to $n.
sub set-fg-color($n) is export {
  csi "38;5;" ~ $n ~ "m";
}

#| set fg color to $n.
sub set-fg-rgb-color($r,$g,$b) is export {
  csi "38;2;$r;$g;$b" ~ "m";
}

#| Set bg color to $n.
sub set-bg-color($n) is export {
  csi "48;5;" ~ $n ~ "m";
}

#| set bg color to $n.
sub set-bg-rgb-color($r,$g,$b) is export {
  csi "48;2;$r;$g;$b" ~ "m";
}

#| Set bg color to default
sub set-bg-default is export {
  csi "49;m";
}

#| Save the cursor position.
sub save-cursor is export {
  csi ~ 's';
}

#| restore the cursor position.
sub restore-cursor is export {
  csi ~ 'u';
}

#| Move to start of line.
sub start-of-line is export {
  print-or-buffer "\r"
}

#| Erase to end of screen.
sub erase-to-end-of-screen is export {
  csi 'D';
}

#| Erase to start of screen.
sub erase-to-start-of-screen is export {
  csi '1D';
}

#| Erase to end of line.
sub erase-to-end-of-line is export {
  csi 'K';
}

#| Erase to start of line.
sub erase-to-start-of-line is export {
  csi '1K';
}

#| Erase current line.
sub erase-current-line is export {
  csi '2K';
}

#| Normal video: same as text-reset
sub normal-video is export {
  csi '0m';
}

#| Text reset (same as normal-video)
sub text-reset is export {
  csi '0m';
}

#| Bold (increased intensity)
sub bold is export {
  csi '1m';
}

#| Faint (lower intensity)
sub faint is export {
  csi '2m';
}

#| italic
sub italic is export {
  csi '3m';
}

#| Underline
sub underline is export {
  csi '4m';
}

#| Blink
sub blink is export {
  csi '5m';
}

#| Reverse video
sub reverse-video is export {
  csi '7m';
}

#| Crossed out (striked)
sub strike is export {
  csi '9m';
}

#| Alternate font n (between 11 and 19)
sub alt-font($n where 11 <= $n <= 19) is export {
  csi $n ~ 'm';
}

#| Get strings instead of printing them.
sub tget(&callable) is export {
  my $*terminal-ansi-buffer = True;
  my $*terminal-ansi-out = "";
  callable;
  return "" if $virtual.?only;
  $*terminal-ansi-out;
}

#| Slow down the output for debugging
sub terminal-ansi-throttle($rate = 0.1) is export {
  $terminal-ansi-delay = $rate;
}

#| Combine a series of outputs into one.
sub atomically(&callable) is export {
  $printing.protect: {
    callable;
  }
  unless $virtual.?only {
    sleep $_ with $terminal-ansi-delay;
  }
}

#| Send commands to a virtual screen
sub enable-virtual is export {
  $virtual //= Terminal::ANSI::Virtual.new(:$state);
}

#| Get the virtual screen receiving commands
sub virtual-screen is export {
  $virtual;
}

#| Suppress all output
sub disable-output is export {
  enable-virtual;
  $virtual.only = True;
}

#| Enable output
sub enable-output is export {
  $virtual.only = False;
}

#| Return a description of the given input sequence
sub parse-input($str) is export {
  my %inputs =
   "\e[A" => "Up",
   "\e[B" => "Down",
   "\e[C" => "Right",
   "\e[D" => "Left",
   "\e[H" => "Home",
   "\e[1~" => "Home",
   "\e[4~" => "End",
   "\t" => "Tab",
   "\e[Z" => "Untab",
   "\e[F" => "End",
   "\e[G" => "Keypad 5",
   "\e[1P" => "F1",
   "\e[1Q" => "F2",
   "\e[1R" => "F3",
   "\e[1S" => "F4",

   "\e[10~" => "F0",
   "\e[11~" => "F1",
   "\eOP"   => "F1",
   "\eOQ"   => "F2",
   "\eOR"   => "F3",
   "\eOS"   => "F4",
   "\e[12~" => "F2",
   "\e[13~" => "F3",
   "\e[14~" => "F4",
   "\e[15~" => "F5",

   "\e[17~" => "F6",
   "\e[18~" => "F7",
   "\e[19~" => "F8",
   "\e[20~" => "F9",
   "\e[21~" => "F10",

   "\e[23~" => "F11",
   "\e[24~" => "F12",
   "\e[25~" => "F13",
   "\e[26~" => "F14",

   "\e[28~" => "F15",
   "\e[29~" => "F16",

   "\e[31~" => "F17",
   "\e[32~" => "F18",
   "\e[33~" => "F19",
   "\e[34~" => "F20",
   "\e[2~"  => "Insert",
   "\e[3~"  => "Delete",
   "\e[5~"  => "PageUp",
   "\e[1;2P" => "PrtSc",
   "\e[6~"  => "PageDown",
   "\e"     => "Esc",
   "\r"     => "Enter",
   "\x[7F]" => "Delete",

  ;

  .return with %inputs{ $str };
  if $str.chars == 2 && $str.starts-with("\e") {
    return "ALT + " ~ $str.substr(1,1);
  }

  if $str.starts-with("\e[1;") && $str.chars > 5 {
    my $modifier = $str.substr(4,1) - 1;
    my @mods;
    @mods.push: "META" if $modifier +& 8;
    @mods.push: "ALT" if $modifier +& 2;
    @mods.push: "CTRL" if $modifier +& 4;
    @mods.push: "SHIFT" if $modifier +& 1;
    with %inputs{ "\e[" ~ $str.substr(5) } -> $key {
      return @mods.join(" + ") ~ " + $key";
    }
  }
  Nil
}

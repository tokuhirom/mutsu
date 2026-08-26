enum Loglevels <<:TRACE(1) DEBUG INFO WARNING ERROR FATAL>>;

use Log::Async::Context;

use Terminal::ANSI::OO 't';

our %LOGCOLORS =
    trace   => '#7bb274',  # Sage green
    debug   => '#6b8ba4',  # Dusty blue
    info    => '#cba560',  # Goldenrod
    warning => '#ab6ca2',  # Wisteria
    error   => '#d9544d',  # Coral red
    fatal   => '#ff0000',  # Red
;

class Log::Async:ver<0.0.7>:auth<github:bduggan> {
    has $.source = Supplier.new;
    has Tap @.taps;
    has Supply $.messages;
    has $.contextualizer is rw;
    has $.untapped-ok is rw = False;

    my $instance;
    method instance {
        return $instance;
    }
    method set-instance($i) {
        $instance = $i;
    }
    method add-context(:$context = Log::Async::Context.new) {
      $.contextualizer = $context;
    }

    method close-taps {
        .close for @.taps;
    }

    method add-tap(Code $c, :$level, :$msg --> Tap ) {
        $!messages //= self.source.Supply;
        my $supply = $!messages;
        $supply = $supply.grep( { $^m<level> ~~ $level }) with $level;
        $supply = $supply.grep( { $^m<msg> ~~ $msg }) with $msg;
        my $tap = $supply.act($c);
        @.taps.push: $tap;
        return $tap;
    }

    method remove-tap(Tap $t) {
      my ($i) = @.taps.grep( { $_ eq $t }, :k );
      $t.close;
      @.taps.splice($i,1,());
    }

    multi method send-to( IO::Handle:D $fh, Code :$formatter is copy, |args --> Tap) {
        $formatter //= -> $m, :$fh {
            $fh.say: "{ $m<when> } ({$m<THREAD>.id}) { $m<level>.lc }: { $m<msg> }",
        }
        my $fmt = $formatter but role { method is-hidden-from-backtrace { True } };
        self.add-tap: -> $m {
            $fmt($m,:$fh);
            $fh.flush;
        }, done => { $fh.close }, quit => { $fh.close }, |args
    }

    multi method send-to(Str $path, Code :$formatter, Bool :$out-buffer = False, |args --> Tap) {
        my $fh = open($path,:a,:$out-buffer) or die "error opening $path";
        self.send-to($fh, :$formatter, |args);
    }

    multi method send-to(IO::Path $path, Code :$formatter, Bool :$out-buffer = False, |args --> Tap) {
        my $fh = $path.open(:a,:$out-buffer) or die "error opening $path";
        self.send-to($fh, :$formatter, |args);
    }

    method log(
      Str :$msg,
      Loglevels:D :$level,
      CallFrame :$frame = callframe(1),
      DateTime :$when = DateTime.now
    ) is hidden-from-backtrace {
        my $ctx = $_.generate with self.contextualizer;
        my $m = { :$msg, :$level, :$when, :$*THREAD, :$frame, :$ctx };
        if @.taps == 0 and not $!untapped-ok {
            note 'Message sent without taps.';
            note 'Try "logger.send-to($*ERR)" or "logger.untapped-ok = True"';
            $!untapped-ok = True;
        }
        (start $.source.emit($m))
          .then({ say $^p.cause unless $^p.status == Kept });
    }

    method done() {
        start { sleep 0.1; $.source.done };
        $.source.Supply.wait;
    }
}

sub set-logger($new) is export(:MANDATORY) {
    Log::Async.set-instance($new);
}
sub logger is export(:MANDATORY) {
    Log::Async.instance;
}

sub trace($msg)   is export(:MANDATORY) is hidden-from-backtrace { logger.log( :$msg :level(TRACE)  :frame(callframe(1))) }
sub debug($msg)   is export(:MANDATORY) is hidden-from-backtrace { logger.log( :$msg :level(DEBUG)  :frame(callframe(1))) }
sub info($msg)    is export(:MANDATORY) is hidden-from-backtrace { logger.log( :$msg :level(INFO)   :frame(callframe(1))) }
sub error($msg)   is export(:MANDATORY) is hidden-from-backtrace { logger.log( :$msg :level(ERROR)  :frame(callframe(1))) }
sub warning($msg) is export(:MANDATORY) is hidden-from-backtrace { logger.log( :$msg :level(WARNING):frame(callframe(1))) }
sub fatal($msg)   is export(:MANDATORY) is hidden-from-backtrace { logger.log( :$msg :level(FATAL)  :frame(callframe(1))) }

sub EXPORT($arg = Nil, $arg2 = Nil, $arg3 = Nil) {
  return { } unless $arg || $arg2;
  my $using-args = False;
  my $level = WARNING;
  my $to = $*ERR;
  my @opts = ($arg, $arg2, $arg3).grep: *.defined;
  my $formatter = -> $m, :$fh { $fh.say: "{ $m<when> } ({$m<THREAD>.id}) { $m<level>.lc }: { $m<msg> }" }
  for @opts {
    when 'trace' { $level = ( * >= TRACE ) }
    when 'debug' { $level = ( * >= DEBUG ) }
    when 'info' { $level = ( * >= INFO ) }
    when 'warn' | 'warning' { $level = ( * >= WARNING ) }
    when 'error' { $level = ( * >= ERROR ) }
    when 'fatal' { $level = ( * >= FATAL ) }
    when 'color' | 'colour' {
      my %colors = %LOGCOLORS;

      $formatter = sub ( $m, :$fh ) {
          $fh.say: ($fh.t ?? t.color( %colors{$m<level>.lc} // '#ff0001' ) !! '' )
          ~ $m<when>
          ~ ' ' ~ ('[' ~ $m<level>.lc ~ ']').fmt('%-9s')
          ~ (' (' ~ $*THREAD.id ~ ')').fmt('%2s')
          ~ ' ' ~ $m<msg>
          ~ ($fh.t ?? t.text-reset !! '' );
      }
    }
    when 'use-args' | 'cli' {
      $using-args = True;
      my regex opt { 'trace' | 'debug' | 'info' | 'warn' | 'warning' | 'error' | 'fatal' }
      my regex filename { \S+ }
      if @*ARGS.first( { / '--' 'logfile=' <filename> / }) -> $opt {
        $opt ~~ /'--logfile=' (.*)/ and $to = ~$0;
        @*ARGS .= grep: { ! / '--' 'logfile=' <filename> / };
      }
      if @*ARGS.first( { / '--' 'log=' <opt> / } ) -> $log-arg {
        $log-arg ~~ / '--' 'log=' <opt> /;
        my $opt = ~$<opt>;
        @*ARGS = @*ARGS.grep( { ! / '--' 'log=' <opt> / } );
        $level = * ≥ Loglevels::{$opt.uc};
      }
      if @*ARGS.grep( '-v' ) {
        @*ARGS .= grep: * ne '-v';
        $level = * ≥ Loglevels::INFO;
      }
      if @*ARGS.grep( '-d' ) {
        @*ARGS .= grep: * ne '-d';
        $level = * ≥ Loglevels::DEBUG;
      }
    }
  }
  if @opts.elems {
     logger.send-to: $to, :$level, :$formatter;
  }
  return { } unless $using-args;
  return {
    '&GENERATE-USAGE' => sub (&main,|args) {
      my $orig = &*GENERATE-USAGE(&main, |args);
      return $orig ~ q:to/ARGS/;


      Additional arguments for logging:

         --log=[trace|debug|info|warn|warning|error|fatal]
          -v  # be verbose
          -d  # show debug output
         --logfile=filename # send logs to a file
      ARGS
    }
  }
}

INIT {
    set-logger(Log::Async.new) unless logger;
}
END {
    Log::Async.instance.done if Log::Async.instance;
}

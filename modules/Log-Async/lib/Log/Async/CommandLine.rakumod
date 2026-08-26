unit module Log::Async::CommandLine;

use Log::Async;

sub parse-log-args {
    my @keepargs;

    my $loglevel = WARNING;
    my $logfh = $*OUT;
    my $silent;
    my $color;
    my $threadid;

    for @*ARGS {
        when '--silent'|'-q' {
            $silent = True;
            logger.untapped-ok = True;
        }
        when '-v' {
            $loglevel = INFO;
        }
        when /^'--'(trace|debug|info|warning|error|fatal)$/ {
            $loglevel = Loglevels::{$0.uc};
        }
        when /^'--logfile='(.+)$/ {
            $logfh = open($0.Str, :a);
        }
        when '--logcolor' {
            $color = True;
        }
        when '--logthreadid' {
            $threadid = True;
        }
        default {
            push @keepargs, $_;
        }
    }

    my &print-log = $color
      ?? sub ($logfh, $m, $threadid) {
             state %colors =
                 TRACE   => "\e[35;1m", # magenta
                 DEBUG   => "\e[34;1m", # blue
                 INFO    => "\e[32;1m", # green
                 WARNING => "\e[33;1m", # yellow
                 ERROR   => "\e[31;1m", # red
                 FATAL   => "\e[31;1m"; # red

             $logfh.say("$m<when> " ~
                         ("($m<THREAD>.id()) " if $threadid) ~
                         "%colors{$m<level>}$m<level>.lc()" ~
                         ("\e[0m" unless $m<level> ~~ ERROR|FATAL) ~
                         ": $m<msg>\e[0m");
         }
      !! sub ($logfh, $m, $threadid) {
             $logfh.say("$m<when> " ~
                        ("($m<THREAD>.id()) " if $threadid) ~
                        "$m<level>.lc(): $m<msg>");
         };

    logger.add-tap(-> $m { &print-log($logfh, $m, $threadid) },
                   :level(* >= $loglevel))
        unless $silent;

    @*ARGS = @keepargs;
}

INIT {
    parse-log-args;
}

=begin pod

=head1 NAME

Log::Async::CommandLine

=head1 SYNOPSIS

    use Log::Async::CommandLine;

    ./someprogram [--trace]
                  [--debug]
                  [--info | -v]
                  [--warning]  # Default
                  [--error]
                  [--fatal]
                  [--silent | -q]

    ./someprogram [--logcolor] # Colorize log output

    ./someprogram [--logthreadid] # Include thread id in log msg

    ./someprogram [--logfile=/var/log/mylogfile]

=head1 DESCRIPTION

A tiny wrapper around Log::Async to set some basic logging
configuration stuff from the commandline.

It will log either to $*OUT or the specified logfile messages with a
level >= the specified level (or warning by default).

Adding the '--logcolor' option will colorize the log output a little.

Adding '--logthreadid' will include the thread id in the logged message.

=end pod

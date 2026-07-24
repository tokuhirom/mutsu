unit module File::Temp:ver<0.0.12>;

use File::Directory::Tree:ver<0.2+>:auth<zef:raku-community-modules>;

# Characters used to create temporary file/directory names
my @filechars = flat('a'..'z', 'A'..'Z', 0..9, '_');
constant MAX-RETRIES = 10;

my %roster;
my %keptfd;
my Lock $roster-lock = Lock.new;
my Lock $keptfd-lock = Lock.new;

my role File::Temp::AutoUnlink {
    submethod DESTROY {
        given self.path {
            if $_.path.IO ~~ :f { # Workaround, should just be $_ ~~ :f
                self.close;
                my $did;
                $roster-lock.protect: {
                    $did = %roster{$_.path}:delete;
                    $_.unlink if $did;
                };
                unless $did {
                    # Something (probably END) already unlinked it
                    # We could have a debug/testing flag do something here.
                }
            }
            # Directories will not get here yet
        }
    }
}

my sub make-temp($type, $template, $tempdir, $prefix, $suffix, $unlink) {
    my $count = MAX-RETRIES;
    while $count-- {
        my $tempfile = $template;
        $tempfile ~~ s/ '*' ** 4..* /{ @filechars.roll($/.chars).join }/;
        my $name = $*SPEC.catfile($tempdir,"$prefix$tempfile$suffix");
        next if $name.IO.e;

        my $fh;
        if $type eq 'file' {
            $fh = try { CATCH { next }; open $name, :rw, :exclusive;  };
            chmod(0o600,$name);
        }
        else {
            try { CATCH { next }; mkdir($name, 0o700) };
        }

        if $unlink {
            $roster-lock.protect: {
                %roster{$name} = $fh;  # UNCOVERABLE
            };
            $fh &&= $fh does File::Temp::AutoUnlink;
        }
        elsif ($type eq 'file') {
            $keptfd-lock.protect: {
                %keptfd{$name} = $fh;  # UNCOVERABLE
            }
        }
        return $type eq 'file' ?? ($name,$fh) !! $name;
    }
    fail "Unable to open temporary $type after {MAX-RETRIES} attempts within \"$tempdir\"";
}

my sub tempfile (
     $tmpl     = '*' x 10,  # positional template
    :$tempdir  = $*TMPDIR,  # where to create these temp files
    :$prefix   = '',        # filename prefix
    :$suffix   = '',        # filename suffix
    :$unlink   = 1,         # remove when program exits?
    :$template = $tmpl      # required named template
) is export {
    make-temp('file', $template, $tempdir, $prefix, $suffix, $unlink)
}

my sub tempdir (
     $tmpl     = '*' x 10,  # positional template
    :$tempdir  = $*TMPDIR,  # where to create tempdir
    :$prefix   = '',        # directory prefix
    :$suffix   = '',        # directory suffix
    :$unlink   = 1,         # remove when program exits?
    :$template = $tmpl      # required named template
) is export {
    make-temp('dir', $template, $tempdir, $prefix, $suffix, $unlink)
}

#- END -------------------------------------------------------------------------
END {
    my sub clean-roster() {
        # Workaround -- directly using %roster.keys not reliable under stress
        my @rk = %roster.keys;
        for @rk -> $fn {
            my $io := $fn.IO;
            if $io.f {
                %roster{$fn}.close;
                unlink($fn);
            }
            elsif $io.d {  # UNCOVERABLE
                rmtree($fn);
            }
        }
        %roster = ();
    }

    my sub clean-keptfd() {
        my @kk = %keptfd.keys;
        for @kk -> $fn {
            %keptfd{$fn}.close;
        }
        %keptfd = ();
    }

    # Do cleanup, either thread-safe if in normal execution, or
    # non-threadsafe during (pre-)compilation or with --doc
    $roster-lock ?? $roster-lock.protect(&clean-roster) !! clean-roster;
    $keptfd-lock ?? $keptfd-lock.protect(&clean-keptfd) !! clean-keptfd;
}

# vim: expandtab shiftwidth=4

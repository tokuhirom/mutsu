#- mktree ----------------------------------------------------------------------
my sub mktree(IO(Cool) $io is copy, Int :$mask = 0o777 ) is export {
    unless $io.d {
        my @makedirs;
        until $io.e {  # UNCOVERABLE
            @makedirs.unshift($io);
            $io .= parent;
        }
        for @makedirs -> $dir {
            mkdir($dir, $mask);
            return False unless $dir.e;
        }
    }
    True
}

#- empty-directory -------------------------------------------------------------
my sub empty-directory(IO(Cool) $io) is export {
    if $io.d {
        for eager $io.dir -> $path {
            if !$path.l and $path.d {
                rmtree $path;
            }
            else {
                unlink $path;
            }
        }
        True
    }
    else {
        "$io is not a directory".Failure
    }
}


#- rmtree ----------------------------------------------------------------------
my sub rmtree(IO(Cool) $io) is export {
    if $io.e {
        if $io.d {
            return False unless empty-directory($io);
            return False unless rmdir($io);
            True
        }
        else {
            "$io is not a directory".Failure
        }
    }
    else {
        True
    }
}

#- hack ------------------------------------------------------------------------
# To allow version fetching in test files
unit module File::Directory::Tree:ver<0.2>:auth<zef:raku-community-modules>;

# vim: expandtab shiftwidth=4

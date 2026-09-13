
my role BetterWorld {
    method do_pragma_or_load_module(Mu $/ is raw, $use, $thisname?) {
        my $name;
        my %cp;
        my $arglist;

        my $RMD := self.RAKUDO_MODULE_DEBUG;

        if $thisname {  # UNCOVERABLE
            $name := $thisname;
        }
        else {
            my $lnd  := self.dissect_longname($/.hash<module_name>.hash<longname>);
            $name    := $lnd.name;
            %cp      := $lnd.colonpairs_hash($use ?? 'use' !! 'no');

            # That's why we do all that:
            return if %cp<if>:exists && %cp<if> == False;

            $arglist := self.arglist($/);
        }

        unless %cp {
            if self.do_pragma($/,$name,$use,$arglist) { return }
        }

        if $use {
            $RMD("Attempting to load '$name'") if $RMD;

            # old way:
            my $comp_unit := try self.load_module($/, $name, %cp, $*GLOBALish);
            # new way:
            $! and $comp_unit := self.load_module($/, $name, %cp, self.cur_lexpad);

            $RMD("Performing imports for '$name'") if $RMD;
            self.do_import($/, $comp_unit.handle, $name, $arglist);
            self.import_EXPORTHOW($/, $comp_unit.handle);
            $RMD("Imports for '$name' done") if $RMD;
        }
        else {
            die "Don't know how to 'no $name' just yet";
        }
    }
}

my role Actions {
    use experimental :rakuast;

    method statement-control:sym<use>(Mu $/) {
        if $/.hash<module-name>.ast -> $ast {
            if my @cp = $ast.colonpairs {
                my $last := @cp.pop;
                if $last.key eq 'if' {
                    my $value := RakuAST::BeginTime.IMPL-BEGIN-TIME-EVALUATE(
                      $last.value, $*R, $*CU.context
                    );
                    if $value.defined {
                        if $value {
                            $ast.set-colonpairs(@cp.FLATTENABLE_LIST);
                        }
                        else {
                            self.attach: $/, RakuAST::Statement::Empty.new;
                            return;
                        }
                    }
                    else {
                        $/.panic("Did not provide compile-time-value for :if adverb in use statement");
                    }
                }
            }
        }

        nextsame;
    }
}

sub EXPORT(|) {
    if Raku.legacy {
        $*W.HOW.mixin($*W, BetterWorld);
    }
    else {
        my $LANG := $*LANG;
        $LANG.define_slang('MAIN',
          $LANG.slang_grammar('MAIN'),
          $LANG.slang_actions('MAIN').^mixin(Actions)
        );
    }

    BEGIN Map.new
}

# vim: expandtab shiftwidth=4

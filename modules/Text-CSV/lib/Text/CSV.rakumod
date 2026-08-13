#!raku

use Slang::Tuxic;
use File::Temp;
use Text::IO::String;

my $VERSION = "0.022";

my constant $opt_v = %*ENV<RAKU_VERBOSE> // 1;

my constant %errors =
    # Success
       0 => "",

    # Generic errors
    1000 => "INI - constructor failed",
    1001 => "INI - separator is equal to quote- or escape sequence",
    1002 => "INI - allow_whitespace with escape or quoter SP or TAB",
    1003 => "INI - \r or \n in main attr not allowed",
    1004 => "INI - callbacks should be Hash or undefined",
#   1005 => "INI - EOL too long",
#   1006 => "INI - SEP too long",
#   1007 => "INI - QUOTE too long",
    1008 => "INI - SEP undefined",

    1010 => "INI - the header is empty",
    1011 => "INI - the header contains more than one valid separator",
    1012 => "INI - the header contains an empty field",
    1013 => "INI - the header contains nun-unique fields",
#   1014 => "INI - header called on undefined stream",

    # Syntax errors
    1500 => "PRM - Invalid/unsupported argument(s)",
    # For perl5 compatability. Due to strict typing, these do not apply
    1501 => "PRM - The key attribute is passed as an unsupported type",

    # Parse errors
    2010 => "ECR - QUO char inside quotes followed by CR not part of EOL", # 5
    2011 => "ECR - Characters after end of quoted field",
    2012 => "EOF - End of data in parsing input stream",
    2013 => "ESP - Specification error for fragments RFC7111",
    2014 => "ENF - Inconsistent number of fields",
    2015 => "ERW - Empty row",

    #  EIQ - Error Inside Quotes
    2021 => "EIQ - NL or EOL inside quotes, binary off",
    2022 => "EIQ - CR char inside quotes, binary off",
    2023 => "EIQ - QUO sequence not allowed",
    2024 => "EIQ - EOF cannot be escaped, not even inside quotes",
    2025 => "EIQ - Loose unescaped escape",
    2026 => "EIQ - Binary character inside quoted field, binary off",
    2027 => "EIQ - Quoted field not terminated",

    # EIF - Error Inside Field
#   2030 => "EIF - NL char inside unquoted verbatim, binary off",
    2031 => "EIF - CR char is first char of field, not part of EOL",
    2032 => "EIF - CR char inside unquoted, not part of EOL",
    2034 => "EIF - Loose unescaped quote",
    2035 => "EIF - Escaped EOF in unquoted field",
    2036 => "EIF - ESC error",
    2037 => "EIF - Binary character in unquoted field, binary off",

    # Combine errors
    2110 => "ECB - Binary character in Combine, binary off",

    # IO errors
    2200 => "EIO - print to IO failed. See errno",

    # Hash-Ref errors
    3001 => "EHR - Unsupported syntax for column_names ()",
    3002 => "EHR - getline_hr () called before column_names ()",
    3003 => "EHR - bind_columns () and column_names () fields count mismatch",
    3004 => "EHR - bind_columns () only accepts refs to scalars",
    3006 => "EHR - bind_columns () did not pass enough refs for parsed fields",
    3007 => "EHR - bind_columns needs refs to writable scalars",
    3008 => "EHR - unexpected error in bound fields",
    3009 => "EHR - print (Hash) called before column_names ()",
    3010 => "EHR - print (Hash) called with invalid arguments",

    3100 => "EHK - Unsupported callback",

    # Content errors
    4001 => "PRM - The key does not exist as field in the data",

    # csv CI
    5000 => "CSV - Unsupported type for in",
    5001 => "CSV - Unsupported type for out",
    ;

sub progress (*@y) {
    my Str $x;
    my @x = @y.map ( -> \x --> Str { x.Str } );
    my $line = callframe (1).annotations<line>;
    for (@x) {
        s{^(\d+)$}   = sprintf "%3d -", $_;
        s:g{"True,"} = "True, ";
        s:g{"new("}  = "new (";
        $x ~= $_ ~ " ";
        }
    $x.say;
    } # progress



class RangeSet {

    has Pair @!ranges;

    method new (**@ranges) {
        self.bless (:@ranges);
        }

    multi method add (Pair:D $p)              { @!ranges.push: $p;                }
    multi method add (Range:D $r)             { @!ranges.push: $r.min => $r.max;  }
    multi method add (Int:D $from)            { @!ranges.push: $from  => $from;   }
    multi method add (Int:D $from, Num:D $to) { @!ranges.push: $from  => $to;     }
    multi method add (Int:D $from, Any:D $to) { @!ranges.push: $from  => $to.Num; }

    method min () { @!ranges>>.key.min   }
    method max () { @!ranges>>.value.max }

    method in (Int:D $i) returns Bool {
        for @!ranges -> $r {
            $i >= $r.key && $i <= $r.value and return True;
            }
        False;
        }

    method to_list () {
        gather {
            my Int $max = -1;
            for sort @!ranges -> $r {
                my $from = ($r.key, $max + 1).max.Int;
                my $to   = $r.value == Inf ?? 65535 !! $r.value;
                take (($from .. $to).Slip);
                $r.value == Inf and last;
                $max = ($max, $r.value).max.Int;
                }
            }
        }

    multi method list (Int $last) {
        my Int @x;
        my Int $max = -1;
        for sort @!ranges -> $r {
            my $from = ($r.key,   $max  + 1).max.Int;
            my $to   = ($r.value, $last - 1).min.Int;
            $from > $to and next;
            @x.append: $from .. $to;
            $to >= $last || $r.value == Inf and last;
            $max = ($max, $r.value).max.Int;
            }
        @x;
        }

    multi method list () {
        my @x;
        my Int $max = -1;
        for sort @!ranges -> $r {
            my $from = ($r.key, $max + 1).max.Int;
            $from > $r.value and next;
            @x.plan: $from .. $r.value;
            $r.value == Inf and last;
            $max = ($max, $r.value).max.Int;
            }
        @x.elems ?? @x !! ($.min .. $.max).grep (self);
        }
    }

class CellSet {

    class CellRange {

        has RangeSet $.row;
        has RangeSet $.col;

        method in (Int $row, Int $col) {
            $!row && $!col && $!row.in ($row) && $!col.in ($col);
            }

        method show {
            say "Row: ", $.row.to_list.raku;
            say "Col: ", $.col.to_list.raku;
            }
        }

    has CellRange @!cr;

    method add (Int:D $tlr,            Int:D $tlc,
                Num   $brr = $tlr.Num, Num   $brc = $tlc.Num) {
        my $row = RangeSet.new; $row.add ($tlr, $brr);
        my $col = RangeSet.new; $col.add ($tlc, $brc);
        @!cr.push: CellRange.new (:$row, :$col);
        }

    method in (Int:D $row, Int:D $col) returns Bool {
        #return @!cr.any (*.in ($row, $col));
        for @!cr -> $c {
            $c.in ($row, $col) and return True;
            }
        False;
        }

    method show {
        say "This CellSet contains there ranges:";
        .show for @!cr;
        say "-----------------------------------";
        }
    }

class CSV::Field {

    has Bool $.is_quoted  is rw is default(False);
    has Str  $.text       is rw;

    has Bool $!is_binary  is default(False);
    has Bool $!is_utf8    is default(False);
    has Bool $!is_missing is default(False);
    has Bool $!is_formula is default(False);
    has Bool $!analysed   is default(False);

    multi method new (Str(Cool) $str) {
        $str.defined ?? self.bless.add ($str) !! self.bless;
        }

    method Bool {
        $!text.defined && $!text ne "0" && $!text ne "";
        }

    method Str {
        # ?$!text ?? $!text !! $Text::CSV::undef_str
        $!text;
        }

    method chars {
        $!text.chars;
        }

    method Buf {
        Buf.new ($!text.encode ("utf8-c8").list);
        }

    method Numeric {
        $!text.defined
            ?? $!text ~~ m{^ <[0..9]> } ?? +$!text
            !!                              $!text.unival.Int
            !! Num;
        }

    method Real {
        $!text.defined
            ?? $!text ~~ m{^ <[0..9]> } ?? +$!text
            !!                              $!text.unival.Int
            !! Real;
        }

    method gist {
        $!text.defined or return "<undef>";
        $!analysed or self.analyse;
        my $s  = $!is_quoted  ?? "Q" !! "q";
           $s ~= $!is_binary  ?? "B" !! "b";
           $s ~= $!is_utf8    ?? "8" !! "7";
           $s ~= $!is_missing ?? "M" !! "m";
           $s ~= $!is_formula ?? "=" !! "-";
        $s ~ ":" ~ $!text.raku;
        }

    method add (Str $chunk) {
        $!text ~= $chunk;
        self;
        }

    method set_quoted () {
        $!is_quoted = True;
        $!text      = "";
        }

    method undefined () {
        !$!text.defined;
        }

    method analyse (Bool $force = !$!analysed) {
        $force or return;

        $!analysed = True;

        !$!text.defined || $!text eq "" and
            return; # Default is False for the rest

        $!text.starts-with ("=") and
            $!is_formula = True;

        $!text ~~ m:m{^ <[ \x09, \x20 .. \x7E ]>+ $} or
            $!is_binary  = True;

        $!text ~~ m:m{^ <[ \x00       .. \x7F ]>+ $} or
            $!is_utf8    = True;
        }

    method is_binary () returns Bool {
        $!analysed or self.analyse;
        $!is_binary;
        }

    method is_utf8 () returns Bool {
        $!analysed or self.analyse;
        $!is_utf8;
        }

    method is_missing () returns Bool {
        $!analysed or self.analyse;
        $!is_missing;
        }

    method is_formula () returns Bool {
        $!analysed or self.analyse;
        $!is_formula;
        }

    method is-quoted ()  returns Bool { $.is_quoted; }
    method is-binary ()  returns Bool { self.is_binary; }
    method is-utf8 ()    returns Bool { self.is_utf8; }
    method is-missing () returns Bool { self.is_missing; }
    method is-formula () returns Bool { self.is_formula; }

    method set-quoted ()              { self.set_quoted; }
    } # CSV::Field

class Text::CSV { ... }

class CSV::Row does Iterable does Positional does Associative {
    has Text::CSV  $.csv;
    has CSV::Field @.fields;

    multi method new (@f)  { @!fields = @f.map ( -> \x --> CSV::Field { CSV::Field.new (x) }); }

    method Str ()          { $!csv ?? $!csv.string (@!fields) !! Str; }
    method iterator ()     { @.fields.iterator; }
    method hash ()         { hash $!csv.column_names Z=> @!fields».Str; }
    method of ()           { CSV::Field; }
    method AT-KEY (Str $k) { %($!csv.column_names Z=> @!fields){$k}; }
    method strings ()      { @!fields».Str; }
    method AT-POS (int $i) { @!fields.AT-POS ($i); }
    method elems           { @!fields.elems; }
    method splice (*@args) { @!fields.splice (@args); }
    method keys ()         { $!csv.column_names; }

    method ASSIGN-POS (int $i, $v) {
        @!fields.AT-POS ($i).text = $v.Str;
        }
    method ASSIGN-KEY (Str:D $k, $v) {
        $!csv.column_names and
            %($!csv.column_names Z=> @!fields){$k}.text = $v.Str;
        }

    multi method push (CSV::Field $f) { @!fields.push: $f; }
    multi method push (Cool       $f) { @!fields.push: CSV::Field.new ($f); }
    multi method push (CSV::Row   $r) { @!fields.append: $r.fields; }

    method pop returns CSV::Field { @!fields.pop; }
    }

class Text::CSV {

    # Defaults are set in BUILD!
    has Str  $!eol;
    has Str  $!sep;
    has Str  $!quo;
    has Str  $!esc;

    has Bool $!binary;
    has Bool $!strict;
    has Bool $!decode_utf8;
    has Bool $!auto_diag;
    has Int  $!diag_verbose;
    has Bool $!keep_meta;
    has Int  $!skip_empty_rows;
    has Any  $!skip_empty_rows_cb;

    has Bool $!blank_is_undef;
    has Bool $!empty_is_undef;
    has Bool $!allow_whitespace;
    has Bool $!allow_loose_quotes;
    has Bool $!allow_loose_escapes;
    has Bool $!allow_unquoted_escape;

    has Bool $!always_quote;
    has Bool $!quote_space;
    has Bool $!escape_null;
    has Bool $!quote_empty;
    has Bool $!quote_binary;

    has Bool $!build;
    has Int  $!record_number;
    has Str  $!formula;
    has Str  $!undef_str;
    has Str  $!comment_str;

    has CSV::Row   $!csv-row;
    has Str        @!ahead;
    has Str        @!cnames;
    has IO::Handle $!io;
    has Bool       $!eof;
    has Int        @!types;
    has Int        @!crange;
    has RangeSet   $!rrange;
    has Int        $!strict_n;
    has            %!callbacks;

    has Int  $!errno;
    has Int  $!error_pos;
    has Int  $!error_field;
    has Str  $!error_input;
    has Str  $!error_message;

    class CSV::Diag does Iterable does Positional is Exception is export {
        has Int $.error   is readonly = 0;
        has Str $.message is readonly = %errors{$!error};
        has Int $.pos     is readonly;
        has Int $.field   is readonly;
        has Int $.record  is readonly;
        has Str $.buffer  is readonly;

        method sink {
            # See also src/core/Exception.pm - role X::Comp  method gist
            my $p = ($!pos    // "Unknown").Str;
            my $f = ($!field  // "Unknown").Str;
            my $r = ($!record // "Unknown").Str;
            my $m =
                 "\e[34m" ~ $!message
               ~ "\e[0m"  ~ " : error $!error @ record $r, field $f, position $p\n";
            $!buffer.defined && $!buffer.chars  && $!pos.defined &&
                $!pos >= 0 && $!pos < $!buffer.chars and $m ~=
                 "\e[32m" ~ substr ($!buffer, 0, $!pos - 1)
               ~ "\e[33m" ~ "\x[23CF]"
               ~ "\e[31m" ~ substr ($!buffer,    $!pos - 1)
               ~ "\e[0m\n";
            # I do not want the "in method sink at ..." here, but there
            # is no way yet to suppress that, so print instead of warn for now
            print $m;
            }
        method Numeric  { $!error; }
        method Str      { $!message; }
        method iterator { $[ $!error, $!message, $!pos, $!field, $!record, $!buffer ].iterator; }
        method hash     { { errno  => $!error,
                            error  => $!message,
                            pos    => $!pos,
                            field  => $!field,
                            recno  => $!record,
                            buffer => $!buffer,
                          }; }
#       method AT-KEY (Str $k) { } NYI
        method AT-POS (int $i) {
               $i == 0 ?? $!error
            !! $i == 1 ?? $!message
            !! $i == 2 ?? $!pos
            !! $i == 3 ?? $!field
            !! $i == 4 ?? $!record
            !! $i == 5 ?? $!buffer
            !! Any;
            }
        }

    # We need this to support aliasses and to catch unsupported attributes
    submethod BUILD (*%init) {
        # Defaults are disabled when BUILD is defined!

        $!sep                   = ',';
        $!quo                   = '"';
        $!esc                   = '"';

        $!binary                = True;
        $!decode_utf8           = True;
        $!auto_diag             = False;
        $!strict                = False;
        $!diag_verbose          = 0;
        $!keep_meta             = False;
        $!skip_empty_rows       = 0;
        $!skip_empty_rows_cb    = Any;

        $!blank_is_undef        = False;
        $!empty_is_undef        = False;
        $!allow_whitespace      = False;
        $!allow_loose_quotes    = False;
        $!allow_loose_escapes   = False;
        $!allow_unquoted_escape = False;

        $!always_quote          = False;
        $!quote_empty           = False;
        $!quote_space           = True;
        $!escape_null           = False;
        $!quote_binary          = True;
        $!formula               = "none";

        $!errno                 = 0;
        $!error_pos             = 0;
        $!error_field           = 0;
        $!error_input           = "";
        $!error_message         = "";
        $!record_number         = 0;

        $!io                    = IO::Handle;
        $!eof                   = False;

        $!csv-row               = CSV::Row.new (csv => self);

        self!set-attributes (%init);
        }

    method !set-attributes (%init) {
        $!build = True;
        for keys %init -> $attr {
            my @can = self.can (lc $attr) or self!fail (1000, "Unknown attribute '$attr'");
            .(self, %init{$attr}) for @can;
            }
        $!build = False;

        self!check_sanity;
        }

    method !fail (Int:D $errno, Int :$field, Str :$input, *@s) {
        $!errno          = $errno;
        $!error_pos      = 0;
        $!error_message  = %errors{$errno};
        $!error_message ~= (":", @s).join (" ") if @s.elems;
        $!error_field    = $field // ($!csv-row.fields.elems + 1);
        $!error_input    = $input;
        $!auto_diag and self.error_diag;    # Void context
        die self.error_diag;                # Exception object
        }

    method set_diag (Int:D $errno, Int :$pos, Int :$fieldno, Int :$recno) {
        $!errno         = $errno;
        $!error_message = %errors{$errno} // "";
        $!error_input   = "";

        $pos.defined     and $!error_pos     = $pos;
        $fieldno.defined and $!error_field   = $fieldno;
        $recno.defined   and $!record_number = $recno;
        }

    method !check_sanity () {
        $!build and return;

        #say "Sanity check: S:"~$!sep~" Q:"~($!quo//"<undef>")~" E:"~($!esc//"<undef>")~" WS:"~$!allow_whitespace;
        $!sep.defined                            or  self!fail (1008);
        $!sep eq ""                              and self!fail (1008);
        $!quo.defined and $!quo eq $!sep         and self!fail (1001);
        $!esc.defined and $!esc eq $!sep         and self!fail (1001);

                          $!sep ~~ m{<[\r\n]>}   and self!fail (1003);
        $!quo.defined and $!quo ~~ m{<[\r\n]>}   and self!fail (1003);
        $!esc.defined and $!esc ~~ m{<[\r\n]>}   and self!fail (1003);

        $!allow_whitespace or return;

                          $!sep ~~ m{ <[\ \t]> } and self!fail (1002);
        $!quo.defined and $!quo ~~ m{ <[\ \t]> } and self!fail (1002);
        $!esc.defined and $!esc ~~ m{ <[\ \t]> } and self!fail (1002);

        $!esc.defined or $!escape_null = False;
        }

    # String attributes
    method !a_str ($attr is rw, *@s) returns Str {
        if (@s.elems == 1) {
            my $x = @s[0];
            $x.defined && $x ~~ Str && $x eq "" and $x = Str;
            $attr = $x;
            self!check_sanity;
            }
        $attr;
        }
    method sep (*@s) returns Str { self!a_str ($!sep, @s); }
    method quo (*@s) returns Str { self!a_str ($!quo, @s); }
    method esc (*@s) returns Str { self!a_str ($!esc, @s); }
    method eol (*@s) returns Str {
        @s[0] ~~ Array and @s = |@s[0]; # Allow *OUT.nl-out
        self!a_str ($!eol, @s);
        }

    # Boolean attributes
    method !a_bool ($attr is rw, *@s) returns Bool {
        if (@s.elems == 1) {
            $attr = ?@s[0];
            self!check_sanity;
            }
        $attr;
        }
    method binary                (*@s) returns Bool { self!a_bool ($!binary,                @s); }
    method always_quote          (*@s) returns Bool { self!a_bool ($!always_quote,          @s); }
    method quote_empty           (*@s) returns Bool { self!a_bool ($!quote_empty,           @s); }
    method quote_space           (*@s) returns Bool { self!a_bool ($!quote_space,           @s); }
    method escape_null           (*@s) returns Bool { self!a_bool ($!escape_null,            @s); }
    method quote_binary          (*@s) returns Bool { self!a_bool ($!quote_binary,          @s); }
    method allow_loose_quotes    (*@s) returns Bool { self!a_bool ($!allow_loose_quotes,    @s); }
    method allow_loose_escapes   (*@s) returns Bool { self!a_bool ($!allow_loose_escapes,   @s); }
    method allow_unquoted_escape (*@s) returns Bool { self!a_bool ($!allow_unquoted_escape, @s); }
    method allow_whitespace      (*@s) returns Bool { self!a_bool ($!allow_whitespace,      @s); }
    method blank_is_undef        (*@s) returns Bool { self!a_bool ($!blank_is_undef,        @s); }
    method empty_is_undef        (*@s) returns Bool { self!a_bool ($!empty_is_undef,        @s); }
    method keep_meta             (*@s) returns Bool { self!a_bool ($!keep_meta,             @s); }
    method auto_diag             (*@s) returns Bool { self!a_bool ($!auto_diag,             @s); }
    method strict                (*@s) returns Bool { self!a_bool ($!strict,                @s); }
    method eof                   ()    returns Bool { $!eof || $!errno == 2012; }

    # Numeric attributes
    method !a_num ($attr is rw, *@s) returns Int {
        @s.elems == 1 and $attr = +@s[0];
        $attr;
        }
    method record_number (*@s) returns Int { self!a_num ($!record_number, @s); }

    # Numeric attributes, boolean allowed
    method !a_bool_int ($attr is rw, *@s) returns Int {
        if (@s.elems == 1) {
            my $v = @s[0];
            $attr = $v ~~ Bool ?? +$v !! $v.defined ?? $v eq "" ?? 0 !! +$v !! 0;
            }
        $attr;
        }
    method diag_verbose (*@s) returns Int { self!a_bool_int ($!diag_verbose, @s); }

    method header (IO::Handle:D $fh,
                   Array       :$sep-set            = [< , ; >],
                   Any         :$munge-column-names = "fc",
                   Bool        :$set-column-names   = True,
                   Bool        :$detect-bom         = True) { # unused for now
        my Str $hdr = $fh.get           or  self!fail (1010);

        # Determine separator conflicts
        my %sep = $hdr.comb.Bag{$sep-set.list}:kv;
        %sep.elems > 1 and self!fail (1011);

        self.sep (%sep.keys);

        given $munge-column-names {
            when Callable | "none" {             }
            when "lc"              { $hdr .= lc; }
            when "uc"              { $hdr .= uc; }
            default                { $hdr .= fc; }
            }

        self.getline ($hdr)             or  self!fail ($!errno);
        my @row = self.strings          or  self!fail (1010);
        $munge-column-names ~~ Callable and @row = @row.map ($munge-column-names);
        @row.grep ("")                  and self!fail (1012);
        if (my @dup = @row.repeated) {      self!fail (1013, @dup.Bag.map ({ "{$_.key}({$_.value+1})" }).join: ", "); }
        $set-column-names               and self.column-names: @row;
        self;
        }

    multi method formula ()         returns Str { $!formula;           };
    multi method formula (Str:U)    returns Str { $!formula = "undef"; };
    multi method formula (Str:D $x) returns Str {
        my Str $f = $x.lc;
        $f eq "" and $f = "empty";
        $f ~~ "none" | "die" | "croak" | "diag" | "empty" | "undef" or self!fail (1500);
        $!formula = $f;
        };

    multi method skip_empty_rows ()          returns Int { $!skip_empty_rows;              };
    multi method skip_empty_rows (Any:U)     returns Int { self.skip_empty_rows ("false"); };
    multi method skip_empty_rows (Bool  $x)  returns Int { self.skip_empty_rows (lc $x);   };
    multi method skip_empty_rows (Int:D $x
                          where 0 <= * <= 5) returns Int { self.skip_empty_rows (~$x);       };
    multi method skip_empty_rows (Callable:D $x) returns Int { $!skip_empty_rows_cb = $x; return $!skip_empty_rows = 6; }
    multi method skip_empty_rows (Routine:D  $x) returns Int { $!skip_empty_rows_cb = $x; return $!skip_empty_rows = 6; }
    multi method skip_empty_rows (Str:D  $x) returns Int {
        my Str $f = $x.lc;
        $!skip_empty_rows_cb = Any;
        $f ~~ "0" | ""     | "false" and return $!skip_empty_rows = 0;
        $f ~~ "1" | "skip" | "true"  and return $!skip_empty_rows = 1;
        $f ~~ "2" | "stop" | "eof"   and return $!skip_empty_rows = 2;
        $f ~~ "3" | "die"            and return $!skip_empty_rows = 3;
        $f ~~ "4" | "croak"          and return $!skip_empty_rows = 4;
        $f ~~ "5" | "error"          and return $!skip_empty_rows = 5;
        self!fail (1500);
        };

    multi method undef_str ()           returns Str { $!undef_str;         };
    multi method undef_str (Any:U)      returns Str { $!undef_str = Str;   };
    multi method undef_str (Str:D $x)   returns Str { $!undef_str = $x     };

    multi method comment_str ()         returns Str { $!comment_str;       };
    multi method comment_str (Any:U)    returns Str { $!comment_str = Str; };
    multi method comment_str (Str:D $x) returns Str { $!comment_str = $x   };

    multi method column_names (Bool:D $ where *.not) returns Array[Str] { @!cnames = (); }
    multi method column_names (Any:U)                returns Array[Str] { @!cnames = (); }
    multi method column_names (0)                    returns Array[Str] { @!cnames = (); }
    multi method column_names (*@c)                  returns Array[Str] {
        @c.elems and @!cnames = @c.map (*.Str);
        @!cnames;
        }

    my %predef-hooks =
        not_blank => { $^row.elems > 1 or $^row[0].defined && $^row[0] ne "" or $^row[0].is-quoted },
        not_empty => { $^row.first: { .defined && $_ ne ""   }},
        filled    => { $^row.first: { .defined && $_ ~~ /\S/ }};
    %predef-hooks<not-blank> = %predef-hooks<not_blank>;
    %predef-hooks<not-empty> = %predef-hooks<not_empty>;

    method callbacks (*@cb is copy) {
        if (@cb.elems == 1) {
            my $b = @cb[0];
            if ($b ~~ Hash || $b ~~ Pair) {
                @cb = $b.kv;
                }
            else {
                !$b.defined or
                   ($b ~~ Bool || $b ~~ Int and !?$b) or
                   ($b ~~ Str  && $b ~~ m:i/^( reset | clear | none | 0 )$/) or
                    self!fail (1004);
                %!callbacks = %();
                @cb = ();
                }
            }
        if (@cb.elems % 2) {
            self!fail (1004);
            }
        if (@cb.elems) {
            my %hooks;
            for @cb -> $name, $callback {
                my $hook = $callback;
                $hook.defined && $hook ~~ Str && %predef-hooks{$hook}.defined and
                    $hook = %predef-hooks{$hook};

                $name.defined &&  $name ~~ Str     &&
                $hook.defined && ($hook ~~ Routine || $hook ~~ Callable) or
                    self!fail (1004);

                my Str $cb_name = $name;
                $cb_name ~~ s{"-"} = "_";
                $cb_name ~~ /^ after_parse
                             | before_print
                             | filter
                             | error
                             $/ or self!fail (3100, $name);
                %hooks{$cb_name} = $hook;
                }
            %!callbacks = %hooks;
            }
        %!callbacks;
        }

    method !rfc7111ranges (Str:D $spec) returns RangeSet {
        $spec eq "" and self!fail (2013);
        my RangeSet $range = RangeSet.new;
        for $spec.split (";") -> $r {
            $r ~~ /^ (<[0..9]>+)["-"[(<[0..9]>+)||("*")]]? $/ or self!fail (2013);
            my Int $from = +$0;
                   $from ==  0  and self!fail (2013);
            my Str $tos  = ($1 // $from).Str;
                   $tos  eq "0" and self!fail (2013);
            my Num $to   = $tos eq "*" ?? Inf !! (+$tos - 1).Num;
            --$from <= $to or self!fail (2013);
            $range.add ($from, $to);
            }
        $range;
        }

    multi method colrange (@cr) {
        if (@cr.elems) {
            my RangeSet $range = RangeSet.new;
            $range.add ($_) for @cr;
            @!crange = $range.to_list;
            }
        @!crange;
        }

    multi method colrange (Str $range) {
        @!crange = ();
        $range.defined and @!crange = self!rfc7111ranges ($range).to_list.flat;
        @!crange;
        }

    multi method colrange () {
        @!crange;
        }

    multi method rowrange (Str $range) returns RangeSet {
        $!rrange = RangeSet;
        $range.defined and $!rrange = self!rfc7111ranges ($range);
        $!rrange;
        }

    multi method rowrange () returns RangeSet {
        $!rrange;
        }

    method version () returns Str {
        $VERSION;
        }

    method status () returns Bool {
        !?$!errno;
        }

    method error_input () returns Str {
        $!errno ?? $!error_input !! Str
        }

    method error_diag () returns CSV::Diag {
        CSV::Diag.new (
            error   => $!errno,
            message => $!error_message,
            pos     => $!error_pos,
            field   => $!error_field,
            record  => $!record_number,
            buffer  => $!error_input // "", # // for 2012
            );
        }

    method is_quoted  (Int:D $i) returns Bool {
        $i >= 0 && $i < $!csv-row.fields.elems && $!csv-row.fields[$i].is_quoted;
        }

    method is_binary  (Int:D $i) returns Bool {
        $i >= 0 && $i < $!csv-row.fields.elems && $!csv-row.fields[$i].is_binary;
        }

    method is_utf8    (Int:D $i) returns Bool {
        $i >= 0 && $i < $!csv-row.fields.elems && $!csv-row.fields[$i].is_utf8;
        }

    method is_missing (Int:D $i) returns Bool {
        $i >= 0 && $i < $!csv-row.fields.elems && $!csv-row.fields[$i].is_missing;
        }

    method !accept-field (CSV::Field $f) returns Bool {
        $!csv-row.push: $f;
        True;
        }

    # combine : direction = 0
    # parse   : direction = 1
    method !ready (Int:D $direction, CSV::Field $f) returns Bool {

        if ($f.undefined) {
            if ($direction) {
                $!blank_is_undef || $!empty_is_undef or $f.add ("");
                }
            #return self!accept-field ($f);
            }

        elsif ($f.Str eq "") {
            $!empty_is_undef and $f.text = Str;
            #return self!accept-field ($f);
            }

        elsif ($!formula ne "none" && $f.is-formula) {
            given $!formula {
                when "die"   { die "Formulas are forbidden"; }
                when "croak" { die "Formulas are forbidden"; }
                when "empty" { $f.text = "";                 }
                when "undef" { $f.text = Str;                }
                when "diag"  {
                    my $fnum = $!csv-row.fields.elems + 1;
                    @!cnames.elems and $fnum ~= " (column: '@!cnames[$fnum - 1]')";
                    my $recn = $direction ?? " in record $!record_number" !! "";
                    warn "Field $fnum$recn contains formula '$f'\n";
                    }
                }
            $f.analyse (True);
            }

        # Postpone all other field attributes like is_binary and is_utf8
        # till it is actually asked for unless it is required right now
        # to fail
        elsif (!$!binary and $f.Str ~~ m{ <[ \x00..\x08 \x0A..\x1F ]> }) {
            $!error_pos     = $/.from + 1 + $f.is_quoted;
            $!errno         = $f.is_quoted ??
                 $f.Str ~~ m{<[ \r ]>} ?? 2022 !!
                 $f.Str ~~ m{<[ \n ]>} ?? 2021 !!  2026 !! 2037;
            $!error_field   = $!csv-row.fields.elems + 1;
            $!error_message = %errors{$!errno};
            $!error_input   = $f.Str;
            $!auto_diag and self.error_diag;
            return False;
            }

        self!accept-field ($f);
        } # ready

    method fields () {
        @!crange ?? ($!csv-row.fields[@!crange]:v) !! $!csv-row.fields;
        }

    method strings () {
        self.fields».Str.Array;
        }

    multi method string () returns Str {
        #progress (0, $!csv-row.fields);
        self.string ($!csv-row.fields);
        }

    multi method string (CSV::Field:D @fld) returns Str {

        %!callbacks<before_print>.defined and
            %!callbacks<before_print>.($!csv-row);

        @fld or return Str;
        my Str $s = $!sep;
        my Str $q = $!quo;
        my Str $e = $!esc;
        my Str @f;
        for @fld -> $f {
            if (!$f.defined || $f.undefined) {
                @f.push: "";
                next;
                }
            my Str $t = $f.Str;
            if ($t eq "") {
                @f.push: $!always_quote || $!quote_empty ?? "$!quo$!quo" !! "";
                next;
                }
            $t.subst-mutate (/( $q | $e )/, { "$e$0" }, :g);
            $t.subst-mutate (/ \x[0] /,     { $e ~ 0 }, :g) if $!escape_null;
            $!always_quote
            ||                    $t ~~ / $e  | $s | \r | \n /
            || ($!quote_space  && $t ~~ / " " | \t /)
            || ($!quote_binary && $t ~~ / <[ \x00..\x08 \x0a..\x1f \x7f..\xa0 ]> /)
                and $t = "$q$t$q";
            @f.push: $t;
            }
        #progress (0, @f);
        #progress (1, my $x =
        defined ($!eol)
          ?? join ( $!sep, @f ) ~ $!eol
          !! join ( $!sep, @f )
        #); $x
        } # string

    multi method combine (Capture $c) returns Bool {
        self.combine ($c.list);
        }
    multi method combine (*@f) returns Bool {
        self.combine (@f);
        }
    multi method combine (@f) returns Bool {
        $!csv-row.fields = ();
        my int $i = 0;
        for flat @f -> $f {
            $i = $i + 1;
            my CSV::Field $cf;
            if ($f.isa (CSV::Field)) {
                $cf = $f;
                }
            else {
                $cf = CSV::Field.new;
                defined $f and $cf.add ($f.Str);
                }
            unless (self!ready (0, $cf)) {
                $!errno       = 2110;
                $!error_input = $f.Str;
                $!error_field = $i;
                return False;
                }
            }
        True;
        }

    method parse (Str $buffer) returns Bool {

        my int $skip = 0;
        my int $pos  = 0;
        my int $ppos = 0;

        $!errno = 0;
        my sub parse_error (Int $errno, Int :$fpos) {
            $!errno         = $errno;
            $!error_pos     = $fpos // $pos;
            $!error_field   = $!csv-row.fields.elems + 1;
            $!error_message = %errors{$errno};
            $!error_input   = $buffer;
            $!eof           = $errno == 2012;
            $!auto_diag && !($!io && $!eof) and self.error_diag;
            False;
            }

        my sub chunks (Str $str, @re) {
            $str.defined
              ?? $str.chars
                ?? $str.split (@re, :v, :skip-empty) # :ignoremark
                !! ("")
              !! ()
            }

        $!record_number = $!record_number + 1;
        $opt_v > 4 and progress ($!record_number, $buffer.raku);

        my CSV::Field $f   = CSV::Field.new;
        my            $eol = $!eol // rx{ \r\n || \r || \n };
        my Str        $sep = $!sep;
        my Str        $quo = $!quo;
        my Str        $esc = $!esc;
        my Bool       $noe = !$esc.defined || ($quo.defined && $esc eq $quo);
        my            @eox = $!eol.defined ?? $eol !! ("\r\n", "\r", "\n");
        my            @chx = @eox;
        defined $sep and @chx.push: $sep;
        defined $quo and @chx.push: $quo;
        $noe         or  @chx.push: $esc;

        $!csv-row.fields = ();

        my sub keep () returns Bool {
            self!ready (1, $f) ?? !($f = CSV::Field.new) !! False;
            } # add

        my sub parse_done () {
            self!ready (1, $f) or return False;
            .($!csv-row) with %!callbacks<after_parse>;
            if ($!strict) {
                $!strict_n //= $!csv-row.fields.elems;
                $!strict_n  == $!csv-row.fields.elems or
                    return parse_error (2014);
                }
            True;
            }

        my @ch;
        $!io and @ch = @!ahead;
        @!ahead = ();
        if ($buffer.defined) {
            $!comment_str && $!csv-row.fields.elems == 0
                && $buffer.starts-with ($!comment_str) and
                    return self.parse ($!io.get // Str);
            @ch.append: chunks ($buffer, @chx);
            }
        @ch or return parse_error (2012);

        $opt_v > 2 and progress (0, @ch.raku);

        if ($!skip_empty_rows and
                @ch.elems == 0 ||
               (@ch.elems == 1 && @ch[0] eq @eox.any)) {
            $!skip_empty_rows == 1 and return self.parse ($!io.get // Str);
            $!skip_empty_rows == 2 and return False;
            $!skip_empty_rows == 3 and die "Empty row";
            $!skip_empty_rows == 4 and die "Empty row";
            $!skip_empty_rows == 5 and return parse_error (2015);
            $!skip_empty_rows == 6 and @ch = chunks ($!skip_empty_rows_cb.(self), @chx);
            }

        @ch.elems or return parse_done ();       # An empty line

        loop {
            # faster than @ch.kv, faster than my int $i
            loop (my Int $i = 0; $i < @ch.elems; $i = $i + 1) {
                my Str $chunk := @ch[$i];
                $ppos += $chunk.chars;

                if ($skip) {
                    $skip--;
                    next;
                    }

                $pos = $ppos;

                $opt_v > 8 and progress ($i, "###", $chunk.raku~"\t", $f.gist);

                if ($chunk eq $sep) {
                    $opt_v > 5 and progress ($i, "SEP - " ~ $f.gist);

                    # ,1,"foo, 3",,bar,
                    # ^           ^
                    if ($f.undefined) {
                        $!blank_is_undef || $!empty_is_undef or
                            $f.add ("");
                        keep () or return False;
                        next;
                        }

                    # ,1,"foo, 3",,bar,
                    #        ^
                    if ($f.is_quoted) {
                        $opt_v > 9 and progress ($i, "    inside quoted field ", @ch[$i..*-1].raku);
                        $f.add ($chunk);
                        next;
                        }

                    # ,1 ,"foo, 3"  ,,bar ,
                    #    ^          ^     ^
                    $!allow_whitespace && !$f.undefined and $f.text ~~ s{ <[\ \t]>+ $} = "";

                    # ,1,"foo, 3",,bar,
                    #   ^        ^    ^
                    keep () or return False;
                    next;
                    }

                if ($quo.defined and $chunk eq $quo) {
                    $opt_v > 5 and progress ($i, "QUO -" ~ $f.gist);

                    # ,1,"foo, 3",,bar,\r\n
                    #    ^
                    if ($f.undefined) {
                        $opt_v > 9 and progress ($i, "    initial quote");
                        $f.set_quoted;
                        next;
                        }

                    if ($f.is_quoted) {

                        $opt_v > 9 and progress ($i, "    inside quoted field ", @ch[$i..*-1].raku);
                        # ,1,"foo, 3"
                        #           ^
                        $i == @ch - 1 and return parse_done ();

                        my Str $next   = @ch[$i + 1];
                        my int $omit   = 1;
                        my int $quoesc = 0;

                        # , 1 , "foo, 3" , , bar , "" \r\n
                        #               ^            ^
                        if ($!allow_whitespace && $next ~~ /^ <[\ \t]>+ $/) {
                            $i == @ch - 2 and return parse_done ();
                            $next = @ch[$i + 2];
                            $omit = $omit + 1;
                            }

                        $opt_v > 8 and progress ($i, "QUO", "next = $next");

                        # ,1,"foo, 3",,bar,\r\n
                        #           ^
                        if ($next eq $sep) {
                            $opt_v > 7 and progress ($i, "SEP");
                            $skip = $omit;
                            keep () or return False;
                            next;
                            }

                        # ,1,"foo, 3"\r\n
                        #           ^
                        # $next ~~ /^ $eol $/ and return parse_done ();
                        if ($!eol.defined
                                ?? $next eq $!eol
                                !! ( $next eq "\r\n"
                                  || $next eq "\n"
                                  || $next eq "\r")) {
                            return parse_done ();
                            }

                        if (defined $esc and $esc eq $quo) {
                            $opt_v > 7 and progress ($i, "ESC", "($next)");

                            $quoesc = 1;

                            # ,1,"foo, 3"056",,bar,\r\n
                            #            ^
                            if ($next.starts-with ("0")) {
                                @ch[$i + 1] ~~ s{^ "0"} = "";
                                $ppos = $ppos + 1;
                                $opt_v > 8 and progress ($i, "Add NIL");
                                $f.add ("\c0");
                                next;
                                }

                            # ,1,"foo, 3""56",,bar,\r\n
                            #            ^
                            if ($next eq $quo) {
                                $skip = $omit;
                                $f.add ($chunk);
                                next;
                                }

                            if ($!allow_loose_escapes) {
                                # ,1,"foo, 3"56",,bar,\r\n
                                #            ^
                                next;
                                }
                            }

                        # No need to special-case \r

                        if ($quoesc == 1) {
                            # 1,"foo" ",3
                            #        ^
                            return parse_error (2023);
                            }

                        if ($!allow_loose_quotes) {
                            # ,1,"foo, 3"456",,bar,\r\n
                            #            ^
                            $f.add ($chunk);
                            next;
                            }

                        # Keep rest of @ch for hooks?
                        return parse_error (2011);
                        }

                    # 1,foo "boo" d'uh,1
                    #       ^
                    if ($!allow_loose_quotes) {
                        $f.add ($chunk);
                        next;
                        }
                    return parse_error (2034);
                    }

                if ($esc.defined and $chunk eq $esc) {
                    $opt_v > 5 and progress ($i, "ESC - " ~ $f.gist);

                    if ($i >= $@ch.elems - 1) {
                        if ($!allow_loose_escapes) {
                            $f.add ($chunk);
                            next;
                            }
                        return parse_error ($f.is_quoted ?? 2024 !! 2035);
                        }

                    my $next := @ch[$i + 1];

                    # ,1,"foo, 3\056",,bar,\r\n
                    #            ^
                    if ($next.starts-with ("0")) {
                        @ch[$i + 1] ~~ s{^ "0"} = "";
                        $ppos = $ppos + 1;
                        $opt_v > 8 and progress ($i, "Add NIL");
                        $f.add ("\c0");
                        next;
                        }

                    # ,1,"foo, 3\"56",,bar,\r\n
                    #            ^
                    if ($next eq $quo) {
                        $skip = 1;
                        $f.add ($quo);
                        next;
                        }

                    # ,1,"foo, 3\\56",,bar,\r\n
                    #            ^
                    if ($next eq $esc) {
                        $skip = 1;
                        $f.add ($esc);
                        next;
                        }

                    if ($!allow_loose_escapes) {
                        # ,1,"foo, 3\56",,bar,\r\n
                        #            ^
                        next;
                        }

                    return parse_error (2025);
                    }

                #if ($chunk ~~ rx{^ $eol $}) {
                if ($!eol.defined
                        ??  $chunk eq $!eol
                        !! ($chunk eq "\r\n" || $chunk eq "\n" || $chunk eq "\r")) {
                    $opt_v > 5 and progress ($i, "EOL - " ~ $f.gist);
                    if ($f.is_quoted) {     # 1,"2\n3"
                        $!binary or
                            return parse_error (
                                $!eol.defined       ?? 2021 !!
                                $chunk ~~ m{<[\r]>} ?? 2022 !!
                                $chunk ~~ m{<[\n]>} ?? 2021 !!  2026);

                        $f.add ($chunk);

                        if ($i == @ch.elems - 1 && $!io.defined) {
                            my $str := $!io.get or return parse_error (2012);
                            @ch.append: chunks ($str, @chx);
                            }

                        next;
                        }

                    # ,1,"foo, 3",,bar
                    #                    ^
                    $!allow_whitespace && !$f.undefined and $f.text ~~ s{ <[\ \t]>+ $} = "";

                    $!io.defined and @!ahead = @ch[($i + 1) .. *];

                    # sep=;
                    if ($!record_number == 1 && $!io.defined && $!csv-row.fields.elems == 0 &&
                            !$f.undefined && $f.Str ~~ /^ "sep=" (.*) /) {
                        $!sep = $0.Str;
                        $!record_number = 0;
                        return self.parse ($!io.get // Str);
                        }

                    return parse_done ();
                    }

                # 1,foo,  bar,4
                #       ^
                if ($!allow_whitespace) {
                    $f.undefined        and $chunk ~~ s{^ <[\ \t]>+  } = "";
                    $i + 1 == @ch.elems and $chunk ~~ s{  <[\ \t]>+ $} = "";
                    }

                unless ($!binary) {
                    $opt_v > 5 and progress ($i, "data - check binary");
                    my $fpos = $pos - $chunk.chars + 1;
                    if ($f.is_quoted) {
                        $chunk ~~ m/  \r / and return parse_error (2022, fpos => $fpos + $/.from);
                        $chunk ~~ m/  \n / and return parse_error (2021, fpos => $fpos + $/.from);
                        }
                    else {
                        $chunk ~~ m/^ \r / and return parse_error (2031, fpos => $fpos);
                        $chunk ~~ m/  \r / and return parse_error (2032, fpos => $fpos + $/.from);
                        }
                    }
                $chunk ne "" and $f.add ($chunk);
                }

            $f.is_quoted       or last;
            $!io.defined       or return parse_error (2027);

            my $str = $!io.get;
            unless ($str.defined) {
                parse_error (2027);
                $!eof = True;
                return False;
                }

            @ch = chunks ($str, @chx);

            $i = 0;
            };

#       !$!binary && $f.is_binary and
#           return parse_error ($f.is_quoted ?? 2026 !! 2037);

        parse_done ();
        } # parse

    method row returns CSV::Row { $!csv-row; }

    method !row_hr (@row) {
        my @cn  = (@!crange ?? @!cnames[@!crange] !! @!cnames);
        hash @cn Z=> @row;
        }

    multi method getline_hr (Str $str, Bool :$meta = $!keep_meta) {
        @!cnames.elems or self!fail (3002);
        self!row_hr (self.getline ($str, :$meta));
        } # getline_hr

    multi method getline_hr (IO::Handle:D $io, Bool :$meta = $!keep_meta) {
        @!cnames.elems or self!fail (3002);
        self!row_hr (self.getline ($io,  :$meta));
        } # getline_hr

    multi method getline (Str $str, Bool :$meta = $!keep_meta) {
        self.parse ($str)
          ?? $meta
            ?? self.fields
            !! self.strings
          !! Nil;
        } # getline

    multi method getline (IO::Handle:D $io, Bool :$meta = $!keep_meta) {
        my Bool $chomped = $io.chomp;
        my $nl    = $io.nl-in;
        $!eol.defined and $io.nl-in = $!eol;
        $io.chomp = False;
        $!io      = $io;
        my Bool $status  = self.parse ($io.get // Str);
        $!io      = IO::Handle;
        $io.nl-in = $nl;
        $io.chomp = $chomped;
        $status ?? $meta ?? self.fields !! self.strings !! ();
        } # getline

    method getline_hr_all (IO::Handle:D  $io,
                           Int   $offset =  0,
                           Int   $length = -1,
                           Bool :$meta   = False) {
        @!cnames.elems or self!fail (3002);
        self.getline_all ($io, $offset, $length, :$meta, :hr);
        }

    method !row (Bool:D $meta, Bool:D $hr) {
        my @row = $meta ?? self.fields !! self.strings;
        $hr or return $[ @row ];

        my @cn = (@!crange ?? @!cnames[@!crange] !! @!cnames);
        my %hash = @cn Z=> @row;
        $%hash;
        }

    # @a = $csv.getline_all ($io);
    # @a = $csv.getline_all ($io, $offset);
    # @a = $csv.getline_all ($io, $offset, $length);
    method getline_all (IO::Handle:D  $io,
                        Int           $offset is copy =  0,
                        Int           $length is copy = -1,
                        Bool         :$meta = $!keep_meta,
                        Bool         :$hr   = False) {
        my Bool $chomped = $io.chomp;
        my $nl           = $io.nl-in;
        $!eol.defined  and $io.nl-in = $!eol;
        $io.chomp        = False;
        $!io             = $io;
        $!record_number  = 0;

        my @lines;
        if ($offset >= 0) {
            while (self.parse ($io.get // Str)) {
                !$!rrange || $!rrange.in ($!record_number - 1) or next;

                $offset--  > 0 and next;
                $length-- == 0 and last;

                !%!callbacks<filter>.defined ||
                    %!callbacks<filter>.($!csv-row) or next;

                @lines.push: self!row ($meta, $hr);
                }
            }
        else {
            $offset = -$offset;
            while (self.parse ($io.get // Str)) {
                !%!callbacks<filter>.defined ||
                    %!callbacks<filter>.($!csv-row) or next;

                @lines.elems == $offset and @lines.shift;
                @lines.push: self!row ($meta, $hr);
                }
            $length >= 0 && @lines.elems > $length and @lines.splice ($length);
            }

        $!io      = IO::Handle;
        $io.nl-in = $nl;
        $io.chomp = $chomped;
        @lines;
        }

    method fragment (IO::Handle:D $io, Str $spec is copy, Bool :$meta = $!keep_meta) {

        $spec.defined && $spec.chars or self!fail (2013);

        self.rowrange (Str);
        self.colrange (Str);

        if ($spec ~~ s:i{^ "row=" } = "") {
            self.rowrange ($spec);
            return @!cnames.elems ?? self.getline_hr_all ($io, :$meta)
                                  !! self.getline_all    ($io, :$meta);
            }

        if ($spec ~~ s:i{^ "col=" } = "") {
            self.colrange ($spec);
            return @!cnames.elems ?? self.getline_hr_all ($io, :$meta)
                                  !! self.getline_all    ($io, :$meta);
            }

        $spec ~~ s:i{^ "cell=" } = "" or self!fail (2013);

        my CellSet $cs = CellSet.new;
        # cell=1,1-2,2;3,3-4,4;1,4;4,1
        for $spec.split (";") -> $r {
            $r ~~ /^     (<[0..9]>+)         ","  (<[0..9]>+)
                   ["-" [(<[0..9]>+)||("*")] "," [(<[0..9]>+)||("*") ]]?
                   $/ or self!fail (2013);
            my Int $tlr = +$0;
                   $tlr ==  0  and self!fail (2013);
            my Int $tlc = +$1;
                   $tlc ==  0  and self!fail (2013);
            my Str $Brr = ($2 // $tlr).Str;
            my Str $Brc = ($3 // $tlc).Str;
            my Num $brr = $Brr eq "*" ?? Inf !! (+$Brr).Num;
            my Num $brc = $Brc eq "*" ?? Inf !! (+$Brc).Num;
            $tlr <= $brr && --$tlc <= --$brc or self!fail (2013);
            $cs.add ($tlr, $tlc, $brr, $brc);
            }

        my Bool $chomped = $io.chomp;
        my $nl           = $io.nl-in;
        $!eol.defined  and $io.nl-in = $!eol;
        $io.chomp        = False;
        $!io             = $io;
        $!record_number  = 0;

        my @lines;
        while (self.parse ($io.get // Str)) {

            my CSV::Field @f = $!csv-row.fields[
                (^($!csv-row.fields.elems)).grep ({
                    $cs.in ($!record_number, $_) })] or next;

            !%!callbacks<filter>.defined ||
                %!callbacks<filter>.(CSV::Row.new (csv => self, @f)) or next;

            my @row = $meta ?? @f !! @f.map ( -> \x --> Str { x.Str });
            if (@!cnames.elems) {
                my %h = @!cnames Z=> @row;
                @lines.push: $%h;
                next;
                }
            @lines.push: $[ @row ];
            }

        $!io      = IO::Handle;
        $io.nl-in = $nl;
        $io.chomp = $chomped;
        @lines;
        }

    multi method print (IO::Handle:D $io, *%p) returns Bool {
        @!cnames.elems or self!fail (3009);
        self.print ($io, %p{@!cnames});
        }

    multi method print (IO::Handle:D $io,  %c) returns Bool {
        @!cnames.elems or self!fail (3009);
        self.print ($io, [ %c{@!cnames} ]);
        }

    multi method print (IO::Handle:D $io, Capture $c) returns Bool {
        self.print ($io, $c.list);
        }

    multi method print (IO::Handle:D $io,  @fld) returns Bool {
        self.combine (@fld) or return False;
        my $nl = $io.nl-out;
        $!eol.defined and $io.nl-out = $!eol;
        $io.print (self.string);
        $io.nl-out = $nl;
        True;
        }

    multi method print (IO::Handle:D $io, *@fld) returns Bool {
        # Temp guard ... I expected *%p sig to be called
        if (@fld[0] ~~ Pair) {
            @!cnames.elems or self!fail (3009);
            my %hash;
            for @fld -> $kv {
                %hash{$kv.key} = $kv.value;
                }
            return self.print ($io, [ %hash{@!cnames} ]);
            }
        self.print ($io, [@fld]);
        }

    method say (IO::Handle:D $io, |f) returns Bool {
        my $eol = $!eol;
        $!eol ||= "\r\n";
        my Bool $state = self.print ($io, |f);
        $!eol = $eol;
        $state;
        }

    # Only as a method, both in and out are required
    method CSV ( Any    :$in!,
                 Any    :$out!     is copy,
                 Any    :$headers  is copy,
                 Str    :$encoding is copy,
                 Str    :$fragment is copy,
                 Bool   :$strict = False,
                 Bool   :$meta   = False,
                        :@kh,
                 *%args) {

        # Aliasses
        #   frag   fragment
        #   enc    encoding
        %args<frag>.defined and $fragment ||= %args<frag> :delete;
        %args<enc >.defined and $encoding ||= %args<enc>  :delete;
        %args<enc >.defined and $encoding ||= %args<enc>  :delete;
        $fragment //= "";

        if (@kh.VAR.name ne "element") { # @kh = "@kh" or [] = "element"
            # https://github.com/rakudo/rakudo/issues/2483
            $out     //= Hash if $out === Any;
            $headers //= "auto";
            }

        # Indirect options to invoke headers
        if ($encoding.defined && $encoding eq "auto") {
            %args<detect-bom> = True;
            $encoding = Str;
            }
        if ($headers ~~ Bool && ?$headers) {
            %args<detect-bom> //= True;
            $headers = Any;
            }

        my $skip = %args<skip> :delete || 0 and
            self.rowrange (++$skip ~ "-*");

        # Check csv-only args
        # Hooks
        #   after_in    after-in    after_parse  after-parse
        #   before_out  before-out
        #   filter
        #   error
        #   on_in       on-in
        my Callable $on-in;
        my Callable $before-out;
        my %hooks;
        my %hdrargs;
        if (my $c = %args<callbacks> :delete) {
            if ($c.defined) {
                $c ~~ Hash or self!fail (1004);
                %args{$c.keys} = $c.values;
                }
            else {
                self.callbacks ($c);
                }
            }
        for (%args.keys) -> $k {
            if ($k.lc ~~ m{^ "on"     <[-_]>   "in"             $}) {
                $on-in               = %args{$k} :delete;
                next;
                }
            if ($k.lc ~~ m{^ "before" <[-_]>   "out"            $}) {
                $before-out          = %args{$k} :delete;
                next;
                }
            if ($k.lc ~~ m{^ "after"  <[-_]> ( "parse" | "in" ) $}) {
                %hooks<after_parse>  = %args{$k} :delete;
                next;
                }
            if ($k.lc ~~ m{^ "before" <[-_]>   "print"          $}) {
                %hooks<before_print> = %args{$k} :delete;
                next;
                }
            if ($k.lc eq "filter") {
                %hooks<filter>       = %args{$k} :delete;
                if (%hooks<filter> &&
                    %hooks<filter>.signature.params.head.sigil eq "%") {
                    $out     //= Hash if $out === Any;
                    $headers //= "auto";
                    }
                next;
                }
            if ($k.lc eq "error") {
                %hooks<error>        = %args{$k} :delete;
                next;
                }

            if ($k.lc ~~ m{^ [ "detect" <[-_]> ]? "bom"         $}) {
                %hdrargs<detect-bom>         = %args{$k} :delete;
                next;
                }
            if ($k.lc ~~ m{^ "sep" [ <[-_]> "set" || "s" ]      $}) {
                %hdrargs<sep-set>            = %args{$k} :delete;
                next;
                }
            if ($k.lc eq "munge") {
                %hdrargs<munge-column-names> = %args{$k} :delete;
                next;
                }
            if ($k.lc ~~ m{^ "munge" [ <[-_]> "column" <[-_]> "names" ]? $}) {
                %hdrargs<munge-column-names> = %args{$k} :delete;
                next;
                }
            if ($k.lc ~~ m{^ "set" <[-_]> "column" <[-_]> "names" $}) {
                %hdrargs<set-column-names>   = %args{$k} :delete;
                next;
                }
            }
        if ($on-in && !%hooks<after_parse>) {
            %hooks<after_parse> = $on-in;
            $on-in = Callable;
            }
        %hooks.keys and self.callbacks (|%hooks.list);

        my $key = %args<key> :delete;
        if ($key.defined) {
            $out     //= Hash if $out === Any;
            $headers //= "auto";
            }

        # Rest is for Text::CSV
        %args<strict>    = $strict;
        %args<keep_meta> = $meta;
        self!set-attributes (%args);

        my @in; # Either AoA or AoH

        my IO::Handle $io-in;
        # in
        #   parse
        #     "file.csv"                       Str
        #     $io                              IO::Handle
        #     \"str,ing"                       Capture
        #   generate
        #     [["a","b"][1,2],[3,4]]     AoA   Array
        #     [{a=>1,b=>2},{a=>3,b=>4}]  AoH   Array
        #     sub { ... }                      Routine
        #     { ... }                          Callable
        #     Supply.new                       Supply
        #     start { ...}                     Channel
        if $in ~~ Supplier {
            $in = $in.Supply;
            }
        given $in {
            when Str {
                $io-in = open $in, :r, :!chomp, :enc($encoding);
                }
            when IO::Handle {
                $io-in = $in;
                }
            when IO::Path {
                $io-in = open ($in);
                }
            when Array {
                $in.list.elems or return;
                given $in.list[0] {
                    when Str {  # AoS
                        $io-in = Text::IO::String.new ($in.list.join ($!eol // "\n"));
                        }
                    default {   # AoA, AoH
                        $fragment ~~ s:i{^ "row=" } = "" and
                            self.rowrange ($fragment);
                        my @i;
                        if $in.list[0] ~~ Hash {
                            my @hdr = $in.list[0].keys.sort;
                            @i = [ @hdr ], $in.map ({$[ $_{@hdr} ]}).Slip;
                            }
                        else {
                            @i = $in.list;
                            }
                        @in = $!rrange ?? @i[$!rrange.list (@i.elems)] !! @i;
                        }
                    }
                }
            when Capture {
                $io-in = Text::IO::String.new ($in.list.map ( -> \x --> Str { x.Str }).join ($!eol // "\n"));
                }
            when Supply {
                $fragment ~~ s:i{^ "row=" } = "" and self.rowrange ($fragment);
                my int $i = 0;
                $in.act (
                    -> \row {
                        !$!rrange || $!rrange.in ($i++) and @in.push:
                            row ~~ Str ?? $[ self.getline (row) ] !! row;
                        },
                    );
                $in.wait;
                @in;
                }
            when Channel {
                $fragment ~~ s:i{^ "row=" } = "" and self.rowrange ($fragment);
                my int $i = 0;
                react {
                    whenever $in -> \row {
                        !$!rrange || $!rrange.in ($i++) and @in.push:
                            row ~~ Str ?? $[ self.getline (row) ] !! row;
                        LAST { done; }
                        }
                    }
                }
            when Callable { # Sub, Routine, Callable, Block, Code
                $fragment ~~ s:i{^ "row=" } = "" and self.rowrange ($fragment);
                my int $i = 0;
                my @hdr;
                @in = gather while $in() -> $r {
                    if (!$!rrange || $!rrange.in ($i++)) {
                        if ($r ~~ Hash) {
                            unless (@hdr.elems) {
                                @hdr = @!cnames.elems ?? @!cnames !! $r.keys.sort;
                                take [ @hdr ];
                                }
                            take [ $r{@hdr} ];
                            }
                        else {
                            take $r;
                            }
                        }
                    };
                }
            when Iterator {
                $fragment ~~ s:i{^ "row=" } = "" and self.rowrange ($fragment);
                my int $i = 0;
                my @hdr;
                @in = gather while $in.pull-one () -> \r {
                    r =:= IterationEnd and last;
                    if (!$!rrange || $!rrange.in ($i++)) {
                        if (r ~~ Hash) {
                            unless (@hdr.elems) {
                                @hdr = @!cnames.elems ?? @!cnames !! r.keys.sort;
                                take [ @hdr ];
                                }
                            take [ r{@hdr} ];
                            }
                        else {
                            take r;
                            }
                        }
                    };
                }
            default {
                self!fail (5000);
                }
            }

        if (%hdrargs and $io-in) {
            if ($headers.defined) {
                %hdrargs<munge-column-names> = "none";
                %hdrargs<set-column-names>   = False;
                }
            self.header ($io-in, |%hdrargs);
            @!cnames.elems and $headers = @!cnames;
            }

        my Str        $tmpfn;
        my IO::Handle $io-out;
        LEAVE if $out ~~ Str { .close with $io-out }
        # out
        #   Array       - return AoA (default)
        #   Hash        - return AoH (headers required)
        #   Str         - return output as Str
        #   "file.csv"  - write to file
        #   $fh         - write to $fh
        #   Routine:D   - pass row(s) to Routine
        #   Supply:D    - emit to Supply
        #   Channel:D   - emit to Channel
        given $out {
            when Str {
                ($tmpfn, $io-out) = $out.defined
                    ?? (Str, open $out, :w, :!chomp)
                    !! tempfile;
                }
            when IO::Handle {
                $io-out = $out;
                }
            when Channel:U {
                $out = Channel.new;
                }
            when Supply:U {
                $out = Supplier::Preserving.new;
                }
            when Array:D {
                $out[0] ~~ Hash and $headers ||= "auto";
                }
            when Hash:D {
                # No specific action required here
                # Check if key is supplied?
                }
            when Array:U | Hash:U | Capture:U |
                 Supplier:D | Callable:D | Channel:D {
                # No specific action required here
                }
            when Nil {
                $io-out = Nil;
                }
            when Any {
                $in ~~ Array and $io-out = $*OUT;
                }
            default {
                self!fail (5001);
                }
            }

        # Find the correct spots to invoke $on-in and $before-out

        given $headers {
            when Array:D {
                self.column_names ($headers.list);
                }
            when Callable:D | Hash:D | Str:D {
                my $hdr;
                if ($io-in ~~ IO::Handle and $io-in.defined) {
                    $hdr = self.getline ($io-in, :!meta);
                    }
                elsif (@in.elems) {
                    $hdr = @in.shift;
                    }
                unless ($headers ~~ Str && $headers eq "skip") {
                    if ($hdr.defined) {
                           if $headers ~~ Callable { $_  = $headers($_)       for @$hdr }
                        elsif $headers ~~ Hash     { $_  = $headers{$_} // $_ for @$hdr }
                        elsif $headers eq "uc"     { $_ .= uc                 for @$hdr }
                        elsif $headers eq "lc"     { $_ .= lc                 for @$hdr }
                        self.column_names ($hdr.list);
                        $out //= Hash if $out === Any;
                        }
                    $io-out.defined and self.say ($io-out, @!cnames);
                    }
                }
            }

        if ($io-in ~~ IO::Handle and $io-in.defined) {
            @in = $fragment
                ?? self.fragment    ($io-in, :$meta, $fragment)
                !! self.getline_all ($io-in, :$meta);
            }

        my @h;
        $out ~~ Hash || $headers ~~ Array || ($headers ~~ Str && $headers eq "auto") and
            @h = @!cnames.elems ?? @!cnames !! @in.shift.list;
        @h.elems and @kh = @h;

      # unless (?$out || ?$tmpfn || ?$io-out) { # TODO: Fix for csv () in void context!
        unless ((?$out && $out !~~ Hash) || ?$tmpfn) {
            if ($out ~~ Hash or @h.elems) {
                # AOH
                @h or return [];
                if (my @dup = @h.repeated) {
                    self!fail (1013, @dup.Bag.map ({ "{$_.key}({$_.value+1})" }).join: ", ");
                    }
                @in.elems && @in[0] ~~ Hash or @in = @in.map (-> @r { $%( @h Z=> @r ) });
                if ($key) {
                    if ($key ~~ Str) {
                        @in[0]{$key }:exists or self!fail (4001);
                        if (?$out) {
                            for @in -> \h { $out{h{$key}} = h };
                            return $out;
                            }
                        return $%( @in.map ( -> \h --> Pair { h{$key} => h }) );
                        }
                    if ($key ~~ Array && $key.elems > 1) {
                        my @k   = @$key;
                        my $sep = @k.shift;
                        @k.map ({@in[0]{$_}:exists}).all or self!fail (4001);
                        if (?$out) {
                            for @in -> \h { $out{h{@k.map ({h{$_}}).join: $sep}} = h };
                            return $out;
                            }
                        return $%( @in.map ( -> \h --> Pair {
                            (@k.map ({h{$_}}).join: $sep) => h }));
                        }
                    self!fail (1501);
                    }
                return @in;
                }
            # AOA
            return @in;
            }

        my $eol = self.eol;
        $eol.defined or self.eol ("\r\n");
        for @in -> $row {
            $!csv-row.fields = $row[0] ~~ CSV::Field
                ?? $row
                !! $row.map ( -> \x --> CSV::Field { CSV::Field.new.add (x.Str); });
            my @row = $meta ?? self.fields !! self.strings;
            my $r = (@h.elems == 0 || @row[0] ~~ Hash)
                ?? @row
                !! $%( @h Z=> @row );
            given $out {
                when Callable {
                    $out($r);
                    }
                when Supplier {
                    $out.emit ($r);
                    }
                when Channel {
                    $out.send ($r);
                    }
                when Array:D {
                    $out.push: $r;
                    }
                default {
                    defined $io-out and $io-out.print (self.string);
                    }
                }
            }
        self.eol ($eol);

        given $out {
            when Supplier::Preserving {
                $out.done;
                return $out.Supply;
                }
            when Channel {
                $out.close;
                return $out;
                }
            when Array:D | Hash:D {
                return $out;
                }
            }

        if (?$tmpfn) {
            $io-out.close;
            my $fh = open $tmpfn, :r, :!chomp;
            return $fh.slurp-rest;
            }

        True;
        }

    method csv ( Any       :$in   is copy,
                 Any       :$out  is copy,
                 Str       :$encoding,
                 Str       :$fragment,
                 Bool      :$meta is copy,
                 Text::CSV :$csv  = self || Text::CSV.new,
                 *%args ) {

        # file can be alias for in or for out
        if (my $file = %args<file> :delete) {
            if ($in.defined) {
                $out //= $file;
                }
            else {
                $in    = $file;
                }
            }
        $meta //= $csv.keep_meta;

        $csv.CSV (:$in, :$out, :$encoding, :$fragment, :$meta, |%args);
        }
    }

BEGIN {
    sub alias (Str:D $m, *@aka) {
        my $r := Text::CSV.^find_method ($m);
        Text::CSV.^add_method ($_, $r) for @aka;
        }

    alias ("sep",                   < sep_char sep-char separator >);
    alias ("quo",                   < quote quote_char quote-char >);
    alias ("esc",                   < escape escape_char escape-char >);
    alias ("always_quote",          < always-quote quote_always quote-always >);
    alias ("quote_empty",           < quote-empty >);
    alias ("quote_space",           < quote-space >);
    alias ("escape_null",           < escape-null quote_null quote-null >);
    alias ("quote_binary",          < quote-binary >);
    alias ("allow_loose_quotes",    < allow-loose-quotes allow_loose_quote allow-loose-quote >);
    alias ("allow_loose_escapes",   < allow-loose-escapes allow_loose_escape allow-loose-escape >);
    alias ("allow_unquoted_escape", < allow-unquoted-escape allow_unquoted_escapes allow-unquoted-escapes >);
    alias ("allow_whitespace",      < allow-whitespace >);
    alias ("blank_is_undef",        < blank-is-undef >);
    alias ("empty_is_undef",        < empty-is-undef >);
    alias ("record_number",         < record-number >);
    alias ("auto_diag",             < auto-diag >);
    alias ("keep_meta",             < keep-meta meta>);
    alias ("diag_verbose",          < diag-verbose verbose_diag verbose-diag >);
    alias ("callbacks",             < hooks >);
    alias ("formula",               < formula-handling formula_handling >);
    alias ("undef_str",             < undef-str >);
    alias ("comment_str",           < comment-str >);
    alias ("skip_empty_rows",       < skip-empty-rows >);

    alias ("column_names",          < column-names >);
    alias ("error_diag",            < error-diag diag diag-error diag_error >);
    alias ("error_input",           < error-input >);
    alias ("getline_all",           < getline-all >);
    alias ("getline_hr_all",        < getline-hr-all >);
    alias ("getline_hr",            < getline-hr >);
    alias ("is_binary",             < is-binary >);
    alias ("is_missing",            < is-missing >);
    alias ("is_quoted",             < is-quoted >);
    alias ("is_utf8",               < is-utf8 >);
    alias ("set_diag",              < SetDiag set-diag >);
    }

sub csv (*%args) is export {
    %args<meta> //= False;
    Text::CSV.csv (|%args);
    }

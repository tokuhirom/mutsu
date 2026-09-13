=begin pod
=head1 JSON::Fast

A naive imperative JSON parser in pure Raku (but with direct access to C<nqp::> ops), to evaluate performance against C<JSON::Tiny>. It is a drop-in replacement for C<JSON::Tiny>’s from-json and to-json subs, but it offers a few extra features.

Currently it seems to be about 4x faster and uses up about a quarter of the RAM JSON::Tiny would use.

This module also includes a very fast to-json function that tony-o created and lizmat later completely refactored.

=head2 SYNOPSIS

=begin code
    use JSON::Fast;
    my $storage-path = $*SPEC.tmpdir.child("json-fast-example-$*PID.json");
    say "using path $storage-path for example";
    for <recreatable monascidian spectrograph bardiest ayins sufi lavanga Dachia> -> $word {
        say "- loading json file";
        my $current-data = from-json ($storage-path.IO.slurp // "\{}");
        # $current-data now contains a Hash object populated with what was in the file
        # (or an empty hash in the very first step when the file didn't exsit yet)

        say "- adding entry for $word";
        $current-data{$word}{"length"} = $word.chars;
        $current-data{$word}{"first letter"} = $word.substr(0,1);

        say "- saving json file";
        $storage-path.IO.spurt(to-json $current-data);
        # to-json gives us a regular string, so we can plop that
        # into the file with the spurt method

        say "json file is now $storage-path.IO.s() bytes big";
        say "===";
    }
    say "here is the entire contents of the json file:";
    say "====";
    say $storage-path.IO.slurp();
    say "====";
    say "deleting storage file ...";
    $storage-path.IO.unlink;
=end code

=head2 Exported subroutines

=head3 to-json

=for code
    my $*JSON_NAN_INF_SUPPORT = 1; # allow NaN, Inf, and -Inf to be serialized.
    say to-json [<my Raku data structure>];
    say to-json [<my Raku data structure>], :!pretty;
    say to-json [<my Raku data structure>], :spacing(4);

=for code
    enum Blerp <Hello Goodbye>;
    say to-json [Hello, Goodbye]; # ["Hello", "Goodbye"]
    say to-json [Hello, Goodbye], :enums-as-value; # [0, 1]

Encode a Raku data structure into JSON. Takes one positional argument, which
is a thing you want to encode into JSON. Takes these optional named arguments:

=head4 pretty

C<Bool>. Defaults to C<True>. Specifies whether the output should be "pretty",
human-readable JSON. When set to C<False>, will output json in a single line.

=head4 spacing

C<Int>. Defaults to C<2>. Applies only when C<pretty> is C<True>.
Controls how much spacing there is between each nested level of the output.

=head4 sorted-keys

Specifies whether keys from objects should be sorted before serializing them
to a string or if C<$obj.keys> is good enough.  Defaults to C<False>.  Can
also be specified as a C<Callable> with the same type of argument that the
C<.sort> method accepts to provide alternate sorting methods.

=head4 enum-as-value

C<Bool>, defaults to C<False>.  Specifies whether C<enum>s should be json-ified
as their underlying values, instead of as the name of the C<enum>.

=head3 from-json

=for code
    my $x = from-json '["foo", "bar", {"ber": "bor"}]';
    say $x.raku;
    # outputs: $["foo", "bar", {:ber("bor")}]

Takes one positional argument that is coerced into a C<Str> type and represents
a JSON text to decode. Returns a Raku datastructure representing that JSON.

=head4 immutable

C<Bool>. Defaults to C<False>. Specifies whether C<Hash>es and C<Array>s should be
rendered as immutable datastructures instead (as C<Map> / C<List>.  Creating an
immutable data structures is mostly saving on memory usage, and a little bit on
CPU (typically around 5%).

This also has the side effect that elements from the returned structure can now
be iterated over directly because they are not containerized.

=for code
    my %hash := from-json "META6.json".IO.slurp, :immutable;
    say "Provides:";
    .say for %hash<provides>;

=head4 allow-jsonc

C<Bool>.  Defaults to C<False>.  Specifies whether commmands adhering to the
L<JSONC standard|https://changelog.com/news/jsonc-is-a-superset-of-json-which-supports-comments-6LwR>
are allowed.

=head2 Additional features

=head3 Adapting defaults of "from-json"

In the C<use> statement, you can add the string C<"immutable"> to make the
default of the C<immutable> parameter to the C<from-json> subroutine C<True>,
rather than C<False>.

=for code
    use JSON::Fast <immutable>;  # create immutable data structures by default

=head3 Adapting defaults of "to-json"

In the C<use> statement, you can add the strings C<"!pretty">,
C<"sorted-keys"> and/or C<"enums-as-value"> to change the associated
defaults of the C<to-json> subroutine.

=for code
    use JSON::FAST <!pretty sorted-keys enums-as-value>;

=head3 Strings containing multiple json pieces

When the document contains additional non-whitespace after the first
successfully parsed JSON object, JSON::Fast will throw the exception
C<X::JSON::AdditionalContent>. If you expect multiple objects, you
can catch that exception, retrieve the parse result from its
C<parsed> attribute, and remove the first C<rest-position> characters
off of the string and restart parsing from there.

=end pod

use nqp;

our class X::JSON::AdditionalContent is Exception is export {
    has $.parsed;
    has $.parsed-length;
    has $.rest-position;

    method message {
        "JSON Input contained additional text after the document (parsed $.parsed-length chars, next non-whitespace lives at $.rest-position)"
    }
}

module JSON::Fast:ver<0.20.1> {

    multi sub to-surrogate-pair(Int $ord) {
        my int $base   = $ord - 0x10000;
        my int $top    = $base +& 0b1_1111_1111_1100_0000_0000 +> 10;
        my int $bottom = $base +&               0b11_1111_1111;
        Q/\u/ ~ (0xD800 + $top).base(16) ~ Q/\u/ ~ (0xDC00 + $bottom).base(16);
    }

    multi sub to-surrogate-pair(Str $input) {
        to-surrogate-pair(nqp::ordat($input, 0));
    }

    my $tab := nqp::list_i(92,116); # \t
    my $lf  := nqp::list_i(92,110); # \n
    my $cr  := nqp::list_i(92,114); # \r
    my $qq  := nqp::list_i(92, 34); # \"
    my $bs  := nqp::list_i(92, 92); # \\

# Convert string to decomposed codepoints.  Run over that integer array
# and inject whatever is necessary, don't do anything if simple ascii.
# Then convert back to string and return that.
    sub str-escape(\text) {
        my $codes := text.NFD;
        my int $i = -1;

        nqp::while(
          nqp::islt_i(++$i,nqp::elems($codes)),
          nqp::if(
            nqp::isle_i((my int $code = nqp::atpos_i($codes,$i)),92)
              || nqp::isge_i($code,128),
            nqp::if(                                           # not ascii
              nqp::isle_i($code,31),
              nqp::if(                                          # control
                nqp::iseq_i($code,10),
                nqp::splice($codes,$lf,$i++,1),                  # \n
                nqp::if(
                  nqp::iseq_i($code,13),
                  nqp::splice($codes,$cr,$i++,1),                 # \r
                  nqp::if(
                    nqp::iseq_i($code,9),
                    nqp::splice($codes,$tab,$i++,1),               # \t
                    nqp::stmts(                                    # other control
                      nqp::splice($codes,$code.fmt(Q/\u%04x/).NFD,$i,1),
                      ($i = nqp::add_i($i,5))
                    )
                  )
                )
              ),
              nqp::if(                                          # not control
                nqp::iseq_i($code,34),
                nqp::splice($codes,$qq,$i++,1),                  # "
                nqp::if(
                  nqp::iseq_i($code,92),
                  nqp::splice($codes,$bs,$i++,1),                 # \
                  nqp::if(
                    nqp::isge_i($code,0x10000),
                    nqp::stmts(                                    # surrogates
                      nqp::splice(
                        $codes,
                        (my $surrogate := to-surrogate-pair($code.chr).NFD),
                        $i,
                        1
                      ),
                      ($i = nqp::sub_i(nqp::add_i($i,nqp::elems($surrogate)),1))
                    )
                  )
                )
              )
            )
          )
        );

        nqp::strfromcodes($codes)
    }

    our sub to-json(
      \obj,
      Bool :$pretty         = True,
      Int  :$level          = 0,
      int  :$spacing        = 2,
           :$sorted-keys    = False,
      Bool :$enums-as-value = False,
    ) {

        my str @out;
        my str $spaces = ' ' x $spacing;
        my str $comma  = ",\n" ~ $spaces x $level;

#-- helper subs from here, with visibility to the above lexicals

        sub pretty-positional(\positional --> Nil) {
            $comma = nqp::concat($comma,$spaces);
            nqp::push_s(@out,'[');
            nqp::push_s(@out,nqp::substr($comma,1));

            for positional.list {
                jsonify($_);
                nqp::push_s(@out,$comma);
            }
            nqp::pop_s(@out);  # lose last comma

            $comma = nqp::substr($comma,0,nqp::sub_i(nqp::chars($comma),$spacing));
            nqp::push_s(@out,nqp::substr($comma,1));
            nqp::push_s(@out,']');
        }

        sub pretty-associative(\associative --> Nil) {
            $comma = nqp::concat($comma,$spaces);
            nqp::push_s(@out,'{');
            nqp::push_s(@out,nqp::substr($comma,1));
            my \pairs := $sorted-keys
              ?? associative.sort($sorted-keys<> =:= True ?? *.key !! $sorted-keys)
              !! associative.list;

            for pairs {
                nqp::push_s(@out,'"');
                nqp::push_s(@out, str-escape(.key.Str));
                nqp::push_s(@out,'": ');
                jsonify(.value);
                nqp::push_s(@out,$comma);
            }
            nqp::pop_s(@out);  # lose last comma

            $comma = nqp::substr($comma,0,nqp::sub_i(nqp::chars($comma),$spacing));
            nqp::push_s(@out,nqp::substr($comma,1));
            nqp::push_s(@out,'}');
        }

        sub unpretty-positional(\positional --> Nil) {
            nqp::push_s(@out,'[');
            my int $before = nqp::elems(@out);
            for positional.list {
                jsonify($_);
                nqp::push_s(@out,",");
            }
            nqp::pop_s(@out) if nqp::elems(@out) > $before;  # lose last comma
            nqp::push_s(@out,']');
        }

        sub unpretty-associative(\associative --> Nil) {
            nqp::push_s(@out,'{');
            my \pairs := $sorted-keys
              ?? associative.sort($sorted-keys<> =:= True ?? *.key !! $sorted-keys)
              !! associative.list;

            my int $before = nqp::elems(@out);
            for pairs {
                nqp::push_s(@out, '"');
                nqp::push_s(@out, str-escape(.key.Str));
                nqp::push_s(@out,'":');
                jsonify(.value);
                nqp::push_s(@out,",");
            }
            nqp::pop_s(@out) if nqp::elems(@out) > $before;  # lose last comma
            nqp::push_s(@out,'}');
        }

        sub jsonify(\obj --> Nil) {

            with obj {

                # basic ones
                if nqp::istype($_, Bool) {
                    nqp::push_s(@out,obj ?? "true" !! "false");
                }
                elsif nqp::istype($_, IntStr) {
                    jsonify(.Int);
                }
                elsif nqp::istype($_, RatStr) {
                    jsonify(.Rat);
                }
                elsif nqp::istype($_, NumStr) {
                    jsonify(.Num);
                }
                elsif nqp::istype($_, Enumeration) {
                    if $enums-as-value {
                        jsonify(.value);
                    }
                    else {
                        nqp::push_s(@out,'"');
                        nqp::push_s(@out,str-escape(.key));
                        nqp::push_s(@out,'"');
                    }
                }
                # Str and Int go below Enumeration, because there
                # are both Str-typed enums and Int-typed enums
                elsif nqp::istype($_, Str) {
                    nqp::push_s(@out,'"');
                    nqp::push_s(@out,str-escape($_));
                    nqp::push_s(@out,'"');
                }

                # numeric ones
                elsif nqp::istype($_, Int) {
                    nqp::push_s(@out,.Str);
                }
                elsif nqp::istype($_, Rat) {
                    nqp::push_s(@out,.contains(".") ?? $_ !! "$_.0")
                      given .Str;
                }
                elsif nqp::istype($_, FatRat) {
                    nqp::push_s(@out,.contains(".") ?? $_ !! "$_.0")
                      given .Str;
                }
                elsif nqp::istype($_, Rational) {
                    nqp::push_s(@out,.contains(".") ?? $_ !! "$_.0")
                      given .Str;
                }
                elsif nqp::istype($_, Num) {
                    if nqp::isnanorinf($_) {
                        nqp::push_s(
                          @out,
                          $*JSON_NAN_INF_SUPPORT ?? obj.Str !! "null"
                        );
                    }
                    else {
                        nqp::push_s(@out,.contains("e") ?? $_ !! $_ ~ "e0")
                          given .Str;
                    }
                }

                # iterating ones
                elsif nqp::istype($_, Seq) {
                    jsonify(.cache);
                }
                elsif nqp::istype($_, Associative) {
                    $pretty
                      ?? pretty-associative($_)
                      !! unpretty-associative($_);
                }
                elsif nqp::istype($_, Positional) {
                    $pretty
                      ?? pretty-positional($_)
                      !! unpretty-positional($_);
                }

                # rarer ones
                elsif nqp::istype($_, Dateish) {
                    nqp::push_s(@out,qq/"$_"/);
                }
                elsif nqp::istype($_, Instant) {
                    nqp::push_s(@out,qq/"{.DateTime}"/);
                }
                elsif nqp::istype($_, Real) {
                    jsonify(.Bridge);
                }
                elsif nqp::istype($_, Version) {
                    jsonify(.Str);
                }

                # huh, what?
                else {
                    die "Don't know how to jsonify {.^name}";
                }
            }
            else {
                nqp::push_s(@out,'null');
            }
        }

#-- do the actual work

        jsonify(obj);
        nqp::join("",@out)
    }

    my $ws := nqp::list_i;
    nqp::bindpos_i($ws,  9, 1);  # \t
    nqp::bindpos_i($ws, 10, 1);  # \n
    nqp::bindpos_i($ws, 13, 1);  # \r
    nqp::bindpos_i($ws, 32, 1);  # space
    nqp::push_i($ws, 0);  # allow for -1 as value

    my sub nom-ws(str $text, int $pos is rw --> Nil) {
        nqp::while(
          nqp::atpos_i($ws, nqp::ordat($text, $pos)),
          ++$pos
        );
        nqp::if(
          nqp::iseq_i(nqp::ordat($text,$pos),47),  # /
          nom-comment($text,++$pos)
        );
    }

    my sub nom-comment(str $text, int $pos is rw --> Nil) {
        unless $*ALLOW-JSONC {
            --$pos;  # un-eat the /
            return;
        }

        my int $startpos = $pos;

        my int $ord;
        nqp::if(
          nqp::iseq_i(($ord = nqp::ordat($text,$pos)),47),          # /
          nqp::stmts(
            nqp::while(  # eating a // style comment
              nqp::isne_i(($ord = nqp::ordat($text,++$pos)),10)     # not \n
                && nqp::isne_i($ord,-1),                            # not eos
              nqp::null
            ),
            nom-ws($text, $ord == -1 ?? $pos !! ++$pos)
          ),
          nqp::if(
            nqp::iseq_i($ord,42),                                   # *
            nqp::stmts(
              ++$pos,
              nqp::until(  # eating a /*  */ style comment
                nqp::iseq_i(($ord = nqp::ordat($text,++$pos)),-1)   # eos
                  || (nqp::iseq_i($ord,47)                          # /
                        && nqp::iseq_i(
                             nqp::ordat($text,nqp::sub_i($pos,1)),
                             42                                     # *
                           )),
                nqp::null
              ),
              nqp::if(
                nqp::iseq_i($ord,-1),
                die-end-in-comment($text,$pos,$startpos),
                nom-ws($text, ++$pos)
              )
            ),
            nqp::if(
              nqp::iseq_i($ord,-1),
              die-end-in-comment($text,$pos,$startpos),
              die-expected-object($text,$pos,$startpos)
            )
          )
        );
    }

    my $hexdigits := nqp::list;
    nqp::bindpos($hexdigits,  48,  0);  # 0
    nqp::bindpos($hexdigits,  49,  1);  # 1
    nqp::bindpos($hexdigits,  50,  2);  # 2
    nqp::bindpos($hexdigits,  51,  3);  # 3
    nqp::bindpos($hexdigits,  52,  4);  # 4
    nqp::bindpos($hexdigits,  53,  5);  # 5
    nqp::bindpos($hexdigits,  54,  6);  # 6
    nqp::bindpos($hexdigits,  55,  7);  # 7
    nqp::bindpos($hexdigits,  56,  8);  # 8
    nqp::bindpos($hexdigits,  57,  9);  # 9
    nqp::bindpos($hexdigits,  65, 10);  # A
    nqp::bindpos($hexdigits,  66, 11);  # B
    nqp::bindpos($hexdigits,  67, 12);  # C
    nqp::bindpos($hexdigits,  68, 13);  # D
    nqp::bindpos($hexdigits,  69, 14);  # E
    nqp::bindpos($hexdigits,  70, 15);  # F
    nqp::bindpos($hexdigits,  97, 10);  # a
    nqp::bindpos($hexdigits,  98, 11);  # b
    nqp::bindpos($hexdigits,  99, 12);  # c
    nqp::bindpos($hexdigits, 100, 13);  # d
    nqp::bindpos($hexdigits, 101, 14);  # e
    nqp::bindpos($hexdigits, 102, 15);  # f

    my $escapees := nqp::list_i;
    nqp::bindpos_i($escapees,  34, 34);  # "
    nqp::bindpos_i($escapees,  47, 47);  # /
    nqp::bindpos_i($escapees,  92, 92);  # \
    nqp::bindpos_i($escapees,  98,  8);  # b
    nqp::bindpos_i($escapees, 102, 12);  # f
    nqp::bindpos_i($escapees, 110, 10);  # n
    nqp::bindpos_i($escapees, 114, 13);  # r
    nqp::bindpos_i($escapees, 116,  9);  # t

    my constant CONTEXT-LENGTH = 5;
    my sub context-for-error-report(str $text, int $pos, int $startpos = -1) {
        my int $start-of-context = 0 max $pos - CONTEXT-LENGTH;
        my str $prematch = nqp::substr($text, $start-of-context, $pos - $start-of-context);
        my str $postmatch = nqp::substr($text, $pos, CONTEXT-LENGTH);
        my int $has-postmatch = nqp::chars($postmatch) > 1;
        "Context: "
          ~ (($start-of-context == 0 ?? "<BOS>" !! "")
            ~ $prematch).raku().substr(1,*-1)
          ~ "→"
          ~ ($has-postmatch
              ?? $postmatch.substr(0,1).raku.substr(1,*-1) ~ "←"
              !! "")
          ~ ($postmatch
            ~ (nqp::chars($postmatch) < CONTEXT-LENGTH ?? "<EOS>" !! "")).raku.substr($has-postmatch ?? 2 !! 1,*-1)
          ~ ($startpos != -1
              ?? " (corresponding start " ~ ($pos - $startpos) ~ " characters earlier, at $startpos)"
              !! "");
    }

    my sub parse-string(str $text, int $pos is rw) {
        nqp::if(
          nqp::eqat($text, '"', nqp::sub_i($pos,1))  # starts with clean "
            && nqp::eqat($text, '"',                 # ends with clean "
                 (my int $end = nqp::findnotcclass(nqp::const::CCLASS_WORD,
                   $text, $pos, nqp::sub_i(nqp::chars($text),$pos)))
          ),
          nqp::stmts(
            (my $string := nqp::substr($text, $pos, nqp::sub_i($end, $pos))),
            ($pos = nqp::add_i($end,1)),
            $string
          ),
          parse-string-slow($text, $pos)
        )
    }

# Slower parsing of string if the string does not exist of 0 or more
# alphanumeric characters
    my sub parse-string-slow(str $text, int $pos is rw) {

        my int $start = nqp::sub_i($pos,1);  # include starter in string
        nqp::until(
          nqp::iseq_i((my $end := nqp::index($text, '"', $pos)), -1),
          nqp::stmts(
            ($pos = $end + 1),
            (my int $index = 1),
            nqp::while(
              nqp::eqat($text, '\\', nqp::sub_i($end, $index)),
              ($index = nqp::add_i($index, 1))
            ),
            nqp::if(
              nqp::bitand_i($index, 1),
              (return unjsonify-string(      # preceded by an even number of \
                nqp::strtocodes(
                  nqp::substr($text, $start, $end - $start),
                  nqp::const::NORMALIZE_NFD,
                  nqp::create(NFD)
                ),
                $pos
              ))
            )
          )
        );
        die "unexpected end of input in string\n" ~ context-for-error-report($text, $pos, $start);
    }

# convert a sequence of Uni elements into a string, with the initial
# quoter as the first element.
    my sub unjsonify-string(Uni:D \codes, int $pos) {
        nqp::shift_i(codes);  # lose the " without any decoration

        # fetch a single codepoint from the next 4 Uni elements
        my sub fetch-codepoint() {
            my int $codepoint = 0;
            my int $times = 5;

            nqp::while(
              ($times = nqp::sub_i($times, 1)),
              nqp::if(
                nqp::elems(codes),
                nqp::if(
                  nqp::iseq_i(
                    (my uint32 $ordinal = nqp::shift_i(codes)),
                    48  # 0
                  ),
                  ($codepoint = nqp::mul_i($codepoint, 16)),
                  nqp::if(
                    (my int $adder = nqp::atpos($hexdigits, $ordinal)),
                    ($codepoint = nqp::add_i(
                      nqp::mul_i($codepoint, 16),
                      $adder
                    )),
                    (die "invalid hexadecimal char {
                        nqp::chr($ordinal).raku
                    } in \\u sequence at $pos")
                  )
                ),
                (die "incomplete \\u sequence in string near $pos")
              )
            );

            $codepoint
        }

        my $output := nqp::create(Uni);
        nqp::while(
          nqp::elems(codes),
          nqp::if(
            nqp::iseq_i(
              (my uint32 $ordinal = nqp::shift_i(codes)),
              92  # \
            ),
            nqp::if(                                           # haz an escape
              nqp::iseq_i(($ordinal = nqp::shift_i(codes)), 117),  # u
              nqp::stmts(                                      # has a \u escape
                nqp::if(
                  nqp::isge_i((my int $codepoint = fetch-codepoint), 0xD800)
                    && nqp::islt_i($codepoint, 0xE000),
                  nqp::if(                                     # high surrogate
                    nqp::iseq_i(nqp::atpos_i(codes, 0),  92)        # \
                      && nqp::iseq_i(nqp::atpos_i(codes, 1), 117),  # u
                    nqp::stmts(                                # low surrogate
                      nqp::shift_i(codes),  # get rid of \
                      nqp::shift_i(codes),  # get rid of u
                      nqp::if(
                        nqp::isge_i((my int $low = fetch-codepoint), 0xDC00),
                        ($codepoint = nqp::add_i(              # got low surrogate
                          nqp::add_i(                          # transmogrify
                            nqp::mul_i(nqp::sub_i($codepoint, 0xD800), 0x400),
                            0x10000                            # with
                          ),                                   # low surrogate
                          nqp::sub_i($low, 0xDC00)
                        )),
                        (die "improper low surrogate \\u$low.base(16) for high surrogate \\u$codepoint.base(16) near $pos")
                      )
                    ),
                    (die "missing low surrogate for high surrogate \\u$codepoint.base(16) near $pos")
                  )
                ),
                nqp::push_i($output, $codepoint)
              ),
              nqp::if(                                         # other escapes?
                ($codepoint = nqp::atpos_i($escapees, $ordinal)),
                nqp::push_i($output, $codepoint),              # recognized escape
                (die "unknown escape code found '\\{           # huh?
                    nqp::chr($ordinal)
                }' found near $pos")
              )
            ),
            nqp::if(                                           # not an escape
              nqp::iseq_i($ordinal, 9) || nqp::iseq_i($ordinal, 10),  # \t \n
              (die "this kind of whitespace is not allowed in a string: '{
                  nqp::chr($ordinal).raku
              }' near $pos"),
              nqp::push_i($output, $ordinal)                   # ok codepoint
            )
          )
        );

        nqp::strfromcodes($output)
    }

    my sub parse-numeric(str $text, int $pos is rw) {
        my int $start = nqp::sub_i($pos,1);

        my int $end = nqp::findnotcclass(nqp::const::CCLASS_NUMERIC,
          $text, $pos, nqp::sub_i(nqp::chars($text),$pos));
        nqp::if(
          nqp::iseq_i(nqp::ordat($text, $end), 46),                      # .
          nqp::stmts(
            ($pos = nqp::add_i($end,1)),
            ($end = nqp::findnotcclass(nqp::const::CCLASS_NUMERIC,
              $text, $pos, nqp::sub_i(nqp::chars($text),$pos))
            )
          )
        );

        nqp::if(
          nqp::iseq_i((my int $ordinal = nqp::ordat($text, $end)), 101)  # e
           || nqp::iseq_i($ordinal, 69),                                 # E
          nqp::stmts(
            ($pos = nqp::add_i($end,1)),
            ($pos = nqp::add_i($pos,
              nqp::eqat($text, '-', $pos) || nqp::eqat($text, '+', $pos)
            )),
            ($end = nqp::findnotcclass(nqp::const::CCLASS_NUMERIC,
              $text, $pos, nqp::sub_i(nqp::chars($text),$pos))
            )
          )
        );

        my $result := nqp::substr($text, $start, nqp::sub_i($end,$start)).Numeric;
        nqp::if(
          nqp::istype($result, Failure),
          nqp::stmts(
            $result.Bool,  # handle Failure
            (die "at $pos: invalid number token $text.substr($start,$end - $start)\n" ~ context-for-error-report($text, $pos))
          ),
          nqp::stmts(
            ($pos = $end),
            $result
          )
        )
    }

    my sub die-end-in-comment(str $text, int $pos, int $startpos) is hidden-from-backtrace {
        die "reached end of input inside comment\n" ~ context-for-error-report($text, $pos, $startpos);
    }

    my sub die-missing-object-key(str $text, int $pos, int $startpos) is hidden-from-backtrace {
        die ($pos == nqp::chars($text)
          ?? "at end of input: expected a quoted string for an object key"
          !! "at $pos: json requires object keys to be strings") ~ "\n" ~ context-for-error-report($text, $pos, $startpos);
    }

    my sub die-unexpected-partitioner(str $text, int $pos, int $startpos) is hidden-from-backtrace {
        die ($pos == nqp::chars($text)
          ?? "at end of input: expected a partitioner inside list of things"
          !! "at $pos, unexpected partitioner '{
                nqp::substr($text,$pos,1)
              }' inside list of things in an array") ~ "\n" ~ context-for-error-report($text, $pos, $startpos);
    }

    my sub die-missing-colon(str $text, int $pos, int $startpos) is hidden-from-backtrace {
        die "expected to see a ':' after an object key at $pos\n" ~ context-for-error-report($text, $pos, $startpos);
    }

    my sub die-unexpected-end-of-object(str $text, int $pos, int $startpos = -1) is hidden-from-backtrace {
        die ($pos == nqp::chars($text)
          ?? "at end of input: unexpected end of object."
          !! "unexpected '{ nqp::substr($text, $pos, 1) }' in an object at $pos") ~ "\n" ~ context-for-error-report($text, $pos, $startpos);
    }

    my sub die-expected-object(str $text, int $pos, int $startpos) is hidden-from-backtrace {
        die ($pos == nqp::chars($text)
          ?? "at end of input: expected a json object"
          !! "at $pos: expected a json object") ~ "\n" ~ context-for-error-report($text, $pos, $startpos);
    }

    my sub parse-obj(str $text, int $pos is rw) {
        my %result;
        my $hash := nqp::ifnull(
          nqp::getattr(%result,Map,'$!storage'),
          nqp::bindattr(%result,Map,'$!storage',nqp::hash)
        );

        my int $startpos = $pos;

        nom-ws($text, $pos);
        my int $ordinal = nqp::ordat($text, $pos);
        nqp::if(
          nqp::iseq_i($ordinal, 125),  # }             {
          nqp::stmts(
            ($pos = nqp::add_i($pos,1)),
            %result
          ),
          nqp::stmts(
            my $descriptor := nqp::getattr(%result,Hash,'$!descriptor');
            nqp::stmts(  # this level is needed for some reason
              nqp::while(
                1,
                nqp::stmts(
                  nqp::if(
                    nqp::iseq_i($ordinal, 34),  # "
                    (my $key := parse-string($text, $pos = nqp::add_i($pos,1))),
                    die-missing-object-key($text, $pos, $startpos)
                  ),
                  nom-ws($text, $pos),
                  nqp::if(
                    nqp::iseq_i(nqp::ordat($text, $pos), 58),  # :
                    ($pos = nqp::add_i($pos, 1)),
                    die-missing-colon($text, $pos, $startpos)
                  ),
                  nom-ws($text, $pos),
                  nqp::bindkey($hash, $key,
                    nqp::p6scalarwithvalue($descriptor, parse-thing($text, $pos))),
                  nom-ws($text, $pos),
                  ($ordinal = nqp::ordat($text, $pos)),
                  nqp::if(
                    nqp::iseq_i($ordinal, 125),  # }  {
                    nqp::stmts(
                      ($pos = nqp::add_i($pos,1)),
                      (return %result)
                    ),
                    nqp::unless(
                      nqp::iseq_i($ordinal, 44),  # ,
                      die-unexpected-end-of-object($text, $pos, $startpos)
                    )
                  ),
                  nom-ws($text, $pos = nqp::add_i($pos,1)),
                  ($ordinal = nqp::ordat($text, $pos)),
                )
              )
            )
          )
        )
    }

    my sub parse-array(str $text, int $pos is rw) {
        my @result;
        nqp::bindattr(@result, List, '$!reified',
          my $buffer := nqp::create(IterationBuffer));

        nom-ws($text, $pos);

        my int $startpos = $pos;

        nqp::if(
          nqp::eqat($text, ']', $pos),
          nqp::stmts(
            ($pos = nqp::add_i($pos,1)),
            @result
          ),
          nqp::stmts(
            (my $descriptor := nqp::getattr(@result, Array, '$!descriptor')),
            nqp::while(
              1,
              nqp::stmts(
                (my $thing := parse-thing($text, $pos)),
                nom-ws($text, $pos),
                (my int $partitioner = nqp::ordat($text, $pos)),
                nqp::if(
                  nqp::iseq_i($partitioner,93),  # ]
                  nqp::stmts(
                    nqp::push($buffer,nqp::p6scalarwithvalue($descriptor,$thing)),
                    ($pos = nqp::add_i($pos,1)),
                    (return @result)
                  ),
                  nqp::if(
                    nqp::iseq_i($partitioner,44),  # ,
                    nqp::stmts(
                      nqp::push($buffer,nqp::p6scalarwithvalue($descriptor,$thing)),
                      ($pos = nqp::add_i($pos,1))
                    ),
                    die-unexpected-partitioner($text, $pos, $startpos)
                  )
                )
              )
            )
          )
        )
    }

    my sub parse-true( int $pos is rw --> True)  { $pos = $pos + 4      }
    my sub parse-false(int $pos is rw --> False) { $pos = $pos + 5      }
    my sub parse-null( int $pos is rw)           { $pos = $pos + 4; Any }

    my sub parse-thing(str $text, int $pos is rw) {
        nom-ws($text, $pos);
        my int $ordinal = nqp::ordat($text, $pos);
        my int $startpos = $pos;

        nqp::iseq_i($ordinal,34)                     # "
          ?? parse-string($text, $pos = $pos + 1)
          !! nqp::iseq_i($ordinal,91)                # [
            ?? parse-array($text, $pos = $pos + 1)
            !! nqp::iseq_i($ordinal,123)             # {
              ?? parse-obj($text, $pos = $pos + 1)
              !! nqp::iscclass(nqp::const::CCLASS_NUMERIC, $text, $pos)
                   || nqp::iseq_i($ordinal,45)       # -
                ?? parse-numeric($text, $pos = $pos + 1)
                !! nqp::iseq_i($ordinal,116) && nqp::eqat($text,'true',$pos)
                  ?? parse-true($pos)
                  !! nqp::iseq_i($ordinal,102) && nqp::eqat($text,'false',$pos)
                    ?? parse-false($pos)
                    !! nqp::iseq_i($ordinal,110) && nqp::eqat($text,'null',$pos)
                      ?? parse-null($pos)
                      !! die-expected-object($text, $pos, $startpos)
    }

# Needed so that subroutines can return native hashes without them
# getting upgraded to Hash.  The equivalent of IterationBuffer but
# then for Associatives.
    my class IterationMap is repr("VMHash") { }

# Since we create immutable structures, we can have all of the empty
# hashes and arrays refer to the same empty Map and empty List.
    my $emptyMap  := Map.new;
    my $emptyList := List.new;

    my sub hllize-map(\the-map) is raw {
        nqp::elems(the-map)
          ?? nqp::p6bindattrinvres(nqp::create(Map),Map,'$!storage',the-map)
          !! $emptyMap
    }

    my sub hllize-list(\the-list) is raw {
        nqp::elems(the-list)
          ?? nqp::p6bindattrinvres(nqp::create(List),List,'$!reified',the-list)
          !! $emptyList
    }

    my sub parse-obj-immutable(str $text, int $pos is rw) {
        my $map := nqp::create(IterationMap);

        nom-ws($text, $pos);

        my int $startpos = $pos;

        my int $ordinal = nqp::ordat($text, $pos);
        nqp::if(
          nqp::iseq_i($ordinal, 125),  # }             {
          nqp::stmts(
            ($pos = nqp::add_i($pos,1)),
            hllize-map($map)
          ),
          nqp::stmts(  # this level is needed for some reason
            nqp::while(
              1,
              nqp::stmts(
                nqp::if(
                  nqp::iseq_i($ordinal, 34),  # "
                  (my $key := parse-string($text, $pos = nqp::add_i($pos,1))),
                  die-missing-object-key($text, $pos, $startpos)
                ),
                nom-ws($text, $pos),
                nqp::if(
                  nqp::iseq_i(nqp::ordat($text, $pos), 58),  # :
                  ($pos = nqp::add_i($pos, 1)),
                  die-missing-colon($text, $pos, $startpos)
                ),
                nom-ws($text, $pos),
                nqp::bindkey($map, $key,parse-thing-immutable($text, $pos)),
                nom-ws($text, $pos),
                ($ordinal = nqp::ordat($text, $pos)),
                nqp::if(
                  nqp::iseq_i($ordinal, 125),  # }  {
                  nqp::stmts(
                    ($pos = nqp::add_i($pos,1)),
                    (return hllize-map($map))
                  ),
                  nqp::unless(
                    nqp::iseq_i($ordinal, 44),  # ,
                    die-unexpected-end-of-object($text, $pos, $startpos)
                  )
                ),
                nom-ws($text, $pos = nqp::add_i($pos,1)),
                ($ordinal = nqp::ordat($text, $pos)),
              )
            )
          )
        )
    }

    my sub parse-array-immutable(str $text, int $pos is rw) {
        my $list := nqp::create(IterationBuffer);

        nom-ws($text, $pos);

        my int $startpos = $pos;

        nqp::if(
          nqp::eqat($text, ']', $pos),
          nqp::stmts(
            ($pos = nqp::add_i($pos,1)),
            hllize-list($list)
          ),
          nqp::stmts(  # this level is needed for some reason
            nqp::while(
              1,
              nqp::stmts(
                (my $thing := parse-thing-immutable($text, $pos)),
                nom-ws($text, $pos),
                (my int $partitioner = nqp::ordat($text, $pos)),
                nqp::if(
                  nqp::iseq_i($partitioner,93),  # ]
                  nqp::stmts(
                    nqp::push($list, $thing),
                    ($pos = nqp::add_i($pos,1)),
                    (return hllize-list($list))
                  ),
                  nqp::if(
                    nqp::iseq_i($partitioner,44),  # ,
                    nqp::stmts(
                      nqp::push($list, $thing),
                      ($pos = nqp::add_i($pos,1))
                    ),
                    die-unexpected-partitioner($text, $pos, $startpos)
                  )
                )
              )
            )
          )
        )
    }

    my sub parse-thing-immutable(str $text, int $pos is rw) {
        nom-ws($text, $pos);
        my int $ordinal = nqp::ordat($text, $pos);
        my int $startpos = $pos;

        nqp::iseq_i($ordinal,34)                     # "
          ?? parse-string($text, $pos = $pos + 1)
          !! nqp::iseq_i($ordinal,91)                # [
            ?? parse-array-immutable($text, $pos = $pos + 1)
            !! nqp::iseq_i($ordinal,123)             # {
              ?? parse-obj-immutable($text, $pos = $pos + 1)
              !! nqp::iscclass(nqp::const::CCLASS_NUMERIC, $text, $pos)
                   || nqp::iseq_i($ordinal,45)       # -
                ?? parse-numeric($text, $pos = $pos + 1)
                !! nqp::iseq_i($ordinal,116) && nqp::eqat($text,'true',$pos)
                  ?? parse-true($pos)
                  !! nqp::iseq_i($ordinal,102) && nqp::eqat($text,'false',$pos)
                    ?? parse-false($pos)
                    !! nqp::iseq_i($ordinal,110) && nqp::eqat($text,'null',$pos)
                      ?? parse-null($pos)
                      !! die-expected-object($text, $pos, $startpos)
    }

    my sub may-die-additional-content($parsed, str $text, int $pos is rw) is hidden-from-backtrace {
        my int $parsed-length = $pos;
        try nom-ws($text, $pos);

        X::JSON::AdditionalContent.new(
          :$parsed, :$parsed-length, rest-position => $pos
        ).throw unless nqp::iseq_i($pos,nqp::chars($text));
    }

    our sub from-json(Str() $text, :$immutable, :$allow-jsonc) {
        my int $pos;
        my $*ALLOW-JSONC := $allow-jsonc;
        my $parsed := $immutable
          ?? parse-thing-immutable($text, $pos)
          !! parse-thing($text, $pos);

        # not at the end yet?
        may-die-additional-content($parsed, $text, $pos)
          unless nqp::iseq_i($pos,nqp::chars($text));

        $parsed
    }
}

sub EXPORT(*@_) {
    my @huh;

    my $from-json-changed;
    my $immutable-default := False;

    my $to-json-changed;
    my $pretty-default         := True;
    my $sorted-keys-default    := False;
    my $enums-as-value-default := False;

    for @_ {
        when "immutable" {
            $immutable-default := True;
            $from-json-changed := True;
        }
        when "!pretty" {
            $pretty-default  := False;
            $to-json-changed := True;
        }
        when "sorted-keys" {
            $sorted-keys-default := True;
            $to-json-changed     := True;
        }
        when "enums-as-value" {
            $enums-as-value-default := True;
            $to-json-changed        := True;
        }
        when "pretty" | "!immutable" | "!sorted-keys" | "!enums-as-value" {
            # no action, these are the defaults
        }
        default {
            @huh.push: $_;
        }
    }

    die "Unrecognized strings in -use- statement: @huh[]"
      if @huh;

    my sub from-json-changed(Str() $text,
      :$immutable = $immutable-default,
    ) {
        JSON::Fast::from-json($text, :$immutable)
    }
    my sub to-json-changed(\obj,
      :$pretty         = $pretty-default,
      :$sorted-keys    = $sorted-keys-default,
      :$enums-as-value = $enums-as-value-default,
    ) {
        JSON::Fast::to-json(obj, :$pretty, :$sorted-keys, :$enums-as-value)
    }

    Map.new((
      '&from-json' => $from-json-changed
        ?? &from-json-changed
        !! &JSON::Fast::from-json,
      '&to-json' => $to-json-changed
        ?? &to-json-changed
        !! &JSON::Fast::to-json,
    ))
}

# vi:syntax=perl6

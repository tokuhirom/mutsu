# Fixture for t/vm/codegen/adr0110-trir-module-linkage.t (#9072).
#
# The shapes JSON::Fast's decoder is built from, in a module: routines that
# call earlier-declared routines (statically linked `CallTr`), a free `my` of
# the enclosing `module { }` block read inside a linked callee, recursion
# through a linked call, and loops left through an expression-position
# `return`. Every one of these either bailed on every call (the linked callee
# was looked up in the wrong table, and the block's `my` was not found) or,
# once linked, answered wrongly (the `return` escaped the linked frame).
use nqp;

module TrirModuleLinkage {
    my $ws := nqp::list_i;
    nqp::bindpos_i($ws, 32, 1);
    nqp::push_i($ws, 0);

    my sub skip-ws(str $text, int $pos is rw --> Nil) {
        nqp::while(nqp::atpos_i($ws, nqp::ordat($text, $pos)), ++$pos);
    }

    my sub parse-int(str $text, int $pos is rw) {
        my int $start = $pos;
        nqp::while(
          nqp::isge_i(nqp::ordat($text, $pos), 48) && nqp::isle_i(nqp::ordat($text, $pos), 57),
          ++$pos
        );
        nqp::substr($text, $start, nqp::sub_i($pos, $start)).Int
    }

    # `[` INT-or-LIST (`,` INT-or-LIST)* `]`, leaving `$pos` past the `]`.
    # The loop exits through `(return @out)` exactly as JSON::Fast's
    # `parse-array` does.
    my sub parse-list(str $text, int $pos is rw) {
        my @out;
        nqp::bindattr(@out, List, '$!reified', my $buffer := nqp::create(IterationBuffer));
        skip-ws($text, $pos);
        nqp::while(
          1,
          nqp::stmts(
            (my $item := parse-item($text, $pos)),
            nqp::push($buffer, $item),
            skip-ws($text, $pos),
            nqp::if(
              nqp::iseq_i(nqp::ordat($text, $pos), 93),
              nqp::stmts(($pos = nqp::add_i($pos, 1)), (return @out))
            ),
            ($pos = nqp::add_i($pos, 1)),
            skip-ws($text, $pos)
          )
        )
    }

    my sub parse-item(str $text, int $pos is rw) {
        skip-ws($text, $pos);
        nqp::iseq_i(nqp::ordat($text, $pos), 91)
          ?? parse-list($text, $pos = nqp::add_i($pos, 1))
          !! parse-int($text, $pos)
    }

    our sub parse(Str $text) {
        my int $pos = 0;
        my $v := parse-item($text, $pos);
        ($v, $pos)
    }

    # A linked call in a loop, whose callee reads the block's `my`.
    our sub count-ws(Str $text) {
        my int $pos = 0;
        my int $n = 0;
        nqp::while(
          nqp::islt_i($pos, nqp::chars($text)),
          nqp::stmts(
            (my int $before = $pos),
            skip-ws($text, $pos),
            ($n = nqp::add_i($n, nqp::sub_i($pos, $before))),
            ($pos = nqp::add_i($pos, 1))
          )
        );
        $n
    }
}

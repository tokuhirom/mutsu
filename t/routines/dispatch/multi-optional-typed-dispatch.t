use Test;

plan 2;

multi sub choose(&code) { 'helper' }
multi sub choose(Blob $payload, Blob $extra?) { 'blob' }

is choose(Buf.new(0x41)), 'blob', 'typed optional candidate wins over helper multi';

# An omitted optional argument still participates in method candidate
# narrowness.  UInt is the builtin subset of Int, so it breaks the tie between
# these two otherwise-applicable optional candidates (Holidays::US::Federal
# uses this shape for Date::Event.etype).
{
    class OptionalMethodDispatch {
        multi method choose(Str $value?) { 'str' }
        multi method choose(UInt $value?) { 'uint' }
    }

    is OptionalMethodDispatch.new.choose, 'uint',
        'builtin subset narrows an optional method candidate with no argument';
}

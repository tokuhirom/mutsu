# Math::Interval Range interoperability

User-defined `infix:<cmp>` overloads now share the native structural Range
implementation after a candidate declines. Range subclasses such as
`Math::Interval` therefore compare with plain ranges by their bounds instead of
falling through to numeric coercion. Mixed arithmetic uses the same inherited
Range bounds, numeric allomorph divisors such as `<4>` work correctly, and a
Range subclass without `Numeric` no longer has its coercion failure swallowed.

Pinned by `t/routines/infix-routine-form-range-cmp.t`, found while measuring
Math::Interval 0.0.3.

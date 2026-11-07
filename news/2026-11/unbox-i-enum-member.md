# `nqp::unbox_i` of an integer enum member is its value

`nqp::unbox_i(D)` for `enum E <A B C D>` answered 0 (it fell through to the address-bearing-object
arm), while `unbox_u` answered 3. Upstream NativeCall's `CArray[int32]` stores through
`nqp::unbox_i`, so `CArray[int32].new: FFTW_REDFT00, FFTW_REDFT00` held zeros and
Math::FFT::Libfftw3 planned an R2HC transform instead of REDFT00 (`t/08-raw-adv`). The integer
enum value is now unboxed.

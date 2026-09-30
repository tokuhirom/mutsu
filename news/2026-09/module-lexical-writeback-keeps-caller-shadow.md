# Module lexical writes keep caller shadows intact

Calls to a module's exported subroutine no longer replay writes to the module's file-scope lexical into a caller's same-named local variable. The positional and zero-argument compiled call paths now use the same compunit and mainline lexical writeback guards as the named and typed call paths.

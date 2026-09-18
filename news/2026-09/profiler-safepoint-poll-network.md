The VM safepoint network now has a consumer-neutral `vm_poll` entry point. GC remains the first
consumer, while `MUTSU_PROFILE=1` can arm the profiler side without changing the interpreter's
execution model. JIT-generated backedges carry their bytecode instruction pointer through the
native helper, ready for the sampler to consume in the next profiler slice.

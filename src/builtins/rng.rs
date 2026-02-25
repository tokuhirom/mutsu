use std::cell::RefCell;

// ── Thread-local RNG (xoshiro256**) ────────────────────────────────────
thread_local! {
    static RNG: RefCell<Xoshiro256StarStar> = RefCell::new(Xoshiro256StarStar::from_time());
}

struct Xoshiro256StarStar {
    s: [u64; 4],
}

impl Xoshiro256StarStar {
    fn from_time() -> Self {
        use std::collections::hash_map::DefaultHasher;
        use std::hash::{Hash, Hasher};
        let mut s = [0u64; 4];
        for (i, slot) in s.iter_mut().enumerate() {
            let mut h = DefaultHasher::new();
            #[cfg(not(target_arch = "wasm32"))]
            {
                std::time::SystemTime::now().hash(&mut h);
                std::thread::current().id().hash(&mut h);
            }
            #[cfg(target_arch = "wasm32")]
            {
                // Use a fixed seed offset per slot in WASM (no system time or threads)
                (0xdeadbeef_u64.wrapping_add(i as u64 * 0x12345)).hash(&mut h);
            }
            (i as u64).hash(&mut h);
            *slot = h.finish();
            if *slot == 0 {
                *slot = 0xdeadbeef;
            }
        }
        Self { s }
    }

    fn from_seed(seed: u64) -> Self {
        // Use splitmix64 to initialize state from a single seed
        let mut state = seed;
        let mut s = [0u64; 4];
        for slot in &mut s {
            state = state.wrapping_add(0x9e3779b97f4a7c15);
            let mut z = state;
            z = (z ^ (z >> 30)).wrapping_mul(0xbf58476d1ce4e5b9);
            z = (z ^ (z >> 27)).wrapping_mul(0x94d049bb133111eb);
            *slot = z ^ (z >> 31);
            if *slot == 0 {
                *slot = 1;
            }
        }
        Self { s }
    }

    fn next_u64(&mut self) -> u64 {
        let result = (self.s[1].wrapping_mul(5)).rotate_left(7).wrapping_mul(9);
        let t = self.s[1] << 17;
        self.s[2] ^= self.s[0];
        self.s[3] ^= self.s[1];
        self.s[1] ^= self.s[2];
        self.s[0] ^= self.s[3];
        self.s[2] ^= t;
        self.s[3] = self.s[3].rotate_left(45);
        result
    }

    fn next_f64(&mut self) -> f64 {
        // Generate a float in [0, 1)
        (self.next_u64() >> 11) as f64 / (1u64 << 53) as f64
    }
}

/// Return a random float in [0, 1)
pub(crate) fn builtin_rand() -> f64 {
    RNG.with(|rng| rng.borrow_mut().next_f64())
}

/// Seed the RNG
pub(crate) fn builtin_srand(seed: u64) {
    RNG.with(|rng| {
        *rng.borrow_mut() = Xoshiro256StarStar::from_seed(seed);
    });
}

/// Seed the RNG with a time-based seed
pub(crate) fn builtin_srand_auto() {
    RNG.with(|rng| {
        *rng.borrow_mut() = Xoshiro256StarStar::from_time();
    });
}

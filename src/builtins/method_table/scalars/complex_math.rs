/// Complex trig function dispatch: returns (real, imag)
pub(crate) fn complex_trig(method: &str, a: f64, b: f64) -> (f64, f64) {
    match method {
        "sin" => (a.sin() * b.cosh(), a.cos() * b.sinh()),
        "cos" => (a.cos() * b.cosh(), -(a.sin() * b.sinh())),
        "tan" => complex_div(complex_trig("sin", a, b), complex_trig("cos", a, b)),
        "asin" => {
            // asin(z) = -i * ln(iz + sqrt(1 - z^2))
            let (re, im) = complex_asin(a, b);
            (re, im)
        }
        "acos" => {
            // acos(z) = pi/2 - asin(z)
            let (re, im) = complex_asin(a, b);
            (std::f64::consts::FRAC_PI_2 - re, -im)
        }
        "atan" => {
            // atan(z) = i/2 * ln((1-iz)/(1+iz))
            let (re, im) = complex_atan(a, b);
            (re, im)
        }
        "sec" => complex_recip(complex_trig("cos", a, b)),
        "cosec" => complex_recip(complex_trig("sin", a, b)),
        "cotan" => complex_div(complex_trig("cos", a, b), complex_trig("sin", a, b)),
        "asec" => complex_trig("acos", complex_recip((a, b)).0, complex_recip((a, b)).1),
        "acosec" => complex_trig("asin", complex_recip((a, b)).0, complex_recip((a, b)).1),
        "acotan" => complex_trig("atan", complex_recip((a, b)).0, complex_recip((a, b)).1),
        "sinh" => (a.sinh() * b.cos(), a.cosh() * b.sin()),
        "cosh" => (a.cosh() * b.cos(), a.sinh() * b.sin()),
        "tanh" => complex_div(complex_trig("sinh", a, b), complex_trig("cosh", a, b)),
        "sech" => complex_recip(complex_trig("cosh", a, b)),
        "cosech" => complex_recip(complex_trig("sinh", a, b)),
        "cotanh" => complex_div(complex_trig("cosh", a, b), complex_trig("sinh", a, b)),
        "asinh" => {
            // asinh(z) = ln(z + sqrt(z^2 + 1))
            let (z2r, z2i) = complex_mul((a, b), (a, b));
            let (sr, si) = complex_sqrt(z2r + 1.0, z2i);
            complex_ln(a + sr, b + si)
        }
        "acosh" => {
            // acosh(z) = ln(z + sqrt(z^2 - 1))
            let (z2r, z2i) = complex_mul((a, b), (a, b));
            let (sr, si) = complex_sqrt(z2r - 1.0, z2i);
            complex_ln(a + sr, b + si)
        }
        "atanh" => {
            // atanh(z) = 1/2 * ln((1+z)/(1-z))
            let num = (1.0 + a, b);
            let den = (1.0 - a, -b);
            let (qr, qi) = complex_div(num, den);
            let (lr, li) = complex_ln(qr, qi);
            (0.5 * lr, 0.5 * li)
        }
        "asech" => complex_trig("acosh", complex_recip((a, b)).0, complex_recip((a, b)).1),
        "acosech" => complex_trig("asinh", complex_recip((a, b)).0, complex_recip((a, b)).1),
        "acotanh" => complex_trig("atanh", complex_recip((a, b)).0, complex_recip((a, b)).1),
        _ => (f64::NAN, 0.0),
    }
}

fn complex_mul(a: (f64, f64), b: (f64, f64)) -> (f64, f64) {
    (a.0 * b.0 - a.1 * b.1, a.0 * b.1 + a.1 * b.0)
}

fn complex_div(a: (f64, f64), b: (f64, f64)) -> (f64, f64) {
    let denom = b.0 * b.0 + b.1 * b.1;
    if denom == 0.0 {
        return (f64::NAN, f64::NAN);
    }
    (
        (a.0 * b.0 + a.1 * b.1) / denom,
        (a.1 * b.0 - a.0 * b.1) / denom,
    )
}

fn complex_recip(z: (f64, f64)) -> (f64, f64) {
    complex_div((1.0, 0.0), z)
}

fn complex_sqrt(re: f64, im: f64) -> (f64, f64) {
    let r = (re * re + im * im).sqrt();
    let sr = ((r + re) / 2.0).sqrt();
    let si = ((r - re) / 2.0).sqrt();
    (sr, if im >= 0.0 { si } else { -si })
}

fn complex_ln(re: f64, im: f64) -> (f64, f64) {
    let r = (re * re + im * im).sqrt();
    (r.ln(), im.atan2(re))
}

fn complex_asin(a: f64, b: f64) -> (f64, f64) {
    // asin(z) = -i * ln(iz + sqrt(1 - z^2))
    let iz = (-b, a); // i * z
    let (z2r, z2i) = complex_mul((a, b), (a, b));
    let (sr, si) = complex_sqrt(1.0 - z2r, -z2i);
    let (lr, li) = complex_ln(iz.0 + sr, iz.1 + si);
    // -i * (lr + li*i) = li + (-lr)*i => (li, -lr)
    (li, -lr)
}

fn complex_atan(a: f64, b: f64) -> (f64, f64) {
    // atan(z) = i/2 * ln((1-iz)/(1+iz))
    let iz = (-b, a);
    let num = (1.0 - iz.0, -iz.1);
    let den = (1.0 + iz.0, iz.1);
    let (qr, qi) = complex_div(num, den);
    let (lr, li) = complex_ln(qr, qi);
    // i/2 * (lr + li*i) = (i*lr + i*li*i)/2 = (-li/2 + lr/2*i) => (-li/2, lr/2)
    (-li / 2.0, lr / 2.0)
}

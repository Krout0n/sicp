fn simpthon(f:fn(f32) -> f32, a:i32, b:i32, n:i32) {
    let h: f32 = (b - a) as f32 / n as f32;
    if a > b { 0.0 }
    fn iter(k:i32, product:f32) {
        let y = f((a as f32) + (k as f32) * h);
        match k {
            0 => iter(k+1, product + y),
            n => product + y,
           (k % 2 == 0) => iter(k+1, product + 4.0 * y),
            _ => iter(k+1, product + 2.0 * y)
        }
    }
    iter(0, 0.0);
}

fn main() {
    let cube = |x| x*x*x;
    println("{}", simpthon(cube, 0, 1, 100));
}

use rayon::prelude::*;

fn listes_premiers(n : u64) -> Vec<u64> {
    let is_prime = |n: u64| -> bool {
        for i in 2..=((n as f64).sqrt().ceil() as u64) {
            if n != i && n % i == 0 {
                return false;
            }
        }
        return true;
    };

    return (2..=n).filter(|n: &u64| is_prime(*n)).collect();
}

fn hamming(n : u64) -> impl Fn(u64) -> bool {
    let premiers = listes_premiers(n);

    let prime_hamming_div = move |n: u64| {
        for i in &premiers[1..] {
            if n % i == 0 {
                return *i;
            }
        }
        return 1;
    };

    move |mut num : u64| -> bool {
        while num % 2 == 0 {
            num /= 2;
        }

        loop {
            if num == 1 {
                return true;
            }

            let div : u64 = prime_hamming_div(num);
            if div == 1 {
                return false;
            } else {
                num /= div;
            }
        }
    }
}

fn main() {

    let n = 10;
    let N = 1000000000;

    let hamming_test = hamming(n);
    let count = (1..=N).into_par_iter().filter(|n : &u64| hamming_test(*n)).count();
    println!("nombres de hamming : {}", count);
}

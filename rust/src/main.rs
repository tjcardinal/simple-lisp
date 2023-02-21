mod eval;
mod expression;
mod parse;

fn main() {
    let mut env = eval::default_env();
    loop {
        let mut input = String::new();
        std::io::stdin().read_line(&mut input).unwrap();

        match parse::parse(&parse::tokenize(&input)) {
            Ok((parse, [])) => {
                println!("{:?}", parse);
                let eval = eval::eval(&mut env, &parse);
                println!("{:?}", eval);
            }
            Ok((_, r)) => println!("Remainder: {:?}", r),
            Err(e) => println!("Error: {:?}", e),
        }
    }
}

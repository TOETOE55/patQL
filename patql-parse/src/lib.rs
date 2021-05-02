pub mod token;
pub mod ast;
pub mod parse;
pub mod check;

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}

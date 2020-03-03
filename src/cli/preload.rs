use pat_ql::ast::{arr, slice, string, unit, var, Decl, Pat};

fn compares() -> Vec<Decl> {
    fn lt(pat: &Pat) -> bool {
        match pat {
            Pat::Arr(ps, _) => match ps.as_slice() {
                [Pat::Num(x), Pat::Num(y), ..] => x < y,
                _ => false,
            },
            _ => false,
        }
    }

    fn lte(pat: &Pat) -> bool {
        match pat {
            Pat::Arr(ps, _) => match ps.as_slice() {
                [Pat::Num(x), Pat::Num(y), ..] => x <= y,
                _ => false,
            },
            _ => false,
        }
    }

    fn gt(pat: &Pat) -> bool {
        match pat {
            Pat::Arr(ps, _) => match ps.as_slice() {
                [Pat::Num(x), Pat::Num(y), ..] => x > y,
                _ => false,
            },
            _ => false,
        }
    }

    fn gte(pat: &Pat) -> bool {
        match pat {
            Pat::Arr(ps, _) => match ps.as_slice() {
                [Pat::Num(x), Pat::Num(y), ..] => x >= y,
                _ => false,
            },
            _ => false,
        }
    }

    let less = arr(vec![string("<"), var("x"), var("y")])
        .expand_to(arr(vec![var("x"), var("y")]).filter("lt", lt));
    let greater = arr(vec![string(">"), var("y"), var("y")])
        .expand_to(arr(vec![var("x"), var("y")]).filter("gt", gt));
    let lesse = arr(vec![string("<="), var("y"), var("y")])
        .expand_to(arr(vec![var("x"), var("y")]).filter("lte", lte));
    let greatere = arr(vec![string(">="), var("y"), var("y")])
        .expand_to(arr(vec![var("x"), var("y")]).filter("gte", gte));
    vec![less, greater, lesse, greatere]
}

pub fn pre() -> Vec<Decl> {
    compares()
}

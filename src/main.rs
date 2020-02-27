use query::ast::{comp, comp_rest, num, qtrue, string, var};
use query::qeval::{query as qry, run_query, Dict, DictStream};

fn main() {
    //    let p1 = comp_rest(vec![var("x")], "x");
    //    // [?x, ?x...]
    //    let p2 = comp(vec![comp(vec![num(1), var("y")]), var("z"), num(2)]);
    //    // [[1,2], 1, 2]
    //    let p3 = comp(vec![comp(vec![num(1)]), comp(vec![]), comp(vec![num(1)]),]);
    //    // [[1, 2], [], ?x]
    //    let p4 = comp(vec![
    //        comp_rest(vec![var("u")], "v"),
    //        var("y"),
    //        comp_rest(vec![var("u")], "z"),
    //    ]);
    //    //[[?u, ?v..], ?y, [?u, ?z..]]
    //    let p5 = comp(vec![
    //        var("v"),
    //        var("y"),
    //        var("z"),
    //    ]);
    //
    //    let mut dict = Dict::default();
    //    let res = dict.unify(&p3, &p4);
    //    let res = dict.unify(&p5, &p4.clone().datum().rename().conclusion);
    //    println!("{}, {}, {}", p3, p4, p5);
    //    println!("{:?}: {:?}", res, dict);
    //    match dict.subst(&p3) {
    //        Ok(x) => println!("{}", x),
    //        Err(err) => println!("{:?}", err),
    //    }

    let rule1 = comp(vec![string("append"), comp(vec![]), var("y"), var("y")]).assert_as(qtrue());
    let rule2 = comp(vec![
        string("append"),
        comp_rest(vec![var("u")], "v"),
        var("y"),
        comp_rest(vec![var("u")], "z"),
    ])
    .assert_as(comp(vec![string("append"), var("v"), var("y"), var("z")]).q());

    let mut db = vec![rule1, rule2];
    let qry1 = comp(vec![
        string("append"),
        var("x"),
        var("y"),
        comp(vec![num(1), num(2), num(3), num(4)]),
    ])
    .q();

    println!(":> {}", qry1);
    let result = run_query(&qry1, &db);
    for query in result {
        println!("{}", query);
    }
}

use query::exp::{arr, num, qtrue, run_query, slice, string, var, Assertions};

fn main() {
    let rule1 = arr(vec![string("append"), arr(vec![]), var("y"), var("y")]).assert_as(qtrue());
    let rule2 = arr(vec![
        string("append"),
        slice(vec![var("u")], "v"),
        var("y"),
        slice(vec![var("u")], "z"),
    ])
    .assert_as(arr(vec![string("append"), var("v"), var("y"), var("z")]).q());

    let db = Assertions::new(vec![rule2, rule1]);
    let qry1 = arr(vec![
        string("append"),
        var("x"),
        var("y"),
        arr(vec![num(1), num(2), num(3), num(4)]),
    ])
    .q();

    println!(":> {}", qry1);
    let result = run_query(qry1, &db);
    for query in result {
        println!("{}", query);
    }
}

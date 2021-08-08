use crate::ast::{BuiltinPredicate, Decl, ExprPat, File, ForallPredicate, Pat, Predicate, Rule};
use lens_rs::*;

pub trait Visitor<'ast> {
    fn visit_file(&mut self, file: &'ast File) {
        for decl in &file.decls {
            self.visit_decl(decl);
        }
    }

    fn visit_decl(&mut self, decl: &'ast Decl) {
        self.visit_rule(&decl.rule);
        self.visit_predicate(&decl.predicate);
    }

    fn visit_predicate(&mut self, predicate: &'ast Predicate) {
        None.into_iter()
            .chain(predicate.traverse_ref(optics!(Builtin.EQ._both)))
            .chain(predicate.traverse_ref(optics!(Builtin.NE._both)))
            .chain(predicate.traverse_ref(optics!(Builtin.GT._both)))
            .chain(predicate.traverse_ref(optics!(Builtin.GE._both)))
            .chain(predicate.traverse_ref(optics!(Builtin.LE._both)))
            .chain(predicate.traverse_ref(optics!(Builtin.LT._both)))
            .chain(predicate.preview_ref(optics!(Builtin.Println)))
            .for_each(|pat| self.visit_pat(pat));

        None.into_iter()
            .chain(predicate.traverse_ref(optics!(All._mapped)))
            .chain(predicate.traverse_ref(optics!(Any._mapped)))
            .chain(predicate.preview_ref(optics!(Forall.predicate._box)))
            .chain(predicate.preview_ref(optics!(Neg._box)))
            .for_each(|p| self.visit_predicate(p));

        predicate
            .preview_ref(optics!(Simple))
            .map(|r| self.visit_rule(r));
    }

    fn visit_rule(&mut self, rule: &'ast Rule) {
        self.visit_pat(&rule.args)
    }

    fn visit_pat(&mut self, pat: &'ast Pat) {
        std::iter::empty()
            .chain(pat.traverse_ref(optics!(Arr.items._mapped)))
            .chain(pat.traverse_ref(optics!(Expr.Add._both._box)))
            .chain(pat.traverse_ref(optics!(Expr.Sub._both._box)))
            .chain(pat.traverse_ref(optics!(Expr.Mul._both._box)))
            .chain(pat.traverse_ref(optics!(Expr.Div._both._box)))
            .chain(pat.traverse_ref(optics!(Expr.Rem._both._box)))
            .chain(pat.traverse_ref(optics!(Expr.And._both._box)))
            .chain(pat.traverse_ref(optics!(Expr.Or._both._box)))
            .chain(pat.traverse_ref(optics!(Expr.NE._both._box)))
            .chain(pat.traverse_ref(optics!(Expr.EQ._both._box)))
            .chain(pat.traverse_ref(optics!(Expr.GT._both._box)))
            .chain(pat.traverse_ref(optics!(Expr.GE._both._box)))
            .chain(pat.traverse_ref(optics!(Expr.LE._both._box)))
            .chain(pat.traverse_ref(optics!(Expr.LT._both._box)))
            .chain(pat.traverse_ref(optics!(Expr.Index._both._box)))
            .chain(pat.traverse_ref(optics!(Expr.Append._both._box)))
            .chain(pat.preview_ref(optics!(Expr.Pure._box)))
            .chain(pat.preview_ref(optics!(Expr.Neg._box)))
            .chain(pat.preview_ref(optics!(Expr.Not._box)))
            .for_each(|pat| self.visit_pat(pat));
    }
}

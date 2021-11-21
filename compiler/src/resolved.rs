
// use crate::*;

// pub enum ResolvedProgram {

// }

// pub struct ResolvedExpression<'a> {
//     result_type: &'a Type,
//     Ident(&'a Variable),
// }

// impl Expression {
//     pub fn resolve<'a>(&self, f: &'a FunctionDefinition) -> ResolvedExpression<'a>{
//         match self {
//             Expression::Ident(i) => 
//                 ResolvedExpression::Ident(f.var.get(i).unwrap()),
//             // Expression::Number(_, _) => true,
//             // Expression::TtyIn() => true,
//             // Expression::Arithmetic(_, left, right, var_type) => {
//             //     if left.resolve(ctxt) && right.resolve(ctxt) {
//             //         if var_type.is_some() {
//             //             true
//             //         } else {
//             //             let l = left.get_type(ctxt);
//             //             let r = right.get_type(ctxt);
//             //             let result_type = if l.byte_count(ctxt.program) > r.byte_count(ctxt.program) {
//             //                 l
//             //             } else {
//             //                 r
//             //             };
//             //             *var_type = Some(result_type.clone());
//             //             true
//             //         }
//             //     } else {
//             //         false
//             //     }
//             // }
//             // Expression::Comparison(c) => c.left.resolve(ctxt) && c.right.resolve(ctxt),
//             // Expression::Deref(e) => e.resolve(ctxt),
//             // Expression::LocalFieldDeref(var, field) |
//             // Expression::PtrFieldDeref(var, field)=> {
//             //     if let Some(v) = ctxt.function.variables.get(var) {
//             //         if let Type::Struct(s) = &v.var_type {
//             //             ctxt.program.types.get(s).is_some()
//             //         } else {
//             //             panic!();
//             //         }
//             //     } else { 
//             //         false
//             //     }
//             // },
//             // Expression::AddressOf(e) => todo!(),
//             // Expression::Index(var, e) => todo!(),
//             // Expression::Cast { old_type:_, new_type:_, value } => value.resolve(ctxt),
//             // Expression::Call(_) => todo!(),
//             // Expression::Optimized { original, optimized } => todo!(),
//             _ => todo!()
//         }
//     }
// }
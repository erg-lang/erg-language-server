use erg_common::traits::Stream;
use erg_compiler::hir::*;
use erg_compiler::erg_parser::token::Token;
use erg_compiler::ty::{HasType, Type};

/// Returns the smallest expression containing `token`. Literals, accessors, containers, etc. are returned.
pub(crate) fn visit_hir_t(hir: &HIR, token: &Token) -> Option<Type> {
    for chunk in hir.module.iter() {
        if let Some(expr) = visit_expr_t(chunk, token) {
            return Some(expr);
        }
    }
    None
}

fn visit_expr_t(expr: &Expr, token: &Token) -> Option<Type> {
    match expr {
        Expr::Lit(lit) => if &lit.token == token { Some(expr.t()) } else { None },
        Expr::Accessor(acc) => match acc {
            Accessor::Ident(ident) => if ident.name.token() == token { Some(expr.t()) } else { None },
            Accessor::Attr(attr) => {
                if attr.ident.name.token() == token {
                    Some(expr.t())
                } else {
                    visit_expr_t(&attr.obj, token)
                }
            },
        },
        Expr::BinOp(bin) => visit_bin_t(bin, token),
        Expr::UnaryOp(unary) => visit_expr_t(&unary.expr, token),
        Expr::Call(call) => visit_call_t(call, token),
        Expr::ClassDef(class_def) => visit_class_def_t(class_def, token),
        Expr::Def(def) => visit_block_t(&def.body.block, token),
        Expr::PatchDef(patch_def) => visit_patchdef_t(patch_def, token),
        Expr::Lambda(lambda) => visit_block_t(&lambda.body, token),
        Expr::Array(arr) => visit_array_t(arr, token),
        Expr::Dict(dict) => visit_dict_t(dict, token),
        Expr::Record(record) => visit_record_t(record, token),
        Expr::Set(set) => visit_set_t(set, token),
        Expr::Tuple(tuple) => visit_tuple_t(tuple, token),
        Expr::TypeAsc(type_asc) => visit_expr_t(&type_asc.expr, token),
        Expr::Import(_) | Expr::Compound(_) | Expr::AttrDef(_) | Expr::Code(_) => None,
    }
}

fn visit_bin_t(bin: &BinOp, token: &Token) -> Option<Type> {
    visit_expr_t(&bin.lhs, token).or_else(|| visit_expr_t(&bin.rhs, token))
}

fn visit_call_t(call: &Call, token: &Token) -> Option<Type> {
    if let Some(attr) = &call.attr_name {
        if attr.name.token() == token {
            return Some(attr.t());
        }
    }
    visit_expr_t(&call.obj, token).or_else(|| visit_args_t(&call.args, token))
}

fn visit_args_t(args: &Args, token: &Token) -> Option<Type> {
    for arg in args.pos_args.iter() {
        if let Some(expr) = visit_expr_t(&arg.expr, token) {
            return Some(expr);
        }
    }
    if let Some(var) = &args.var_args {
        if let Some(expr) = visit_expr_t(&var.expr, token) {
            return Some(expr);
        }
    }
    for arg in args.kw_args.iter() {
        if let Some(expr) = visit_expr_t(&arg.expr, token) {
            return Some(expr);
        }
    }
    None
}

fn visit_class_def_t(class_def: &ClassDef, token: &Token) -> Option<Type> {
    visit_expr_t(&class_def.require_or_sup, token)
        .or_else(|| visit_block_t(&class_def.methods, token))
}

fn visit_block_t(block: &Block, token: &Token) -> Option<Type> {
    for chunk in block.iter() {
        if let Some(expr) = visit_expr_t(chunk, token) {
            return Some(expr);
        }
    }
    None
}

fn visit_patchdef_t(patch_def: &PatchDef, token: &Token) -> Option<Type> {
    visit_expr_t(&patch_def.base, token)
        .or_else(|| visit_block_t(&patch_def.methods, token))
}

fn visit_array_t(arr: &Array, token: &Token) -> Option<Type> {
    match arr {
        Array::Normal(arr) => visit_args_t(&arr.elems, token),
        _ => None, // todo!(),
    }
}

fn visit_dict_t(dict: &Dict, token: &Token) -> Option<Type> {
    match dict {
        Dict::Normal(dict) => {
            for kv in &dict.kvs {
                if let Some(expr) = visit_expr_t(&kv.key, token) {
                    return Some(expr);
                } else if let Some(expr) = visit_expr_t(&kv.value, token) {
                    return Some(expr);
                }
            }
            None
        }
        _ => None, // todo!(),
    }
}

fn visit_record_t(record: &Record, token: &Token) -> Option<Type> {
    for field in record.attrs.iter() {
        if let Some(expr) = visit_block_t(&field.body.block, token) {
            return Some(expr);
        }
    }
    None
}

fn visit_set_t(set: &Set, token: &Token) -> Option<Type> {
    match set {
        Set::Normal(set) => visit_args_t(&set.elems, token),
        _ => None, // todo!(),
    }
}

fn visit_tuple_t(tuple: &Tuple, token: &Token) -> Option<Type> {
    match tuple {
        Tuple::Normal(tuple) => visit_args_t(&tuple.elems, token),
        // _ => None, // todo!(),
    }
}

#[allow(unused)]
/// Returns the smallest expression containing `token`. Literals, accessors, containers, etc. are returned.
pub(crate) fn visit_hir<'h>(hir: &'h HIR, token: &Token) -> Option<&'h Expr> {
    for chunk in hir.module.iter() {
        if let Some(expr) = visit_expr(chunk, token) {
            return Some(expr);
        }
    }
    None
}

fn visit_expr<'e>(expr: &'e Expr, token: &Token) -> Option<&'e Expr> {
    match expr {
        Expr::Lit(lit) => if &lit.token == token { Some(expr) } else { None },
        Expr::Accessor(acc) => match acc {
            Accessor::Ident(ident) => if ident.name.token() == token { Some(expr) } else { None },
            Accessor::Attr(attr) => {
                if attr.ident.name.token() == token {
                    Some(expr)
                } else {
                    visit_expr(&attr.obj, token)
                }
            },
        },
        Expr::BinOp(bin) => visit_bin(bin, token),
        Expr::UnaryOp(unary) => visit_expr(&unary.expr, token),
        Expr::Call(call) => visit_call(call, token),
        Expr::ClassDef(class_def) => visit_class_def(class_def, token),
        Expr::Def(def) => visit_block(&def.body.block, token),
        Expr::PatchDef(patch_def) => visit_patchdef(patch_def, token),
        Expr::Lambda(lambda) => visit_block(&lambda.body, token),
        Expr::Array(arr) => visit_array(arr, token),
        Expr::Dict(dict) => visit_dict(dict, token),
        Expr::Record(record) => visit_record(record, token),
        Expr::Set(set) => visit_set(set, token),
        Expr::Tuple(tuple) => visit_tuple(tuple, token),
        Expr::TypeAsc(type_asc) => visit_expr(&type_asc.expr, token),
        Expr::Import(_) | Expr::Compound(_) | Expr::AttrDef(_) | Expr::Code(_) => None,
    }
}

fn visit_bin<'b>(bin: &'b BinOp, token: &Token) -> Option<&'b Expr> {
    visit_expr(&bin.lhs, token).or_else(|| visit_expr(&bin.rhs, token))
}

fn visit_call<'c>(call: &'c Call, token: &Token) -> Option<&'c Expr> {
    visit_expr(&call.obj, token).or_else(|| visit_args(&call.args, token))
}

fn visit_args<'a>(args: &'a Args, token: &Token) -> Option<&'a Expr> {
    for arg in args.pos_args.iter() {
        if let Some(expr) = visit_expr(&arg.expr, token) {
            return Some(expr);
        }
    }
    if let Some(var) = &args.var_args {
        if let Some(expr) = visit_expr(&var.expr, token) {
            return Some(expr);
        }
    }
    for arg in args.kw_args.iter() {
        if let Some(expr) = visit_expr(&arg.expr, token) {
            return Some(expr);
        }
    }
    None
}

fn visit_class_def<'c>(class_def: &'c ClassDef, token: &Token) -> Option<&'c Expr> {
    visit_expr(&class_def.require_or_sup, token)
        .or_else(|| visit_block(&class_def.methods, token))
}

fn visit_block<'b>(block: &'b Block, token: &Token) -> Option<&'b Expr> {
    for chunk in block.iter() {
        if let Some(expr) = visit_expr(chunk, token) {
            return Some(expr);
        }
    }
    None
}

fn visit_patchdef<'p>(patch_def: &'p PatchDef, token: &Token) -> Option<&'p Expr> {
    visit_expr(&patch_def.base, token)
        .or_else(|| visit_block(&patch_def.methods, token))
}

fn visit_array<'a>(arr: &'a Array, token: &Token) -> Option<&'a Expr> {
    match arr {
        Array::Normal(arr) => visit_args(&arr.elems, token),
        _ => None, // todo!(),
    }
}

fn visit_dict<'d>(dict: &'d Dict, token: &Token) -> Option<&'d Expr> {
    match dict {
        Dict::Normal(dict) => {
            for kv in &dict.kvs {
                if let Some(expr) = visit_expr(&kv.key, token) {
                    return Some(expr);
                } else if let Some(expr) = visit_expr(&kv.value, token) {
                    return Some(expr);
                }
            }
            None
        }
        _ => None, // todo!(),
    }
}

fn visit_record<'r>(record: &'r Record, token: &Token) -> Option<&'r Expr> {
    for field in record.attrs.iter() {
        if let Some(expr) = visit_block(&field.body.block, token) {
            return Some(expr);
        }
    }
    None
}

fn visit_set<'s>(set: &'s Set, token: &Token) -> Option<&'s Expr> {
    match set {
        Set::Normal(set) => visit_args(&set.elems, token),
        _ => None, // todo!(),
    }
}

fn visit_tuple<'t>(tuple: &'t Tuple, token: &Token) -> Option<&'t Expr> {
    match tuple {
        Tuple::Normal(tuple) => visit_args(&tuple.elems, token),
        // _ => None, // todo!(),
    }
}
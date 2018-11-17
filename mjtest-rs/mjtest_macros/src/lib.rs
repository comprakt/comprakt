#![feature(proc_macro_hygiene)]

extern crate mjtest;
extern crate proc_macro;
extern crate proc_macro2;
extern crate quote;
extern crate syn;

use mjtest::SyntaxTestCase;
use proc_macro2::{Ident, Span, TokenStream, TokenTree};
use quote::{quote, ToTokens};
use std::{collections::HashSet, iter::FromIterator};

#[proc_macro]
pub fn gen_syntax_tests(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    // parse arguments
    let input: TokenStream = input.into();
    let expr: syn::Expr = syn::parse2(input.clone()).expect("group");
    let t: syn::ExprTuple = match expr {
        syn::Expr::Tuple(t) => t,
        x => panic!("expected 2-tuple expression as arugment, got {:?}", x),
    };

    let params: Vec<syn::Expr> = t.elems.into_iter().collect();
    if params.len() != 2 {
        panic!("expected 2-tuple, got tuple of length {:?}", params);
    }
    let handler = match &params[0] {
        syn::Expr::Path(p) => p,
        x => panic!("expected function name literalm got {:?}", x),
    };
    let releaseonly_array = match &params[1] {
        syn::Expr::Array(a) => &a.elems,
        x => panic!("expected array of string literals, got {:?}", x),
    };
    let releaseonly: HashSet<String> =
        HashSet::from_iter(releaseonly_array.into_iter().map(|l| match l {
            syn::Expr::Lit(syn::ExprLit {
                lit: syn::Lit::Str(l),
                ..
            }) => l.value(),
            x => panic!("expected string literal, got {:?}", x),
        }));

    // parse test cases
    let cases = SyntaxTestCase::all().expect("could not load test cases");

    // generate test cases
    let mut out = proc_macro2::TokenStream::new();
    for case in cases {
        let mut tcdef = proc_macro2::TokenStream::new();
        case.to_tokens(&mut tcdef);
        let ignore = if releaseonly.contains(case.file_name()) && cfg!(debug_assertions) {
            quote!{ #[ignore] }
        } else {
            quote!{}
        };
        let test_name = TokenTree::Ident(Ident::new(&case.test_name(), Span::call_site()));
        let mut handler_toks = proc_macro2::TokenStream::new();
        handler.to_tokens(&mut handler_toks);
        let test = quote!{
            #ignore
            #[test]
            fn #test_name() {
                let tc = #tcdef;
                let handler = #handler_toks;
                handler(&tc);
            }
        };
        out.extend(test);
    }
    out.into()
}

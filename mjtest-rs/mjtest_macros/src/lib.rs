#![feature(proc_macro_hygiene)]

extern crate mjtest;
extern crate proc_macro;
extern crate proc_macro2;
extern crate quote;
extern crate syn;

use mjtest::{SemanticTestCase, SyntaxTestCase};
use proc_macro2::{Ident, Span, TokenStream, TokenTree};
use quote::quote;
use std::{collections::HashSet, iter::FromIterator};

struct GenArgs {
    handler: syn::ExprPath,
    release_only: HashSet<String>, // list of file names (no path components!)
}

trait TestCase: quote::ToTokens {
    fn file_name(&self) -> &str;
    fn test_name(&self) -> String;
}

impl<T> TestCase for T
where
    T: quote::ToTokens + std::ops::Deref<Target = mjtest::SyntaxAndSemanticFilePath>,
{
    fn test_name(&self) -> String {
        let fp: &mjtest::SyntaxAndSemanticFilePath = self;
        let prefix = match fp {
            mjtest::SyntaxAndSemanticFilePath::Valid(_) => "valid",
            mjtest::SyntaxAndSemanticFilePath::Invalid(_) => "invalid",
        };
        let escaped_name = self.file_name().replace(".", "__");
        format!("mjtest_{}_{}", prefix, escaped_name)
    }
    fn file_name(&self) -> &str {
        let fp: &mjtest::SyntaxAndSemanticFilePath = self;
        fp.file_name()
    }
}

impl GenArgs {
    // FIXME TryFrom
    fn must_from_token_stream(input: proc_macro::TokenStream) -> GenArgs {
        let input: TokenStream = input.into();
        let expr: syn::Expr = syn::parse2(input.clone()).expect("group");
        let t: syn::ExprTuple = match expr {
            syn::Expr::Tuple(t) => t,
            x => panic!("expected 2-tuple expression as arugment, got {:?}", x),
        };

        let mut params = t.elems.into_iter();
        if params.len() != 2 {
            panic!("expected 2-tuple, got tuple of length {:?}", params.len());
        }
        let handler = match params.next().unwrap() {
            syn::Expr::Path(p) => p,
            x => panic!("expected function name literalm got {:?}", x),
        };
        let releaseonly_array = match params.next().unwrap() {
            syn::Expr::Array(a) => a.elems,
            x => panic!("expected array of string literals, got {:?}", x),
        };
        let release_only: HashSet<String> =
            HashSet::from_iter(releaseonly_array.into_iter().map(|l| match l {
                syn::Expr::Lit(syn::ExprLit {
                    lit: syn::Lit::Str(l),
                    ..
                }) => l.value(),
                x => panic!("expected string literal, got {:?}", x),
            }));

        GenArgs {
            handler,
            release_only,
        }
    }

    fn gen_testcase<T: TestCase>(&self, case: &T, out: &mut proc_macro2::TokenStream) {
        let mut tcdef = proc_macro2::TokenStream::new();
        case.to_tokens(&mut tcdef);
        let ignore = if self.release_only.contains(case.file_name()) && cfg!(debug_assertions) {
            quote! { #[ignore] }
        } else {
            quote! {}
        };
        let test_name = TokenTree::Ident(Ident::new(&case.test_name(), Span::call_site()));
        let handler = &self.handler;
        let test = quote! {
            #ignore
            #[test]
            fn #test_name() {
                let tc = #tcdef;
                let handler = #handler;
                handler(&tc);
            }
        };
        out.extend(test);
    }
}

#[proc_macro]
pub fn gen_syntax_tests(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let gen_args = GenArgs::must_from_token_stream(input);
    let cases = SyntaxTestCase::all().expect("could not load test cases");
    // generate test cases
    let mut out = proc_macro2::TokenStream::new();
    cases
        .into_iter()
        .for_each(|tc| gen_args.gen_testcase::<SyntaxTestCase>(&tc, &mut out));
    out.into()
}

#[proc_macro]
pub fn gen_semantic_tests(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let gen_args = GenArgs::must_from_token_stream(input);
    let cases = SemanticTestCase::all().expect("could not load test cases");
    // generate test cases
    let mut out = proc_macro2::TokenStream::new();
    cases
        .into_iter()
        .for_each(|tc| gen_args.gen_testcase(&tc, &mut out));
    out.into()
}

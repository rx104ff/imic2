// In logos-macros/src/lib.rs

extern crate proc_macro;

use proc_macro::TokenStream;
use quote::quote;
use syn::{
    braced, custom_keyword, parse::{Parse, ParseStream, Result}, parse_macro_input, DeriveInput, Expr, Ident, Token, Type
};


// This is the new derive macro. When you add `#[derive(JudgmentTraits)]` to a struct
// like `Judgment<V, T, R>`, this function will run and generate the necessary trait impls.
#[proc_macro_derive(JudgmentTraits)]
pub fn derive_judgment_traits(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = &input.ident;

    // These helpers capture the generic parameters from the struct definition
    // (e.g., <V, T, R>) so they can be used in the `impl` blocks.
    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();
    
    // We assume the generic parameters on the struct are named V, T, and R.
    let v_param = syn::Ident::new("V", proc_macro2::Span::call_site());
    let t_param = syn::Ident::new("T", proc_macro2::Span::call_site());
    let r_param = syn::Ident::new("R", proc_macro2::Span::call_site());

    // The `quote!` macro generates the final Rust code.
    let expanded = quote! {
        // Automatically generate `impl HasExpr for Judgment<V, T, R>`.
        // The `crate::` paths ensure this works correctly from any module.
        impl #impl_generics crate::system::judgment_traits::HasExpr for #name #ty_generics #where_clause {
            type V = #v_param;
            fn expr(&self) -> &crate::common::ast::expr::Expr<Self::V> { &self.expr }
        }

        // Automatically generate `impl HasEnv for Judgment<V, T, R>`.
        impl #impl_generics crate::system::judgment_traits::HasEnv for #name #ty_generics #where_clause {
            type V = #v_param;
            type T = #t_param;
            fn env(&self) -> &crate::common::ast::env::Env<Self::V, Self::T> { &self.env }
        }

        // Automatically generate `impl HasResult for Judgment<V, T, E>`.
        impl #impl_generics crate::system::judgment_traits::HasResult for #name #ty_generics #where_clause {
            type R = #r_param;
            fn result(&self) -> &Self::R { &self.result }
        }

        // Automatically generate `impl FromParts for Judgment<V, T, E>`.
        impl #impl_generics crate::system::judgment_traits::FromParts for #name #ty_generics #where_clause {
            type V = #v_param;
            type T = #t_param;
            type R = #r_param;
            fn from_parts(env: crate::common::ast::env::Env<Self::V, Self::T>, expr: crate::common::ast::expr::Expr<Self::V>, result: Self::R) -> Self {
                Self { env, expr, result }
            }
        }
    };

    TokenStream::from(expanded)
}

#[proc_macro]
pub fn define_system(input: TokenStream) -> TokenStream {
    // ---- PARSING LOGIC IS ENCAPSULATED INSIDE THE FUNCTION ----

    custom_keyword!(rule);
    custom_keyword!(axiom);
    custom_keyword!(judgment);
    custom_keyword!(context);

    struct RuleDefinition {
        rule_constructor: Expr,
        axiom_constructor: Option<Expr>,
    }

    struct SystemDefinition {
        system_name: Ident,
        rules: Vec<RuleDefinition>,
        judgment_type: Type,
        context_type: Type,
    }

    impl Parse for SystemDefinition {
        fn parse(input: ParseStream) -> Result<Self> {
            let _: Token![pub] = input.parse()?;
            let _: Token![struct] = input.parse()?;
            let system_name: Ident = input.parse()?;
            let content;
            braced!(content in input);

            let mut rules = Vec::new();
            while !content.is_empty() {
                content.parse::<rule>()?;
                let rule_constructor: Expr = content.parse()?;
                let axiom_constructor = if content.peek(Token![=>]) {
                    content.parse::<Token![=>]>()?;
                    content.parse::<axiom>()?;
                    // Parse the constructor expression for the axiom.
                    Some(content.parse()?)
                } else {
                    None
                };
            rules.push(RuleDefinition { rule_constructor, axiom_constructor });
                content.parse::<Token![;]>()?;
            }
            input.parse::<Token![,]>()?;
            input.parse::<judgment>()?;
            input.parse::<Token![:]>()?;
            let judgment_type: Type = input.parse()?;
            input.parse::<Token![,]>()?;
            input.parse::<context>()?;
            input.parse::<Token![:]>()?;
            let context_type: Type = input.parse()?;
            input.parse::<Option<Token![,]>>()?;
            Ok(SystemDefinition {
                system_name,
                rules,
                judgment_type,
                context_type,
            })
        }
    }

    // ---- MACRO EXPANSION LOGIC ----

    let def = parse_macro_input!(input as SystemDefinition);

    let system_name = &def.system_name;
    let judgment_type = &def.judgment_type;
    let context_type = &def.context_type;

    let provider_types = def.rules.iter().map(|_| {
        let rule_trait_obj = quote! { Box<dyn crate::system::core::Rule<#judgment_type, #context_type>> };
        let axiom_trait_obj = quote! { Option<Box<dyn crate::system::core::Axiom<#judgment_type, #context_type>>> };
        quote! { (#rule_trait_obj, #axiom_trait_obj) }
    });

    let provider_initializers = def.rules.iter().map(|r| {
        let rule_init = &r.rule_constructor;
        let rule_boxed = quote! { Box::new(#rule_init) };
        let axiom_boxed = match &r.axiom_constructor {
            Some(axiom_init) => quote! { Some(Box::new(#axiom_init)) },
            None => quote! { None },
        };
        quote! { (#rule_boxed, #axiom_boxed) }
    });

    let mut derive_body = quote! {};
    for (i, _rule) in def.rules.iter().enumerate() {
        let index = syn::Index::from(i);
        // The logic for calling `.apply()` and `.axiom()` on the trait objects remains the same.
        let axiom_check_code = quote! {
            if let Some(axiom_provider) = axiom_provider_opt {
                if let Some(axiom_result) = axiom_provider.axiom(ctx, judgment) {
                   derived_premises.push(axiom_result?);
                }
            }
        };
        derive_body.extend(quote! {
            {
                let (rule_provider, axiom_provider_opt) = &self.providers.#index;
                if let Some(rule_result) = rule_provider.apply(ctx, judgment) {
                    let (rule_name, recursive_premises) = rule_result?;
                    let mut derived_premises = Vec::new();
                    for premise_judgment in recursive_premises {
                        derived_premises.push(self.derive(ctx, &premise_judgment)?);
                    }
                    #axiom_check_code
                    return Ok(crate::common::proof::Derivation {
                        judgment: judgment.clone(),
                        rule: rule_name,
                        premises: derived_premises,
                    });
                }
            }
        });
    }

    let expanded = quote! {
        pub struct #system_name {
            pub providers: (
                #(#provider_types,)*
            ),
        }

        impl crate::system::core::System<#judgment_type, #context_type> for #system_name {
            fn derive(&self, ctx: &mut #context_type, judgment: &#judgment_type) -> Result<crate::common::proof::Derivation<#judgment_type>, String> {
                use crate::system::core::{Axiom, Rule};
                #derive_body
                Err(format!("No rule found to derive the judgment for: {:?}", judgment))
            }
        }

        impl #system_name {
            pub fn new() -> Self {
                Self {
                    providers: (
                        #(#provider_initializers,)*
                    )
                }
            }
        }
    };

    TokenStream::from(expanded)
}

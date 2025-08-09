// In logos-macros/src/lib.rs

extern crate proc_macro;

use proc_macro::TokenStream;
use quote::quote;
use syn::{
    braced, custom_keyword,
    parse::{Parse, ParseStream, Result},
    parse_macro_input, Ident, Token, Type,
};

#[proc_macro]
pub fn define_system(input: TokenStream) -> TokenStream {
    // ---- PARSING LOGIC IS ENCAPSULATED INSIDE THE FUNCTION ----

    custom_keyword!(rule);
    custom_keyword!(axiom);
    custom_keyword!(judgment);
    custom_keyword!(context);

    struct RuleDefinition {
        rule_provider: Type,
        axiom_provider: Option<Type>,
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
                let rule_provider: Type = content.parse()?;
                let axiom_provider = if content.peek(Token![=>]) {
                    content.parse::<Token![=>]>()?;
                    content.parse::<axiom>()?;
                    Some(content.parse()?)
                } else {
                    None
                };
                rules.push(RuleDefinition {
                    rule_provider,
                    axiom_provider,
                });
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

    let provider_types = def.rules.iter().map(|r| {
        let rule_ty = &r.rule_provider;
        match &r.axiom_provider {
            Some(axiom_ty) => quote! { (#rule_ty, Option<#axiom_ty>) },
            None => quote! { (#rule_ty, Option<()>) },
        }
    });

    let mut derive_body = quote! {};
    for (i, rule) in def.rules.iter().enumerate() {
        let index = syn::Index::from(i);

        let axiom_check_code = if rule.axiom_provider.is_some() {
            quote! {
                if let Some(axiom_provider) = axiom_provider_opt {
                    if let Some(axiom_result) = axiom_provider.axiom(ctx, judgment) {
                       derived_premises.push(axiom_result?);
                    }
                }
            }
        } else {
            quote! {}
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

        impl crate::system::traits::System<#judgment_type, #context_type> for #system_name {
            fn derive(&self, ctx: &mut #context_type, judgment: &#judgment_type) -> Result<crate::common::proof::Derivation<#judgment_type>, String> {
                use crate::system::traits::{Axiom, Rule};
                #derive_body
                Err(format!("No rule found to derive the judgment for: {:?}", judgment))
            }
        }
    };

    TokenStream::from(expanded)
}

use proc_macro::TokenStream;
use proc_macro2::Ident;

use quote::spanned::Spanned;
use syn::{punctuated::Punctuated, DeriveInput, Field, Token, Data, Fields, Type};

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let st = syn::parse_macro_input!(input as DeriveInput);
    match do_expand(&st) {
        Ok(token_stream) => token_stream.into(),
        Err(err) => err.to_compile_error().into(),
    }
}

type StructNamed = Punctuated<Field, Token![,]>;

fn get_fields_from_derive_input(st: &DeriveInput) -> syn::Result<&StructNamed> {
    match &st.data {
        Data::Struct(data_struct) => match &data_struct.fields {
            Fields::Named(fields_named) => Ok(&fields_named.named),
            _ => Err(syn::Error::new_spanned(st, "Can only act on named fields, not Unnamed fields and Unit.".to_string()))
        },
        _ => Err(syn::Error::new_spanned(st, "Must define on Struct, not on Enum or Union.".to_string()))
    }
}


fn generate_builder_struct_fields_def(fields: &StructNamed) -> syn::Result<proc_macro2::TokenStream> {
    let idents: Vec<&Option<Ident>> = fields.iter().map(|f| &f.ident).collect();
    let types: Vec<&Type> = fields.iter().map(|f| &f.ty).collect();
    let result = quote::quote!(
        #(#idents: core::option::Option<#types>),*
    );
    Ok(result)
}

fn generate_builder_struct_factory_init_clauses(fields: &StructNamed) -> syn::Result<proc_macro2::TokenStream> {
    let idents: Vec<&Option<Ident>> = fields.iter().map(|f| &f.ident).collect();
    let result = quote::quote!(
      #(#idents: core::option::Option::None),*
    );
    Ok(result)
}

fn do_expand(st: &DeriveInput) -> syn::Result<proc_macro2::TokenStream> {
    // 结构体名字字面量
    let struct_name_literal = st.ident.to_string();
    // 结构提名字标识符
    let struct_name_ident = st.ident.clone();

    // builder结构体名字字面量
    let builder_name_literal = format!("{}Builder", struct_name_literal);
    // builder结构体名字标识符
    let builder_name_ident = Ident::new(&builder_name_literal, st.__span());

    let fields = get_fields_from_derive_input(st)?;
    let builder_struct_field_def = generate_builder_struct_fields_def(fields)?;
    let builder_struct_factory_init_clauses = generate_builder_struct_factory_init_clauses(fields)?;
    // 生成builder结构体, 为原始结构体添加builder方法
    let result = quote::quote!(
        pub struct #builder_name_ident {
            #builder_struct_field_def
        }
        impl #struct_name_ident {
            pub fn builder()->#builder_name_ident {
                #builder_name_ident {
                    #builder_struct_factory_init_clauses
                }
            }
        }
    );
    Ok(result)
}

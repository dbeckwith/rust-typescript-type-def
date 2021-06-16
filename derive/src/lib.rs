#![warn(rust_2018_idioms, clippy::all)]
#![deny(clippy::correctness)]

use darling::{
    ast,
    util::SpannedValue,
    FromDeriveInput,
    FromField,
    FromMeta,
    FromVariant,
};
use indexmap::IndexSet;
use proc_macro2::{Span, TokenStream};
use proc_macro_error::{abort, abort_call_site, proc_macro_error};
use quote::{format_ident, quote, ToTokens};
use std::iter;
use syn::{
    ext::IdentExt,
    parse_quote,
    parse_str,
    punctuated::Punctuated,
    AngleBracketedGenericArguments,
    Attribute,
    Binding,
    DeriveInput,
    Expr,
    GenericArgument,
    Generics,
    Ident,
    Lifetime,
    Lit,
    LitStr,
    ParenthesizedGenericArguments,
    Path,
    PathArguments,
    PathSegment,
    ReturnType,
    Token,
    Type,
    TypePath,
    TypeTuple,
};

#[proc_macro_error]
#[proc_macro_derive(TypeDef, attributes(type_def, serde))]
pub fn derive_type_def(
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let input = match syn::parse2::<DeriveInput>(input.into()) {
        Ok(data) => data,
        Err(err) => return err.to_compile_error().into(),
    };
    let mut input = match TypeDefInput::from_derive_input(&input) {
        Ok(input) => input,
        Err(error) => return error.write_errors().into(),
    };

    data_to_static(&mut input.data);

    let ty_name = &input.ident;

    let deps_def = make_deps_def(&input);
    let info_def = make_info_def(&input);

    let ty_generics = {
        let generics = input.generics;
        if generics.type_params().next().is_some()
            || generics.const_params().next().is_some()
        {
            abort_call_site!("cannot derive TypeDef for generic types");
        }
        // replace any lifetimes with 'static
        let ty_generics = generics
            .lifetimes()
            .map(|_| Lifetime::new("'static", Span::call_site()))
            .collect::<Punctuated<_, Token![,]>>();
        if ty_generics.is_empty() {
            TokenStream::new()
        } else {
            let mut tokens = TokenStream::new();
            <Token![<]>::default().to_tokens(&mut tokens);
            ty_generics.to_tokens(&mut tokens);
            <Token![>]>::default().to_tokens(&mut tokens);
            tokens
        }
    };

    (quote! {
        impl ::typescript_type_def::TypeDef for #ty_name
        #ty_generics
        {
            type Deps = #deps_def;

            const INFO: ::typescript_type_def::type_expr::TypeInfo = #info_def;
        }
    })
    .into()
}

#[derive(FromDeriveInput)]
#[darling(attributes(type_def, serde), forward_attrs)]
struct TypeDefInput {
    attrs: Vec<Attribute>,
    ident: Ident,
    generics: Generics,
    data: ast::Data<TypeDefVariant, TypeDefField>,
    #[darling(default)]
    namespace: Namespace,
    #[darling(default)]
    tag: Option<SpannedValue<String>>,
    #[darling(default)]
    content: Option<SpannedValue<String>>,
    #[darling(default)]
    untagged: SpannedValue<bool>,
    #[darling(default)]
    rename_all: Option<SpannedValue<String>>,
    #[darling(default)]
    #[allow(dead_code)] // doesn't affect JSON
    transparent: SpannedValue<bool>,
}

#[derive(FromField)]
#[darling(attributes(serde), forward_attrs)]
struct TypeDefField {
    attrs: Vec<Attribute>,
    ident: Option<Ident>,
    ty: Type,
    #[darling(default)]
    flatten: SpannedValue<bool>,
    #[darling(default)]
    skip_serializing_if: Option<SpannedValue<String>>,
    #[darling(default)]
    default: SpannedValue<bool>,
}

#[derive(FromVariant)]
#[darling(attributes(serde))]
struct TypeDefVariant {
    ident: Ident,
    fields: ast::Fields<TypeDefField>,
    #[darling(default)]
    rename_all: Option<SpannedValue<String>>,
}

#[derive(Default)]
struct Namespace {
    parts: Vec<Ident>,
}

fn make_deps_def(TypeDefInput { data, .. }: &TypeDefInput) -> Type {
    let elems: IndexSet<_> = match data {
        ast::Data::Struct(ast::Fields { fields, .. }) => fields
            .iter()
            .map(|TypeDefField { ty, .. }| ty)
            .cloned()
            .collect(),
        ast::Data::Enum(variants) => variants
            .iter()
            .flat_map(
                |TypeDefVariant {
                     fields: ast::Fields { fields, .. },
                     ..
                 }| {
                    fields.iter().map(|TypeDefField { ty, .. }| ty).cloned()
                },
            )
            .collect(),
    };
    let mut elems = elems.into_iter().collect::<Punctuated<_, _>>();
    if !elems.empty_or_trailing() {
        elems.push_punct(Default::default());
    }
    Type::Tuple(TypeTuple {
        paren_token: Default::default(),
        elems,
    })
}

fn make_info_def(
    TypeDefInput {
        ident: ty_name,
        data,
        namespace,
        tag,
        content,
        untagged,
        rename_all,
        ..
    }: &TypeDefInput,
) -> Expr {
    let path_parts = namespace
        .parts
        .iter()
        .map(|part| -> Expr { type_ident(&part.to_string()) });
    let ty_name = type_ident(&ty_name.to_string());
    let name: Expr = parse_quote! {
        ::typescript_type_def::type_expr::TypeName {
            path: &[#(&#path_parts,)*],
            name: &#ty_name,
            generics: &[],
        }
    };
    let def: Expr = match data {
        ast::Data::Struct(ast::Fields { fields, style, .. }) => {
            if let Some(tag) = tag {
                abort!(tag.span(), "`tag` option is only valid for enums");
            }
            if let Some(content) = content {
                abort!(
                    content.span(),
                    "`content` option is only valid for enums"
                );
            }
            if **untagged {
                abort!(
                    untagged.span(),
                    "`untagged` option is only valid for enums"
                );
            }

            match style {
                ast::Style::Unit => todo!(),
                ast::Style::Tuple => fields_to_type_expr(fields, rename_all),
                ast::Style::Struct => {
                    if fields.is_empty() {
                        todo!();
                    }
                    let all_flatten = fields
                        .iter()
                        .all(|TypeDefField { flatten, .. }| **flatten);
                    type_expr_intersection(
                        fields
                            .iter()
                            .filter_map(
                                |TypeDefField { ty, flatten, .. }| {
                                    flatten.then(|| type_expr_ref(ty))
                                },
                            )
                            .chain((!all_flatten).then(|| {
                                fields_to_type_expr(fields, rename_all)
                            })),
                    )
                },
            }
        },
        ast::Data::Enum(variants) => {
            variants_to_type_expr(variants, tag, content, untagged, rename_all)
        },
    };
    parse_quote! {
        ::typescript_type_def::type_expr::TypeInfo::Custom(
            ::typescript_type_def::type_expr::CustomTypeInfo {
                name: &#name,
                def: &#def,
            },
        )
    }
}

fn fields_to_type_expr(
    fields: &[TypeDefField],
    rename: &Option<SpannedValue<String>>,
) -> Expr {
    let named = fields.first().unwrap().ident.is_some();
    let fields = fields.iter().filter_map(
        |TypeDefField {
             attrs,
             ident: field_name,
             ty,
             flatten,
             skip_serializing_if,
             default,
             ..
         }|
         -> Option<Expr> {
            if **flatten {
                if !named {
                    abort!(flatten.span(), "tuple fields cannot be flattened");
                }
                return None;
            }
            let mut ty = ty;
            if let Some(field_name) = field_name {
                // TODO: docs
                let name = type_string(
                    &serde_rename_ident(field_name, rename).value(),
                );
                let optional =
                    if let Some(skip_serializing_if) = skip_serializing_if {
                        if let Some(inner_ty) = is_option(ty) {
                            if parse_str::<Path>(skip_serializing_if).unwrap()
                                == parse_str::<Path>("Option::is_none").unwrap()
                            {
                                ty = inner_ty;
                            }
                        }
                        true
                    } else {
                        **default
                    };
                let r#type = type_expr_ref(ty);
                Some(type_object_field(&name, optional, &r#type))
            } else {
                Some(type_expr_ref(ty))
            }
        },
    );
    if named {
        type_expr_object(fields)
    } else {
        type_expr_tuple(fields)
    }
}

fn variants_to_type_expr(
    variants: &[TypeDefVariant],
    tag: &Option<SpannedValue<String>>,
    content: &Option<SpannedValue<String>>,
    untagged: &SpannedValue<bool>,
    variant_rename: &Option<SpannedValue<String>>,
) -> Expr {
    type_expr_union(variants.iter().map(
        |TypeDefVariant {
             ident: variant_name,
             fields: ast::Fields { style, fields, .. },
             rename_all: field_rename,
             ..
         }|
         -> Expr {
            let variant_name = serde_rename_ident(variant_name, variant_rename);
            match (tag, content, **untagged) {
                (None, None, false) => match style {
                    ast::Style::Unit => type_expr_string(&variant_name.value()),
                    ast::Style::Tuple | ast::Style::Struct => {
                        type_expr_object(iter::once(type_object_field(
                            &type_string(&variant_name.value()),
                            false,
                            &fields_to_type_expr(fields, field_rename),
                        )))
                    },
                },
                (None, None, true) => match style {
                    ast::Style::Unit => type_expr_ident("null"),
                    ast::Style::Tuple | ast::Style::Struct => {
                        fields_to_type_expr(fields, field_rename)
                    },
                },
                (Some(tag), None, false) => match style {
                    ast::Style::Unit => {
                        type_expr_object(iter::once(type_object_field(
                            &type_string(&**tag),
                            false,
                            &type_expr_string(&variant_name.value()),
                        )))
                    },
                    ast::Style::Tuple | ast::Style::Struct => {
                        if matches!(style, ast::Style::Tuple)
                            && fields.len() != 1
                        {
                            abort!(
                                tag.span(),
                                "cannot tag enums with tuple variants"
                            );
                        }
                        type_expr_intersection(std::array::IntoIter::new([
                            type_expr_object(iter::once(type_object_field(
                                &type_string(&**tag),
                                false,
                                &type_expr_string(&variant_name.value()),
                            ))),
                            fields_to_type_expr(fields, field_rename),
                        ]))
                    },
                },
                (Some(tag), Some(content), false) => match style {
                    ast::Style::Unit => {
                        type_expr_object(iter::once(type_object_field(
                            &type_string(&**tag),
                            false,
                            &type_expr_string(&variant_name.value()),
                        )))
                    },
                    ast::Style::Tuple | ast::Style::Struct => {
                        type_expr_object(std::array::IntoIter::new([
                            type_object_field(
                                &type_string(&**tag),
                                false,
                                &type_expr_string(&variant_name.value()),
                            ),
                            type_object_field(
                                &type_string(&**content),
                                false,
                                &fields_to_type_expr(fields, field_rename),
                            ),
                        ]))
                    },
                },
                (Some(tag), _, true) => {
                    abort!(
                        tag.span(),
                        "cannot give both `tag` and `untagged` options"
                    );
                },
                (None, Some(content), _) => {
                    abort!(
                        content.span(),
                        "`content` option requires `tag` option"
                    );
                },
            }
        },
    ))
}

fn type_ident(ident: &str) -> Expr {
    parse_quote! {
        ::typescript_type_def::type_expr::Ident(
            #ident,
        )
    }
}

fn type_string(s: &str) -> Expr {
    parse_quote! {
        ::typescript_type_def::type_expr::TypeString(
            #s,
        )
    }
}

fn type_expr_ident(ident: &str) -> Expr {
    parse_quote! {
        ::typescript_type_def::type_expr::TypeExpr::ident(
            &::typescript_type_def::type_expr::Ident(
                #ident,
            ),
        )
    }
}

fn type_expr_string(s: &str) -> Expr {
    parse_quote! {
        ::typescript_type_def::type_expr::TypeExpr::String(
            ::typescript_type_def::type_expr::TypeString(
                #s,
            ),
        )
    }
}

fn type_expr_ref(ty: &Type) -> Expr {
    parse_quote! {
        ::typescript_type_def::type_expr::TypeExpr::Ref(
            <#ty as ::typescript_type_def::TypeDef>::INFO,
        )
    }
}

fn type_expr_tuple(exprs: impl Iterator<Item = Expr>) -> Expr {
    let exprs = exprs.collect::<Vec<_>>();
    if exprs.len() == 1 {
        exprs.into_iter().next().unwrap()
    } else {
        parse_quote! {
            ::typescript_type_def::type_expr::TypeExpr::Tuple(
                ::typescript_type_def::type_expr::Tuple(&[
                    #(&#exprs,)*
                ]),
            )
        }
    }
}

fn type_object_field(name: &Expr, optional: bool, r#type: &Expr) -> Expr {
    parse_quote! {
        ::typescript_type_def::type_expr::ObjectField {
            name: &#name,
            optional: #optional,
            r#type: &#r#type,
        }
    }
}

fn type_expr_object(exprs: impl Iterator<Item = Expr>) -> Expr {
    parse_quote! {
        ::typescript_type_def::type_expr::TypeExpr::Object(
            ::typescript_type_def::type_expr::Object(&[
                #(&#exprs,)*
            ]),
        )
    }
}

fn type_expr_intersection(exprs: impl Iterator<Item = Expr>) -> Expr {
    let exprs = exprs.collect::<Vec<_>>();
    if exprs.len() == 1 {
        exprs.into_iter().next().unwrap()
    } else {
        parse_quote! {
            ::typescript_type_def::type_expr::TypeExpr::Intersection(
                ::typescript_type_def::type_expr::Intersection(&[
                    #(&#exprs,)*
                ]),
            )
        }
    }
}

fn type_expr_union(exprs: impl Iterator<Item = Expr>) -> Expr {
    let exprs = exprs.collect::<Vec<_>>();
    if exprs.len() == 1 {
        exprs.into_iter().next().unwrap()
    } else {
        parse_quote! {
            ::typescript_type_def::type_expr::TypeExpr::Union(
                ::typescript_type_def::type_expr::Union(&[
                    #(&#exprs,)*
                ]),
            )
        }
    }
}

fn serde_rename_ident(
    ident: &Ident,
    rename: &Option<SpannedValue<String>>,
) -> LitStr {
    LitStr::new(
        &{
            let ident = ident.unraw().to_string();
            if let Some(rename) = rename {
                use heck::{
                    CamelCase,
                    KebabCase,
                    MixedCase,
                    ShoutySnakeCase,
                    SnakeCase,
                };
                match rename.as_str() {
                    "lowercase" => ident.to_lowercase(),
                    "UPPERCASE" => ident.to_uppercase(),
                    "camelCase" => ident.to_mixed_case(),
                    "PascalCase" => ident.to_camel_case(),
                    "snake_case" => ident.to_snake_case(),
                    "SCREAMING_SNAKE_CASE" => ident.to_shouty_snake_case(),
                    "kebab-case" => ident.to_kebab_case(),
                    "SCREAMING-KEBAB-CASE" => {
                        ident.to_kebab_case().to_uppercase()
                    },
                    _ => abort!(rename.span(), "unknown case conversion"),
                }
            } else {
                ident
            }
        },
        ident.span(),
    )
}

impl FromMeta for Namespace {
    fn from_value(value: &Lit) -> Result<Self, darling::Error> {
        match value {
            Lit::Str(lit_str) => Ok(Self {
                parts: lit_str
                    .value()
                    .split('.')
                    .map(|part| format_ident!("{}", part))
                    .collect(),
            }),
            _ => Err(darling::Error::custom("expected string literal")),
        }
    }
}

fn data_to_static(data: &mut ast::Data<TypeDefVariant, TypeDefField>) {
    match data {
        ast::Data::Struct(ast::Fields { fields, .. }) => {
            for TypeDefField { ty, .. } in fields {
                ty_to_static(ty);
            }
        },
        ast::Data::Enum(variants) => {
            for TypeDefVariant {
                fields: ast::Fields { fields, .. },
                ..
            } in variants
            {
                for TypeDefField { ty, .. } in fields {
                    ty_to_static(ty);
                }
            }
        },
    }
}

fn ty_to_static(ty: &mut Type) {
    if let Type::Path(TypePath {
        path: Path { segments, .. },
        ..
    }) = ty
    {
        for PathSegment { arguments, .. } in segments {
            match arguments {
                PathArguments::None => {},
                PathArguments::AngleBracketed(
                    AngleBracketedGenericArguments { args, .. },
                ) => {
                    for arg in args {
                        match arg {
                            GenericArgument::Lifetime(lifetime) => {
                                *lifetime =
                                    Lifetime::new("'static", Span::call_site());
                            },
                            GenericArgument::Type(ty) => ty_to_static(ty),
                            GenericArgument::Binding(Binding {
                                ty, ..
                            }) => ty_to_static(ty),
                            GenericArgument::Constraint(_)
                            | GenericArgument::Const(_) => {},
                        }
                    }
                },
                PathArguments::Parenthesized(
                    ParenthesizedGenericArguments { inputs, output, .. },
                ) => {
                    for input in inputs {
                        ty_to_static(input);
                    }
                    match output {
                        ReturnType::Default => {},
                        ReturnType::Type(_, ty) => ty_to_static(ty),
                    }
                },
            }
        }
    }
}

fn is_option(ty: &Type) -> Option<&Type> {
    if let Type::Path(TypePath {
        qself: None,
        path:
            Path {
                leading_colon: None,
                segments,
            },
    }) = ty
    {
        if segments.len() == 1 {
            let PathSegment { ident, arguments } = &segments[0];
            if ident == "Option" {
                if let PathArguments::AngleBracketed(
                    AngleBracketedGenericArguments { args, .. },
                ) = arguments
                {
                    if args.len() == 1 {
                        if let GenericArgument::Type(ty) = &args[0] {
                            return Some(ty);
                        }
                    }
                }
            }
        }
    }
    None
}

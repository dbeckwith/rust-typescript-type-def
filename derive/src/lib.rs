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
use proc_macro2::{Span, TokenStream};
use proc_macro_error::{abort, abort_call_site, proc_macro_error};
use quote::{format_ident, quote, ToTokens};
use syn::{
    ext::IdentExt,
    parse_quote,
    parse_str,
    punctuated::Punctuated,
    AngleBracketedGenericArguments,
    Attribute,
    Binding,
    Block,
    DeriveInput,
    GenericArgument,
    Generics,
    Ident,
    Lifetime,
    Lit,
    LitStr,
    Meta,
    MetaNameValue,
    ParenthesizedGenericArguments,
    Path,
    PathArguments,
    PathSegment,
    ReturnType,
    Stmt,
    Token,
    Type,
    TypePath,
};

#[proc_macro_error]
#[proc_macro_derive(TypescriptTypeDef, attributes(typescript_type_def, serde))]
pub fn derive_typescript_type_def(
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let input = match syn::parse2::<DeriveInput>(input.into()) {
        Ok(data) => data,
        Err(err) => return err.to_compile_error().into(),
    };
    let mut input = match TypescriptTypeDefInput::from_derive_input(&input) {
        Ok(input) => input,
        Err(error) => return error.write_errors().into(),
    };

    data_to_static(&mut input.data);

    let ty_name = &input.ident;

    let emit_name_body = make_emit_name_body(&input);
    let emit_deps_body = make_emit_deps_body(&input);
    let emit_def_body = make_emit_def_body(&input);

    let generics = input.generics;
    if generics.type_params().next().is_some()
        || generics.const_params().next().is_some()
    {
        abort_call_site!("cannot derive TypescriptTypeDef for generic types");
    }
    let ty_generics = generics
        .lifetimes()
        .map(|_| Lifetime::new("'static", Span::call_site()))
        .collect::<Punctuated<_, Token![,]>>();
    let ty_generics = if ty_generics.is_empty() {
        TokenStream::new()
    } else {
        let mut tokens = TokenStream::new();
        <Token![<]>::default().to_tokens(&mut tokens);
        ty_generics.to_tokens(&mut tokens);
        <Token![>]>::default().to_tokens(&mut tokens);
        tokens
    };

    (quote! {
        impl ::typescript_type_def::TypescriptTypeDef
        for #ty_name #ty_generics
        {
            fn emit_name(
                ctx: &mut ::typescript_type_def::Context<'_>,
            ) -> ::std::io::Result<()> #emit_name_body

            fn emit_deps(
                ctx: &mut ::typescript_type_def::EmitDepsContext<'_>,
            ) -> ::std::io::Result<()> #emit_deps_body

            fn emit_def(
                ctx: &mut ::typescript_type_def::Context<'_>,
            ) -> ::std::io::Result<()> #emit_def_body
        }
    })
    .into()
}

#[derive(FromDeriveInput)]
#[darling(attributes(typescript_type_def, serde), forward_attrs)]
struct TypescriptTypeDefInput {
    attrs: Vec<Attribute>,
    ident: Ident,
    generics: Generics,
    data: ast::Data<TypescriptTypeDefVariant, TypescriptTypeDefField>,
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
struct TypescriptTypeDefField {
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
struct TypescriptTypeDefVariant {
    ident: Ident,
    fields: ast::Fields<TypescriptTypeDefField>,
    #[darling(default)]
    rename_all: Option<SpannedValue<String>>,
}

fn make_emit_name_body(
    TypescriptTypeDefInput {
        ident: ty_name,
        namespace,
        ..
    }: &TypescriptTypeDefInput,
) -> Block {
    let mut stmts = Vec::new();
    for part in &namespace.parts {
        stmts.push(parse_quote! {
            ::std::write!(ctx.out, "{}.", stringify!(#part))?;
        });
    }
    stmts.push(parse_quote! {
        ::std::write!(ctx.out, "{}", stringify!(#ty_name))?;
    });
    stmts.push(Stmt::Expr(parse_quote!(Ok(()))));
    Block {
        brace_token: Default::default(),
        stmts,
    }
}

fn make_emit_deps_body(
    TypescriptTypeDefInput { data, .. }: &TypescriptTypeDefInput,
) -> Block {
    match data {
        ast::Data::Struct(fields) => make_struct_emit_deps_body(fields),
        ast::Data::Enum(variants) => make_enum_emit_deps_body(variants),
    }
}

fn make_struct_emit_deps_body(
    ast::Fields { fields, .. }: &ast::Fields<TypescriptTypeDefField>,
) -> Block {
    let mut stmts = Vec::new();
    for TypescriptTypeDefField { ty, .. } in fields.iter() {
        stmts.push(parse_quote! {
            ctx.emit_dep::<#ty>()?;
        });
    }
    stmts.push(Stmt::Expr(parse_quote!(Ok(()))));
    Block {
        brace_token: Default::default(),
        stmts,
    }
}

fn make_enum_emit_deps_body(variants: &[TypescriptTypeDefVariant]) -> Block {
    let mut stmts = Vec::new();
    for TypescriptTypeDefVariant { fields, .. } in variants {
        for TypescriptTypeDefField { ty, .. } in fields.iter() {
            stmts.push(parse_quote! {
                ctx.emit_dep::<#ty>()?;
            });
        }
    }
    stmts.push(Stmt::Expr(parse_quote!(Ok(()))));
    Block {
        brace_token: Default::default(),
        stmts,
    }
}

fn make_emit_def_body(
    TypescriptTypeDefInput {
        attrs,
        ident: ty_name,
        data,
        namespace,
        tag,
        content,
        untagged,
        rename_all,
        ..
    }: &TypescriptTypeDefInput,
) -> Block {
    let mut stmts = Vec::new();
    if !namespace.parts.is_empty() {
        stmts.push(parse_quote! {
            ::std::write!(ctx.out, "export namespace ")?;
        });
        let mut first = true;
        for part in &namespace.parts {
            if !first {
                stmts.push(parse_quote! {
                    ::std::write!(ctx.out, ".")?;
                });
            }
            stmts.push(parse_quote! {
                ::std::write!(ctx.out, "{}", stringify!(#part))?;
            });
            first = false;
        }
        stmts.push(parse_quote! {
            ::std::write!(ctx.out, "{{")?;
        });
    }
    handle_doc(&mut stmts, attrs);
    stmts.push(parse_quote! {
        ::std::write!(ctx.out, "export type {}=", stringify!(#ty_name))?;
    });
    match data {
        ast::Data::Struct(ast::Fields { style, fields, .. }) => {
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
                ast::Style::Struct => {
                    let all_flatten = !fields.is_empty()
                        && fields.iter().all(
                            |TypescriptTypeDefField { flatten, .. }| **flatten,
                        );
                    for (idx, TypescriptTypeDefField { ty, flatten, .. }) in
                        fields.iter().enumerate()
                    {
                        if **flatten {
                            stmts.push(parse_quote! {
                                ctx.emit_name::<#ty>()?;
                            });
                            if !all_flatten || idx != fields.len() - 1 {
                                stmts.push(parse_quote! {
                                    ::std::write!(ctx.out, "&")?;
                                });
                            }
                        }
                    }
                    if !all_flatten {
                        stmts.push(parse_quote! {
                            ::std::write!(ctx.out, "{{")?;
                        });
                        handle_fields(&mut stmts, fields, rename_all);
                        stmts.push(parse_quote! {
                            ::std::write!(ctx.out, "}}")?;
                        });
                    }
                },
                ast::Style::Tuple => {
                    if fields.len() != 1 {
                        stmts.push(parse_quote! {
                            ::std::write!(ctx.out, "[")?;
                        });
                    }
                    handle_fields(&mut stmts, fields, rename_all);
                    if fields.len() != 1 {
                        stmts.push(parse_quote! {
                            ::std::write!(ctx.out, "]")?;
                        });
                    }
                },
                ast::Style::Unit => {
                    let ty_name = ty_name.to_string();
                    stmts.push(parse_quote! {
                        ::std::write!(ctx.out, "{}", stringify!(#ty_name))?;
                    });
                },
            }
        },
        ast::Data::Enum(variants) => {
            if let Some(content) = content {
                if tag.is_none() {
                    abort!(
                        content.span(),
                        "`content` option requires `tag` option"
                    );
                }
            }
            if tag.is_some() && **untagged {
                abort!(
                    untagged.span(),
                    "cannot give both `tag` and `untagged` options"
                );
            }

            let variant_rename = rename_all;
            for TypescriptTypeDefVariant {
                ident: variant_name,
                fields: ast::Fields { style, fields, .. },
                rename_all: field_rename,
                ..
            } in variants
            {
                stmts.push(parse_quote! {
                    ::std::write!(ctx.out, "|")?;
                });
                let variant_name = handle_rename(variant_name, variant_rename);
                match style {
                    ast::Style::Struct => {
                        if !**untagged && tag.is_none() {
                            stmts.push(parse_quote! {
                                ::std::write!(
                                    ctx.out,
                                    "{{{}:",
                                    stringify!(#variant_name)
                                )?;
                            });
                        }
                        stmts.push(parse_quote! {
                            ::std::write!(ctx.out, "{{")?;
                        });
                        if let Some(tag) = tag {
                            let tag = &**tag;
                            stmts.push(parse_quote! {
                                ::std::write!(
                                    ctx.out,
                                    "{}:{};",
                                    stringify!(#tag),
                                    stringify!(#variant_name)
                                )?;
                            });
                        }
                        if let Some(content) = content {
                            let content = &**content;
                            stmts.push(parse_quote! {
                                ::std::write!(
                                    ctx.out,
                                    "{}:{{",
                                    stringify!(#content)
                                )?;
                            });
                        }
                        handle_fields(&mut stmts, fields, field_rename);
                        if content.is_some() {
                            stmts.push(parse_quote! {
                                ::std::write!(ctx.out, "}};")?;
                            });
                        }
                        stmts.push(parse_quote! {
                            ::std::write!(ctx.out, "}}")?;
                        });
                        if !**untagged && tag.is_none() {
                            stmts.push(parse_quote! {
                                ::std::write!(ctx.out, "}}")?;
                            });
                        }
                    },
                    ast::Style::Tuple => {
                        if let (Some(tag), None) = (tag, content) {
                            if fields.len() != 1 {
                                abort!(
                                    tag.span(),
                                    "cannot tag enums with tuple variants"
                                );
                            }
                            let ty = &fields.first().unwrap().ty;
                            let tag = &**tag;
                            stmts.push(parse_quote! {
                                ::std::write!(ctx.out, "(")?;
                            });
                            stmts.push(parse_quote! {
                                ctx.emit_name::<#ty>()?;
                            });
                            stmts.push(parse_quote! {
                                ::std::write!(
                                    ctx.out,
                                    "&{{{}:{};}})",
                                    stringify!(#tag),
                                    stringify!(#variant_name)
                                )?;
                            });
                        } else {
                            if let (Some(tag), Some(content)) = (tag, content) {
                                if fields.len() != 1 {
                                    abort!(
                                        tag.span(),
                                        "cannot tag enums with tuple variants"
                                    );
                                }
                                let tag = &**tag;
                                let content = &**content;
                                stmts.push(parse_quote! {
                                    ::std::write!(
                                        ctx.out,
                                        "{{{}:{};{}:",
                                        stringify!(#tag),
                                        stringify!(#variant_name),
                                        stringify!(#content)
                                    )?;
                                });
                            } else if !**untagged {
                                stmts.push(parse_quote! {
                                    ::std::write!(
                                        ctx.out,
                                        "{{{}:",
                                        stringify!(#variant_name)
                                    )?;
                                });
                            }
                            if fields.len() != 1 {
                                stmts.push(parse_quote! {
                                    ::std::write!(ctx.out, "[")?;
                                });
                            }
                            handle_fields(&mut stmts, fields, field_rename);
                            if fields.len() != 1 {
                                stmts.push(parse_quote! {
                                    ::std::write!(ctx.out, "]")?;
                                });
                            }
                            if (tag.is_some() && content.is_some())
                                || !**untagged
                            {
                                stmts.push(parse_quote! {
                                    ::std::write!(ctx.out, ";}}")?;
                                });
                            }
                        }
                    },
                    ast::Style::Unit => {
                        if let Some(tag) = tag {
                            let tag = &**tag;
                            stmts.push(parse_quote! {
                                ::std::write!(
                                    ctx.out,
                                    "{{{}:{};}}",
                                    stringify!(#tag),
                                    stringify!(#variant_name)
                                )?;
                            });
                        } else if **untagged {
                            stmts.push(parse_quote! {
                                ::std::write!(ctx.out, "null")?;
                            });
                        } else {
                            stmts.push(parse_quote! {
                                ::std::write!(
                                    ctx.out,
                                    "{}",
                                    stringify!(#variant_name)
                                )?;
                            });
                        }
                    },
                }
            }
        },
    }
    stmts.push(parse_quote! {
        ::std::write!(ctx.out, ";")?;
    });
    if !namespace.parts.is_empty() {
        stmts.push(parse_quote! {
            ::std::write!(ctx.out, "}}")?;
        });
    }
    stmts.push(Stmt::Expr(parse_quote!(Ok(()))));
    Block {
        brace_token: Default::default(),
        stmts,
    }
}

fn handle_fields(
    stmts: &mut Vec<Stmt>,
    fields: &[TypescriptTypeDefField],
    rename: &Option<SpannedValue<String>>,
) {
    let mut first = true;
    for TypescriptTypeDefField {
        attrs,
        ident,
        ty,
        flatten,
        skip_serializing_if,
        default,
        ..
    } in fields
    {
        if **flatten {
            if ident.is_none() {
                abort!(flatten.span(), "tuple fields cannot be flattened");
            }
            continue;
        }
        if ident.is_none() && !first {
            stmts.push(parse_quote! {
                ::std::write!(ctx.out, ",")?;
            });
        }
        let mut ty = ty;
        if let Some(ident) = ident {
            handle_doc(stmts, attrs);
            let ident = handle_rename(ident, rename);
            stmts.push(parse_quote! {
                ::std::write!(ctx.out, "{}", stringify!(#ident))?;
            });
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
            if optional {
                stmts.push(parse_quote! {
                    ::std::write!(ctx.out, "?")?;
                });
            }
            stmts.push(parse_quote! {
                ::std::write!(ctx.out, ":")?;
            });
        }
        stmts.push(parse_quote! {
            ctx.emit_name::<#ty>()?;
        });
        if ident.is_some() {
            stmts.push(parse_quote! {
                ::std::write!(ctx.out, ";")?;
            });
        }
        first = false;
    }
}

fn handle_doc(stmts: &mut Vec<Stmt>, attrs: &[Attribute]) {
    let mut docs = attrs
        .iter()
        .filter_map(|attr| {
            if let Ok(Meta::NameValue(MetaNameValue {
                path,
                lit: Lit::Str(lit_str),
                ..
            })) = attr.parse_meta()
            {
                path.is_ident("doc").then(|| lit_str.value())
            } else {
                None
            }
        })
        .collect::<Vec<_>>();
    let min_indent = if let Some(min_indent) = docs
        .iter()
        .filter_map(|doc| {
            if doc.is_empty() {
                None
            } else {
                Some(
                    doc.find(|c: char| !c.is_whitespace())
                        .unwrap_or_else(|| doc.len()),
                )
            }
        })
        .min()
    {
        min_indent
    } else {
        return;
    };
    if min_indent > 0 {
        for doc in &mut docs {
            if !doc.is_empty() {
                *doc = doc.split_off(min_indent);
            }
        }
    }
    stmts.push(parse_quote! {
        ::std::writeln!(ctx.out, "\n/**")?;
    });
    for doc in docs {
        stmts.push(parse_quote! {
            ::std::writeln!(ctx.out, " * {}", #doc)?;
        });
    }
    stmts.push(parse_quote! {
        ::std::writeln!(ctx.out, " */")?;
    });
}

fn handle_rename(
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

#[derive(Default)]
struct Namespace {
    parts: Vec<Ident>,
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

fn data_to_static(
    data: &mut ast::Data<TypescriptTypeDefVariant, TypescriptTypeDefField>,
) {
    match data {
        ast::Data::Struct(ast::Fields { fields, .. }) => {
            for TypescriptTypeDefField { ty, .. } in fields {
                ty_to_static(ty);
            }
        },
        ast::Data::Enum(variants) => {
            for TypescriptTypeDefVariant {
                fields: ast::Fields { fields, .. },
                ..
            } in variants
            {
                for TypescriptTypeDefField { ty, .. } in fields {
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

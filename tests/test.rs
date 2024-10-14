use serde::Serialize;
use std::collections::{HashMap, HashSet};
use typescript_type_def::{
    type_expr::{DefinedTypeInfo, Ident, TypeDefinition, TypeExpr, TypeInfo},
    write_definition_file, DefinitionFileOptions, TypeDef,
};

static TEST_OPTIONS: DefinitionFileOptions<'_> = DefinitionFileOptions {
    header: None,
    root_namespace: Some("types"),
};

fn test_emit<T>() -> String
where
    T: TypeDef,
{
    let mut buf = Vec::new();
    write_definition_file::<_, T>(&mut buf, TEST_OPTIONS).unwrap();
    String::from_utf8(buf).unwrap()
}

macro_rules! assert_eq_str {
    ($actual:expr, $expected:expr) => {{
        let actual = $actual;
        let expected = $expected;
        ::difference::assert_diff!(expected, actual.as_str(), "\n", 0);
    }};
}

#[test]
fn emit() {
    type Inner = Vec<HashMap<Option<usize>, HashSet<String>>>;

    #[derive(Serialize)]
    struct Test(Inner);

    impl TypeDef for Test {
        const INFO: TypeInfo = TypeInfo::Defined(DefinedTypeInfo {
            def: TypeDefinition {
                docs: None,
                path: &[],
                name: Ident("Test"),
                generic_vars: &[],
                def: TypeExpr::Ref(&Inner::INFO),
            },
            generic_args: &[],
        });
    }

    let emitted;
    let stats;
    {
        let mut buf = Vec::new();
        stats =
            write_definition_file::<_, Test>(&mut buf, TEST_OPTIONS).unwrap();
        emitted = String::from_utf8(buf).unwrap();
    }
    assert_eq_str!(
        emitted,
        r#"export default types;
export namespace types {
    export type Usize = number;
    export type Test = (Record<(types.Usize | null), (string)[]>)[];
}
"#
    );
    assert_eq!(stats.type_definitions, 2);
}

mod derive {
    #![allow(dead_code)]

    use super::*;
    use std::{fmt, marker::PhantomData};

    #[derive(Serialize, TypeDef)]
    #[serde(rename_all = "SCREAMING_SNAKE_CASE")]
    struct Parent {
        foo_bar: usize,
    }

    #[derive(Serialize, TypeDef)]
    struct Test {
        #[serde(flatten)]
        parent: Parent,
        a: String,
        b: Option<usize>,
        #[serde(skip_serializing_if = "Option::is_none")]
        c: Option<Vec<bool>>,
        #[serde(skip_serializing_if = "Option::is_none")]
        d: Option<u8>,
        #[serde(skip)]
        e: Result<(), std::fs::File>,
        #[serde(rename = "FFF")]
        f: String,
        g: Result<String, usize>,
        h: Result<(), usize>,
        i: &'static [&'static str],
    }

    #[derive(Serialize, TypeDef)]
    struct Test2(Test, usize, String);

    #[derive(Serialize, TypeDef)]
    struct Test3(Test2);

    #[derive(Serialize, TypeDef)]
    #[serde(untagged)]
    enum Test4 {
        A(Test3),
        B(String, usize),
        C {
            a: String,
            b: usize,
        },
        #[serde(skip)]
        D(std::fs::File),
    }

    #[derive(Serialize, TypeDef)]
    #[serde(rename_all = "kebab-case")]
    enum Test5 {
        A,
        B,
        CoolBeans,
        #[serde(rename = "DDD")]
        D,
    }

    #[derive(Serialize, TypeDef)]
    enum Test6 {
        A { a: usize },
        B(usize, String),
        C(String),
        D,
    }

    #[derive(Serialize, TypeDef)]
    struct Test7;

    #[derive(Serialize, TypeDef)]
    #[serde(rename = "TEST_8")]
    struct Test8 {}

    #[derive(Serialize, TypeDef)]
    enum Test9 {}

    #[allow(clippy::large_enum_variant)]
    #[derive(Serialize, TypeDef)]
    #[serde(tag = "type", content = "value")]
    enum Test10 {
        A {
            a: String,
            b: usize,
            #[serde(flatten)]
            c: Parent,
        },
        #[serde(rename_all = "UPPERCASE")]
        B {
            a: Test4,
            b: Test5,
            c: Test6,
            d: Test7,
            e: Test8,
            f: Option<Test9>,
            g: (),
            h: std::net::IpAddr,
            i: Test11,
        },
        C(Parent),
        D,
        E {},
        F(),
    }

    #[derive(Serialize, TypeDef)]
    #[serde(rename_all_fields = "kebab-case")]
    enum Test11 {
        A {
            a_a: usize,
        },
        B {
            b_a: usize,
        },
        #[serde(rename_all = "UPPERCASE")]
        C {
            c_a: usize,
        },
    }

    #[test]
    fn emit() {
        assert_eq_str!(
            test_emit::<Test10>(),
            r#"export default types;
export namespace types {
    export type Usize = number;
    export type Parent = {
        "FOO_BAR": types.Usize;
    };
    export type U8 = number;
    export type Test = (types.Parent & {
        "a": string;
        "b": (types.Usize | null);
        "c"?: (boolean)[];
        "d"?: types.U8;
        "FFF": string;
        "g": ({
            "Ok": string;
        } | {
            "Err": types.Usize;
        });
        "h": ({
            "Ok": null;
        } | {
            "Err": types.Usize;
        });
        "i": (string)[];
    });
    export type Test2 = [types.Test, types.Usize, string];
    export type Test3 = types.Test2;
    export type Test4 = (types.Test3 | [string, types.Usize] | {
        "a": string;
        "b": types.Usize;
    });
    export type Test5 = ("a" | "b" | "cool-beans" | "DDD");
    export type Test6 = ({
        "A": {
            "a": types.Usize;
        };
    } | {
        "B": [types.Usize, string];
    } | {
        "C": string;
    } | "D");
    export type Test7 = null;
    export type TEST_8 = {
    };
    export type Test9 = never;
    export type Test11 = ({
        "A": {
            "a-a": types.Usize;
        };
    } | {
        "B": {
            "b-a": types.Usize;
        };
    } | {
        "C": {
            "C_A": types.Usize;
        };
    });
    export type Test10 = ({
        "type": "A";
        "value": (types.Parent & {
            "a": string;
            "b": types.Usize;
        });
    } | {
        "type": "B";
        "value": {
            "A": types.Test4;
            "B": types.Test5;
            "C": types.Test6;
            "D": types.Test7;
            "E": types.TEST_8;
            "F": (types.Test9 | null);
            "G": null;
            "H": string;
            "I": types.Test11;
        };
    } | {
        "type": "C";
        "value": types.Parent;
    } | {
        "type": "D";
    } | {
        "type": "E";
        "value": {
        };
    } | {
        "type": "F";
        "value": [];
    });
}
"#
        );
    }

    #[test]
    fn json() {
        use std::str::FromStr;
        assert_eq_str!(
            serde_json::to_string(&Test10::B {
                a: Test4::A(Test3(Test2(
                    Test {
                        parent: Parent { foo_bar: 123 },
                        a: "foo".to_owned(),
                        b: None,
                        c: Some(vec![true, false]),
                        d: None,
                        e: Ok(()),
                        f: "f".to_owned(),
                        g: Ok("test".to_owned()),
                        h: Err(1234),
                        i: &["test"],
                    },
                    4,
                    "bar".to_owned(),
                ))),
                b: Test5::CoolBeans,
                c: Test6::B(42, "baz".to_owned()),
                d: Test7,
                e: Test8 {},
                f: None,
                g: (),
                h: std::net::IpAddr::from_str("::1").unwrap(),
                i: Test11::A { a_a: 0 },
            })
            .unwrap(),
            r#"{"type":"B","value":{"A":[{"FOO_BAR":123,"a":"foo","b":null,"c":[true,false],"FFF":"f","g":{"Ok":"test"},"h":{"Err":1234},"i":["test"]},4,"bar"],"B":"cool-beans","C":{"B":[42,"baz"]},"D":null,"E":{},"F":null,"G":null,"H":"::1","I":{"A":{"a-a":0}}}}"#
        );
    }

    mod enum_tags {
        use super::*;

        #[derive(Clone, Copy, Serialize, TypeDef)]
        struct Inner {
            x: bool,
        }

        const INNER: Inner = Inner { x: true };

        #[test]
        fn tag() {
            #[derive(Serialize, TypeDef)]
            #[serde(tag = "type")]
            enum Test {
                A { a: Inner },
                B(Inner),
                // C(Inner, Inner), // not allowed
                D,
            }

            assert_eq_str!(
                serde_json::to_string(&Test::A { a: INNER }).unwrap(),
                r#"{"type":"A","a":{"x":true}}"#
            );
            assert_eq_str!(
                serde_json::to_string(&Test::B(INNER)).unwrap(),
                r#"{"type":"B","x":true}"#
            );
            assert_eq_str!(
                serde_json::to_string(&Test::D).unwrap(),
                r#"{"type":"D"}"#
            );
            assert_eq_str!(
                test_emit::<Test>(),
                r#"export default types;
export namespace types {
    export type Inner = {
        "x": boolean;
    };
    export type Test = (({
        "type": "A";
    } & {
        "a": types.Inner;
    }) | ({
        "type": "B";
    } & types.Inner) | {
        "type": "D";
    });
}
"#
            );
        }

        #[test]
        fn tag_content() {
            #[derive(Serialize, TypeDef)]
            #[serde(tag = "type", content = "value")]
            enum Test {
                A { a: Inner },
                B(Inner),
                C(Inner, Inner),
                D,
            }

            assert_eq_str!(
                serde_json::to_string(&Test::A { a: INNER }).unwrap(),
                r#"{"type":"A","value":{"a":{"x":true}}}"#
            );
            assert_eq_str!(
                serde_json::to_string(&Test::B(INNER)).unwrap(),
                r#"{"type":"B","value":{"x":true}}"#
            );
            assert_eq_str!(
                serde_json::to_string(&Test::C(INNER, INNER)).unwrap(),
                r#"{"type":"C","value":[{"x":true},{"x":true}]}"#
            );
            assert_eq_str!(
                serde_json::to_string(&Test::D).unwrap(),
                r#"{"type":"D"}"#
            );
            assert_eq_str!(
                test_emit::<Test>(),
                r#"export default types;
export namespace types {
    export type Inner = {
        "x": boolean;
    };
    export type Test = ({
        "type": "A";
        "value": {
            "a": types.Inner;
        };
    } | {
        "type": "B";
        "value": types.Inner;
    } | {
        "type": "C";
        "value": [types.Inner, types.Inner];
    } | {
        "type": "D";
    });
}
"#
            );
        }

        #[test]
        fn untagged() {
            #[derive(Serialize, TypeDef)]
            #[serde(untagged)]
            enum Test {
                A { a: Inner },
                B(Inner),
                C(Inner, Inner),
                D,
            }

            assert_eq_str!(
                serde_json::to_string(&Test::A { a: INNER }).unwrap(),
                r#"{"a":{"x":true}}"#
            );
            assert_eq_str!(
                serde_json::to_string(&Test::B(INNER)).unwrap(),
                r#"{"x":true}"#
            );
            assert_eq_str!(
                serde_json::to_string(&Test::C(INNER, INNER)).unwrap(),
                r#"[{"x":true},{"x":true}]"#
            );
            assert_eq_str!(serde_json::to_string(&Test::D).unwrap(), r#"null"#);
            assert_eq_str!(
                test_emit::<Test>(),
                r#"export default types;
export namespace types {
    export type Inner = {
        "x": boolean;
    };
    export type Test = ({
        "a": types.Inner;
    } | types.Inner | [types.Inner, types.Inner] | null);
}
"#
            );
        }

        #[test]
        fn none() {
            #[derive(Serialize, TypeDef)]
            enum Test {
                A { a: Inner },
                B(Inner),
                C(Inner, Inner),
                D,
            }

            assert_eq_str!(
                serde_json::to_string(&Test::A { a: INNER }).unwrap(),
                r#"{"A":{"a":{"x":true}}}"#
            );
            assert_eq_str!(
                serde_json::to_string(&Test::B(INNER)).unwrap(),
                r#"{"B":{"x":true}}"#
            );
            assert_eq_str!(
                serde_json::to_string(&Test::C(INNER, INNER)).unwrap(),
                r#"{"C":[{"x":true},{"x":true}]}"#
            );
            assert_eq_str!(serde_json::to_string(&Test::D).unwrap(), r#""D""#);
            assert_eq_str!(
                test_emit::<Test>(),
                r#"export default types;
export namespace types {
    export type Inner = {
        "x": boolean;
    };
    export type Test = ({
        "A": {
            "a": types.Inner;
        };
    } | {
        "B": types.Inner;
    } | {
        "C": [types.Inner, types.Inner];
    } | "D");
}
"#
            );
        }
    }

    #[test]
    fn docs() {
        /// enum `Test`
        #[derive(Serialize, TypeDef)]
        enum Test {
            /// struct variant `Test::A`
            A {
                /// struct variant field `Test::A.a`
                a: Test2,
            },
            /// newtype variant `Test::B`
            B(
                /// newtype variant field `Test::B.0`
                String,
            ),
            /// tuple variant `Test::C`
            C(
                /// tuple variant field `Test::C.0`
                String,
                /// tuple variant field `Test::C.1`
                String,
            ),
            /// unit variant `Test::D`
            D,
        }

        /// struct `Test2`
        #[derive(Serialize, TypeDef)]
        struct Test2 {
            /// field `a` of `Test2`
            pub a: Test3,
        }

        /// struct `Test3`
        #[derive(Serialize, TypeDef)]
        #[type_def(namespace = "test")]
        struct Test3 {
            /// field `a` of `Test3`
            pub a: String,
        }

        assert_eq_str!(
            test_emit::<Test>(),
            r#"export default types;
export namespace types {
    export namespace test {

        /**
         * struct `Test3`
         */
        export type Test3 = {

            /**
             * field `a` of `Test3`
             */
            "a": string;
        };
    }

    /**
     * struct `Test2`
     */
    export type Test2 = {

        /**
         * field `a` of `Test2`
         */
        "a": types.test.Test3;
    };

    /**
     * enum `Test`
     */
    export type Test = ({

        /**
         * struct variant `Test::A`
         */
        "A": {

            /**
             * struct variant field `Test::A.a`
             */
            "a": types.Test2;
        };
    } | {

        /**
         * newtype variant `Test::B`
         */
        "B": string;
    } | {

        /**
         * tuple variant `Test::C`
         */
        "C": [string, string];
    } | 
    /**
     * unit variant `Test::D`
     */
"D");
}
"#
        );
    }

    #[test]
    fn raw_idents() {
        #[allow(non_camel_case_types)]
        #[derive(Serialize, TypeDef)]
        struct r#struct {
            r#field: String,
        }

        #[allow(non_camel_case_types)]
        #[derive(Serialize, TypeDef)]
        enum r#enum {
            r#variant1,
            r#variant2 { r#field: r#struct },
        }

        assert_eq_str!(
            serde_json::to_string(&r#enum::r#variant1).unwrap(),
            r#""variant1""#
        );
        assert_eq_str!(
            serde_json::to_string(&r#enum::r#variant2 {
                r#field: r#struct {
                    r#field: "foo".to_owned()
                }
            })
            .unwrap(),
            r#"{"variant2":{"field":{"field":"foo"}}}"#
        );
        assert_eq_str!(
            test_emit::<r#enum>(),
            r#"export default types;
export namespace types {
    export type struct = {
        "field": string;
    };
    export type enum = ("variant1" | {
        "variant2": {
            "field": types.struct;
        };
    });
}
"#
        );
    }

    #[test]
    fn namespace() {
        #[derive(Serialize, TypeDef)]
        #[type_def(namespace = "x.y.z")]
        struct Test {
            a: String,
        }

        assert_eq_str!(
            test_emit::<Test>(),
            r#"export default types;
export namespace types {
    export namespace x.y.z {
        export type Test = {
            "a": string;
        };
    }
}
"#
        );
    }

    #[test]
    fn generics() {
        #[derive(Serialize, TypeDef)]
        struct Test<'a, A>
        where
            A: fmt::Display,
        {
            #[serde(skip)]
            _marker: PhantomData<&'a ()>,
            a: HashMap<String, A>,
        }

        impl<'a, A> fmt::Display for Test<'a, A>
        where
            A: fmt::Display,
        {
            fn fmt(&self, _f: &mut fmt::Formatter<'_>) -> fmt::Result {
                unimplemented!()
            }
        }

        #[derive(Serialize, TypeDef)]
        struct Test2 {
            a: Test<'static, String>,
            b: Option<Test<'static, usize>>,
            c: HashMap<String, Test<'static, usize>>,
            d: Test<'static, Test<'static, usize>>,
        }

        assert_eq_str!(
            test_emit::<Test2>(),
            r#"export default types;
export namespace types {
    export type Test<A> = {
        "a": Record<string, A>;
    };
    export type Usize = number;
    export type Test2 = {
        "a": types.Test<string>;
        "b": (types.Test<types.Usize> | null);
        "c": Record<string, types.Test<types.Usize>>;
        "d": types.Test<types.Test<types.Usize>>;
    };
}
"#
        );
    }

    #[test]
    fn default() {
        #[derive(Serialize, TypeDef)]
        struct Test {
            #[serde(default)]
            a: String,
            #[serde(default = "default_b")]
            b: String,
        }

        fn default_b() -> String {
            "foobar".to_owned()
        }

        assert_eq_str!(
            test_emit::<Test>(),
            r#"export default types;
export namespace types {
    export type Test = {
        "a"?: string;
        "b"?: string;
    };
}
"#
        );
    }

    #[test]
    fn foreign_field() {
        #[derive(Serialize)]
        #[serde(transparent)]
        struct ExternalString(String);

        #[derive(Serialize, TypeDef)]
        #[serde(transparent)]
        struct ExternalStringWrapper(
            #[type_def(type_of = "String")] ExternalString,
        );

        #[derive(Serialize, TypeDef)]
        struct Test {
            #[type_def(type_of = "String")]
            a: ExternalString,
            b: ExternalStringWrapper,
            c: String,
            #[type_def(type_of = "i16")]
            d: usize,
            #[type_def(flatten, type_of = "Test3")]
            e: Test2,
        }

        #[derive(Serialize)]
        struct Test2 {
            foo: String,
        }

        #[derive(Serialize, TypeDef)]
        struct Test3 {
            bar: String,
        }

        assert_eq_str!(
            test_emit::<Test>(),
            r#"export default types;
export namespace types {
    export type Test3 = {
        "bar": string;
    };
    export type ExternalStringWrapper = string;
    export type I16 = number;
    export type Test = (types.Test3 & {
        "a": string;
        "b": types.ExternalStringWrapper;
        "c": string;
        "d": types.I16;
    });
}
"#
        );
    }

    #[test]
    fn no_root_namespace() {
        #[derive(Serialize, TypeDef)]
        struct Test {
            a: usize,
        }

        let mut buf = Vec::new();
        let options = DefinitionFileOptions {
            header: None,
            root_namespace: None,
        };
        write_definition_file::<_, Test>(&mut buf, options).unwrap();
        let result = String::from_utf8(buf).unwrap();

        assert_eq_str!(
            result,
            r#"export type Usize = number;
export type Test = {
    "a": Usize;
};
"#
        );
    }
}

#[cfg(feature = "json_value")]
mod json_value {
    use super::test_emit;
    use serde::Serialize;
    use typescript_type_def::TypeDef;
    #[test]
    fn json_value() {
        #[derive(Serialize, TypeDef)]
        struct Test {
            a: String,
            b: serde_json::Value,
            c: serde_json::Number,
        }

        assert_eq_str!(
            test_emit::<Test>(),
            r#"export default types;
export namespace types {
    export type JSONValue = (null | boolean | number | string | (JSONValue)[] | {
        [key:string]:JSONValue;
    });
    export type Test = {
        "a": string;
        "b": types.JSONValue;
        "c": number;
    };
}
"#
        );
    }
}

mod write_ref_expr {
    use super::*;

    #[test]
    fn root_namespace() {
        #[derive(Serialize, TypeDef)]
        struct Test {
            a: usize,
        }

        let mut buf = Vec::new();
        Test::INFO.write_ref_expr(&mut buf, Some("types")).unwrap();
        let result = String::from_utf8(buf).unwrap();

        assert_eq_str!(result, r#"types.Test"#);
    }

    #[test]
    fn no_root_namespace() {
        #[derive(Serialize, TypeDef)]
        struct Test {
            a: usize,
        }

        let mut buf = Vec::new();
        Test::INFO.write_ref_expr(&mut buf, None).unwrap();
        let result = String::from_utf8(buf).unwrap();

        assert_eq_str!(result, r#"Test"#);
    }

    #[test]
    fn generics() {
        #[derive(Serialize, TypeDef)]
        struct Test<T> {
            a: T,
        }

        let mut buf = Vec::new();
        Test::<Vec<u8>>::INFO
            .write_ref_expr(&mut buf, Some("types"))
            .unwrap();
        let result = String::from_utf8(buf).unwrap();

        assert_eq_str!(result, r#"types.Test<(types.U8)[]>"#);
    }
}

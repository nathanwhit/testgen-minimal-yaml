#![warn(clippy::all)]
#![warn(clippy::pedantic)]
#![allow(unused)]
extern crate proc_macro;

use heck::SnakeCase;
use minimal_yaml::{Entry, Yaml};
use proc_macro::TokenStream as ProcTokenStream;
use quote::{format_ident, quote};
use regex::RegexSetBuilder;
use serde::Serialize;
use std::error::Error;
use std::fs::File;
use std::io::Read;
use std::path::{Path, PathBuf};
use syn::LitStr;
use walkdir::{DirEntry, WalkDir};

use derive_builder::Builder;

type TestgenError = Box<dyn Error + 'static>;

fn is_hidden(entry: &DirEntry) -> bool {
    entry
        .file_name()
        .to_str()
        .map_or(false, |s| s.starts_with('.'))
}
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum TestKind {
    PassSuccess,
    FailSuccess,
}

impl Default for TestKind {
    fn default() -> Self {
        Self::PassSuccess
    }
}

#[derive(Debug, Clone, Builder)]
struct Test {
    name: String,
    id: String,
    #[builder(setter(into))]
    input_path: PathBuf,
    #[builder(setter(into, strip_option))]
    #[builder(default = "None")]
    output_path: Option<PathBuf>,
    #[builder(default = "None")]
    #[builder(setter(into, strip_option))]
    error_msg: Option<String>,
    #[builder(setter(into))]
    event_path: PathBuf,
    #[builder(default = "TestKind::PassSuccess")]
    kind: TestKind,
}

fn collect_tests<P: AsRef<Path>>(path: P) -> Result<Vec<Test>, TestgenError> {
    let walker = WalkDir::new(&path);
    let mut tests: Vec<Test> = Vec::new();

    let mut builder = TestBuilder::default();
    for entry in walker.into_iter().filter_entry(|e| !is_hidden(e)) {
        let entry = entry.unwrap();
        if entry.file_type().is_file() {
            match entry.file_name().to_str() {
                Some("===") => {
                    let mut contents = String::new();
                    let mut file = File::open(entry.path()).unwrap();
                    file.read_to_string(&mut contents).unwrap();
                    builder.name(String::from(contents.trim()));
                }
                Some("in.yaml") => {
                    builder.input_path(entry.path());
                }
                Some("out.yaml") => {
                    builder.output_path(entry.path());
                }
                Some("test.event") => {
                    builder.event_path(entry.path());
                }
                Some("error") => {
                    // println!("error");
                    builder.kind(TestKind::FailSuccess);
                    let mut contents = String::new();
                    let mut file = File::open(entry.path()).unwrap();
                    file.read_to_string(&mut contents).unwrap();
                    builder.error_msg(String::from(contents.trim()));
                }
                _ => (),
            }
        } else if entry.file_type().is_dir() {
            let built_test = builder.build();
            // println!("{:?}", built_test);
            if let Ok(test) = built_test {
                tests.push(test)
            }
            builder = TestBuilder::default();
            if let Some(id) = entry.file_name().to_str() {
                // println!("{}", id);
                builder.id(String::from(id));
            }
        }
    }
    Ok(tests)
}
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum YamlKind {
    Scalar,
    Sequence,
    Mapping,
}

macro_rules! impl_from_yaml_for_yamlkind {
    ($typ : ty) => {
        impl From<$typ> for YamlKind {
            fn from(other: $typ) -> Self {
                match other {
                    Yaml::Mapping(..) => Self::Mapping,
                    Yaml::Sequence(..) => Self::Sequence,
                    Yaml::Scalar(..) => Self::Scalar,
                }
            }
        }
    };
}

impl_from_yaml_for_yamlkind!(Yaml<'_>);
impl_from_yaml_for_yamlkind!(&Yaml<'_>);
impl_from_yaml_for_yamlkind!(&mut Yaml<'_>);

#[derive(Debug, Clone, Default)]
struct IEntry<'a> {
    key: IYaml<'a>,
    value: IYaml<'a>,
}

impl<'a> From<Entry<'a>> for IEntry<'a> {
    fn from(other: Entry<'a>) -> Self {
        Self {
            key: other.key.into(),
            value: other.value.into(),
        }
    }
}

impl<'a> From<IEntry<'a>> for Entry<'a> {
    fn from(other: IEntry<'a>) -> Self {
        Self {
            key: other.key.into(),
            value: other.value.into(),
        }
    }
}

#[derive(Debug, Clone)]
enum IYaml<'a> {
    Scalar(&'a str),
    Sequence(Vec<IYaml<'a>>),
    Mapping(Vec<IEntry<'a>>),
    Dummy,
    EndSeq,
    EndMap,
}

impl<'a> Default for IYaml<'a> {
    fn default() -> Self {
        Self::Dummy
    }
}

impl<'a> From<Yaml<'a>> for IYaml<'a> {
    fn from(other: Yaml<'a>) -> Self {
        match other {
            Yaml::Scalar(s) => Self::Scalar(s),
            Yaml::Sequence(seq) => Self::Sequence(seq.into_iter().map(Self::from).collect()),
            Yaml::Mapping(mapping) => {
                Self::Mapping(mapping.into_iter().map(IEntry::from).collect())
            }
        }
    }
}
impl<'a> From<IYaml<'a>> for Yaml<'a> {
    fn from(other: IYaml<'a>) -> Self {
        match other {
            IYaml::Scalar(s) => Self::Scalar(s),
            IYaml::Sequence(seq) => Self::Sequence(seq.into_iter().map(Self::from).collect()),
            IYaml::Mapping(mapping) => {
                Self::Mapping(mapping.into_iter().map(Entry::from).collect())
            }
            _ => panic!("Internal structure shouldn't exist here!"),
        }
    }
}

fn pop_if_match<'a, 'b>(
    vect: &'a mut Vec<Yaml<'b>>,
    kind: YamlKind,
) -> Result<Yaml<'b>, TestgenError> {
    match vect.last() {
        Some(val) => {
            let val_kind = YamlKind::from(val);
            if val_kind == kind {
                Ok(vect.pop().unwrap())
            } else {
                Err(format!("Expected {:?} but found {:?}", kind, val_kind).into())
            }
        }
        None => Err(format!("Expected {:?} but found None", kind).into()),
    }
}

fn parse_event_to_iyaml<'a>(buf: &'a str) -> Result<Vec<IYaml<'a>>, TestgenError> {
    let mut lines = buf.lines();
    let mut stack: Vec<IYaml> = Vec::new();
    for line in lines {
        let (plus_minus, rest) = line.split_at(1);
        let typ = &rest[..3];
        match plus_minus {
            "+" => match typ {
                "STR" | "DOC" => (),
                "MAP" => stack.push(IYaml::Mapping(Vec::new())),
                "SEQ" => stack.push(IYaml::Sequence(Vec::new())),
                _ => return Err("Unexpected event specifier".into()),
            },
            "-" => match typ {
                "STR" | "DOC" => (),
                "MAP" => stack.push(IYaml::EndMap),
                "SEQ" => stack.push(IYaml::EndSeq),
                _ => return Err("Unexpected event specifier".into()),
            },
            "=" => match typ {
                "VAL" => {
                    let content: &str = &rest[4..];
                    if content.starts_with(':') {
                        let val = content.trim_start_matches(':');
                        stack.push(IYaml::Scalar(val));
                    } else {
                        stack.push(IYaml::Scalar(content));
                        // return Err(format!("Test event contains tags -- {}", content).into());
                    }
                }
                _ => return Err(format!("Expected VAL, found {}", typ).into()),
            },
            _ => return Err("Expected a '+', '-', or '='".into()),
        }
    }
    Ok(stack)
}

fn parse_mapping(stack: &mut Vec<IYaml>) -> Result<(), TestgenError> {
    // println!("Mapping : {:?}", stack);
    let mut to_push = Vec::new();
    loop {
        match stack.pop() {
            Some(IYaml::Mapping(mut v)) if v.is_empty() => {
                while let Some(key) = to_push.pop() {
                    let entry = if let Some(value) = to_push.pop() {
                        IEntry { key, value }
                    } else {
                        println!("Key was {:?}", key);
                        println!("Stack was {:?}", stack);
                        return Err("Expected value but found none".into());
                    };
                    v.push(entry);
                }
                stack.push(IYaml::Mapping(v));
                return Ok(());
            }

            Some(IYaml::EndMap) => parse_mapping(stack)?,
            Some(IYaml::EndSeq) => parse_seq(stack)?,
            Some(el) => to_push.push(el),
            _ | None => return Err("Expected entries".into()),
        }
    }
}

fn parse_seq(stack: &mut Vec<IYaml>) -> Result<(), TestgenError> {
    // println!("Seq : {:?}", stack);
    let mut to_push = Vec::new();
    loop {
        match stack.pop() {
            Some(IYaml::Sequence(mut v)) if v.is_empty() => {
                to_push.drain(..).rev().for_each(|e| v.push(e));
                stack.push(IYaml::Sequence(v));
                return Ok(());
            }
            Some(IYaml::EndMap) => parse_mapping(stack)?,
            Some(IYaml::EndSeq) => parse_seq(stack)?,
            Some(el) => to_push.push(el),
            None => return Err("Unexpected end of sequence".into()),
        }
    }
}

fn parse_test_event<'a, 'b>(test: &'a Test, buf: &'b str) -> Result<Yaml<'b>, TestgenError> {
    let mut stack = parse_event_to_iyaml(&buf).map_err(|e| {
        format!(
            "{} error occurred while parsing test {:?}",
            e.to_string(),
            test
        )
    })?;
    if let Some(element) = stack.pop() {
        match element {
            IYaml::EndMap => parse_mapping(&mut stack).map_err(|e| {
                format!(
                    "{} error occurred while parsing test {:?}",
                    e.to_string(),
                    test
                )
            })?,
            IYaml::EndSeq => parse_seq(&mut stack).map_err(|e| {
                format!(
                    "{} error occurred while parsing test {:?}",
                    e.to_string(),
                    test
                )
            })?,
            IYaml::Scalar(..) => return Ok(element.into()),
            _ => return Err("Expected a mapping or sequence at the top-level".into()),
        }
    } else {
        return Ok(Yaml::Sequence(vec![]));
    }

    Ok(stack.pop().unwrap().into())
}

fn filter_tests<'a>(
    tests: &'a [Test],
    blacklist: &'a [&str],
) -> Result<Vec<&'a Test>, TestgenError> {
    let regexes = RegexSetBuilder::new(blacklist)
        .case_insensitive(true)
        .build()?;
    Ok(tests
        .iter()
        .filter(|t| !regexes.is_match(&t.name))
        .collect())
}

#[proc_macro]
pub fn yaml_test_gen(input: ProcTokenStream) -> ProcTokenStream {
    let raw_path = syn::parse::<LitStr>(input).unwrap().value();
    let path = PathBuf::from(raw_path);
    let tests = collect_tests(path).unwrap();
    let filtered_tests = filter_tests(
        &tests,
        &["set", "tag", "anchor", "alias", "directive", "fold", "node"],
    )
    .unwrap();
    let mut test_decls = filtered_tests.iter().filter_map(|test| {
        match test.kind {
            TestKind::PassSuccess => Some({
                let mut event_file = File::open(&test.event_path).unwrap();
                let mut buf = String::new();
                event_file.read_to_string(&mut buf).unwrap();
                let expected = parse_test_event(test, &buf)
                    .map_err(|e| eprintln!("{}", e))
                    .ok();
                let expected = match expected {
                    Some(yml) => bincode::serialize(&Yaml::from(&yml)).unwrap(),
                    // Some(yml) => syn::parse_str::<syn::Expr>(yml.to_string()),
                    None => return None,
                };
                let test_pth = test.input_path.to_str().unwrap();
                let test_name = format_ident!(
                    "test_{}_{}",
                    test.name.replace('.', "_").to_snake_case(),
                    test.id
                );
                let mut expected_str = String::new();
                let mut expected_item = quote! {
                    vec![#(#expected),*]
                };
                // println!("Test name = {}", test_name);
                quote! {
                    #[test]
                    fn #test_name() {
                        use crate::Yaml::*;
                        use crate::Yaml;
                        use serde::Deserialize;
                        use std::io::Read;
                        let mut file = std::fs::File::open(#test_pth).unwrap();
                        let mut buf = String::new();
                        file.read_to_string(&mut buf).unwrap();
                        let exp_lit = #expected_item;
                        let res = crate::parse(&buf).unwrap();
                        let expected: Yaml = bincode::deserialize(&exp_lit).unwrap();
                        assert_eq!(expected, res);
                    }
                }
            }),
            _ => None,
        }
    });

    let decl = quote! {
        mod yaml_test_suite {
            #(#test_decls)*
        }
    };
    decl.into()
}

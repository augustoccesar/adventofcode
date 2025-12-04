mod csharp;
mod golang;
mod java;
mod python;
mod ruby;
mod rust;
mod typescript;

use std::{collections::HashMap, fmt::Display, path::PathBuf};

use crate::languages::{
    csharp::CSharp, golang::Golang, java::Java, python::Python, ruby::Ruby, rust::Rust,
    typescript::Typescript,
};

pub trait ManagedLanguage {
    fn run(&self, year: u16, day: u8);
    fn available_days(&self) -> HashMap<u16, Vec<u8>>;
    fn prepare_day(&self, year: u16, day: u8);
    fn path_to_day(&self, year: u16, day: u8) -> PathBuf;
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord, clap::ValueEnum)]
pub enum Language {
    #[value(alias("cs"))]
    CSharp,
    #[value(alias("go"))]
    Golang,
    Java,
    #[value(alias("py"))]
    Python,
    #[value(alias("rb"))]
    Ruby,
    #[value(alias("rs"))]
    Rust,
    #[value(alias("ts"))]
    Typescript,
}

impl Language {
    pub fn managed(&self) -> Box<dyn ManagedLanguage> {
        match self {
            Language::CSharp => Box::new(CSharp),
            Language::Golang => Box::new(Golang),
            Language::Java => Box::new(Java),
            Language::Python => Box::new(Python),
            Language::Ruby => Box::new(Ruby),
            Language::Rust => Box::new(Rust),
            Language::Typescript => Box::new(Typescript),
        }
    }

    pub fn all() -> impl Iterator<Item = Language> {
        [
            Self::CSharp,
            Self::Golang,
            Self::Java,
            Self::Python,
            Self::Ruby,
            Self::Rust,
            Self::Typescript,
        ]
        .into_iter()
    }

    pub fn icon(&self) -> String {
        let language_str: &str = self.into();

        format!("resources/icons/{language_str}.svg")
    }
}

impl Display for Language {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Language::CSharp => f.write_str("C#"),
            Language::Golang => f.write_str("Golang"),
            Language::Java => f.write_str("Java"),
            Language::Python => f.write_str("Python"),
            Language::Ruby => f.write_str("Ruby"),
            Language::Rust => f.write_str("Rust"),
            Language::Typescript => f.write_str("Typescript"),
        }
    }
}

impl From<Language> for &str {
    fn from(value: Language) -> Self {
        (&value).into()
    }
}

impl From<&Language> for &str {
    fn from(value: &Language) -> Self {
        match value {
            Language::CSharp => "csharp",
            Language::Golang => "golang",
            Language::Java => "java",
            Language::Python => "python",
            Language::Ruby => "ruby",
            Language::Rust => "rust",
            Language::Typescript => "typescript",
        }
    }
}

// Parse the format:
// <year>;<day1>;<day2>;...<dayN>\n
// <year>;<day1>;<day2>;...<dayN>
fn parse_year_available_days(input: &str) -> HashMap<u16, Vec<u8>> {
    let mut map = HashMap::new();
    for line in input.trim_end().split("\n") {
        let items = line.split(";").collect::<Vec<_>>();
        if items.is_empty() {
            continue;
        }

        let year = items[0].parse::<u16>().unwrap();
        let mut days = Vec::with_capacity(25);

        for day in &items[1..] {
            days.push(day.parse::<u8>().unwrap());
        }

        map.insert(year, days);
    }

    map
}

use crate::languages::Language;

#[derive(clap::Args)]
pub struct Args {
    language: Language,
    year: u16,
    day: u8,
}

impl Args {
    pub fn handle(&self) {
        self.language.managed().run(self.year, self.day);
    }
}

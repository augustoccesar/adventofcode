use crate::languages::Language;

#[derive(clap::Args)]
pub struct Args {
    language: Language,
    year: u16,
    day: u8,
}

impl Args {
    pub fn handle(&self) {
        self.language.managed().prepare_day(self.year, self.day);
    }
}

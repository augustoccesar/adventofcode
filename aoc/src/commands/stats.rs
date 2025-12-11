use std::{
    collections::{BTreeMap, HashMap},
    sync::{Arc, Mutex},
    thread,
    time::Duration,
};

use crate::languages::Language;

#[derive(clap::Args)]
pub struct Args {}

impl Args {
    pub fn handle(&self) {
        dbg!(generate());
    }
}

#[derive(Debug, Clone)]
struct Resolution {
    pub language: Language,
    pub part_one: Option<Duration>,
    pub part_two: Option<Duration>,
}

type Registry = BTreeMap<u16, HashMap<u8, Vec<Resolution>>>;
type ThreadRegistry = Arc<Mutex<Registry>>;

fn generate() -> Registry {
    let years_registry: ThreadRegistry = Arc::new(Mutex::new(BTreeMap::new()));

    let mut thread_handles = vec![];

    for language in Language::all() {
        let registry = Arc::clone(&years_registry);
        let handle = thread::spawn(move || {
            println!("[RUNNING] Building language project...");
            language.managed().build();
            println!("[DONE] Built!");

            println!("[RUNNING] Collecting days done in {language}...");
            let available_days = language.managed().available_days();
            println!("[DONE] Collected!");

            for (year_number, days) in &available_days {
                for day_number in days {
                    println!("[RUNNING] Day {day_number} for year {year_number}...");

                    let resolution: Vec<(String, Duration)> =
                        language.managed().formatted_run(*year_number, *day_number);

                    let mut registry_guard = registry.lock().unwrap();
                    registry_guard
                        .entry(*year_number)
                        .or_default()
                        .entry(*day_number)
                        .or_default()
                        .push(Resolution {
                            language: language.clone(),
                            part_one: resolution.get(0).map(|r| r.1),
                            part_two: resolution.get(1).map(|r| r.1),
                        });

                    println!("[DONE] Day {day_number} for year {year_number}...");
                }
            }

            println!("[DONE] {language}");
        });

        thread_handles.push(handle);
    }

    for handle in thread_handles {
        handle
            .join()
            .expect("should be able to join language day discovery thread to main thread");
    }

    // TODO(augustoccesar)[2025-12-11]: Look into Arc::try_unwrap
    years_registry.lock().unwrap().clone()
}

use std::{
    fs::{self, OpenOptions},
    io::Write,
    process,
};

pub fn prepare_day(year: u16, day: u8) {
    let day_file_path = crate::base_path().join(&format!("golang/y{year}/d{day:0>2}.go"));
    let year_package_exists = day_file_path.parent().unwrap().exists();

    if !day_file_path.exists() {
        day_file_path
            .parent()
            .map(|path| fs::create_dir_all(path).unwrap());

        let mut day_file = OpenOptions::new()
            .write(true)
            .create(true)
            .open(day_file_path)
            .unwrap();

        day_file
            .write_all(
                DAY_TEMPLATE
                    .replace("$padded_day", &format!("{day:0>2}"))
                    .replace("$day", &format!("{}", day))
                    .replace("$year", &format!("{}", year))
                    .as_bytes(),
            )
            .unwrap();

        let mut main_file = OpenOptions::new()
            .read(true)
            .write(true)
            .open(crate::base_path().join("golang/main.go"))
            .unwrap();

        crate::file::insert_codegen(
            &mut main_file,
            "target_dict",
            &format!("DayMapKey{{{year}, {day}}}: &y{year}.Day{day:0>2}{{}},\n"),
        );

        if !year_package_exists {
            crate::file::insert_codegen(
                &mut main_file,
                "target_import",
                &format!("\"com.github/augustoccesar/adventofcode/golang/y{year}\"\n"),
            );
        }

        process::Command::new("go")
            .arg("fmt")
            .current_dir(crate::base_path().join("golang"))
            .status()
            .unwrap();
    }
}

const DAY_TEMPLATE: &str = r#"package y$year

type Day$padded_day struct{}

func (d *Day$padded_day) Year() int { return $year }
func (d *Day$padded_day) Day() int  { return $day }

func (d *Day$padded_day) PartOne() string {
	return "-"
}

func (d *Day$padded_day) PartTwo() string {
	return "-"
}

"#;

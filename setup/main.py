import json
import os
import sys

from os.path import exists

def main():
    subcommand = sys.argv[1]
    match subcommand:
        case "prepare":
            if len(sys.argv) != 4:
                raise ValueError("Invalid amount of arguments")

            year = int(sys.argv[2])
            day = int(sys.argv[3])
            prepare_handler(year, day)


SETTINGS = json.load(open("./setup/settings.json", "r"))

def prepare_handler(year: int, day: int):
    padded_day = "{:0>2}".format(day)

    template_path =f"./setup/templates/{year}"
    if not exists(template_path):
        raise FileNotFoundError(f"Template for year {year} not found in the ./templates folder")

    if str(year) not in SETTINGS.keys():
        raise ValueError(f"Settings not found for year {year}")

    settings = SETTINGS[str(year)]
    task_destination = f"./{year}{settings['tasks_path']}/day{padded_day}"
    if exists(task_destination):
        raise Exception(f"Folder for day {day} in year {year} already exists")

    os.makedirs(task_destination)

    template = open(template_path, "r").read()
    template = template.replace("$padded_day", padded_day).replace("$day", str(day))

    task_full_path = f"{task_destination}/day{padded_day}.{settings['extension']}"
    with open(task_full_path, "w") as file:
        file.write(template)

    for instruction in settings['instructions']:
        with open(f"./{year}/{instruction['file']}", "r+") as file:
            file_content = file.read()
            
            label_idx = file_content.find(instruction["label"])
            file_content = file_content[:label_idx] + instruction["data"] + instruction["label"] + file_content[label_idx+len(instruction["label"]):]
            file_content = file_content.replace("$padded_day", padded_day).replace("$day", str(day))
            
            file.seek(0)
            file.write(file_content)

if __name__ == "__main__":
    main()

import json
import os
import re
import sys
from os.path import exists
from typing import Dict

import markdownify
import requests
from bs4 import BeautifulSoup

SETTINGS = json.load(open("./setup/settings.json", "r"))


def prepare_handler(year_param: str, day_param: str):
    year = int(year_param)
    day = int(day_param)

    padded_day = "{:0>2}".format(day)

    template_path = f"./setup/templates/{year}"
    if not exists(template_path):
        raise FileNotFoundError(
            f"Template for year {year} not found in the templates folder")

    if str(year) not in SETTINGS.keys():
        raise ValueError(f"Settings not found for year {year}")

    settings: Dict = SETTINGS[str(year)]
    task_destination = f"./{year}{settings['tasks_path']}/day{padded_day}"
    if exists(task_destination):
        raise Exception(f"Folder for day {day} in year {year} already exists")

    os.makedirs(task_destination)

    create_readme(year, day, task_destination)

    template = open(template_path, "r").read()
    template = template.replace(
        "$padded_day", padded_day).replace("$day", str(day))

    task_file_name = settings.get("task_file_name") or f"day{padded_day}"
    task_file_name_format = settings.get("task_file_name_format")
    if task_file_name_format and task_file_name_format == "camel":
        task_file_name = task_file_name.title()

    task_full_path = f"{task_destination}/{task_file_name}.{settings['extension']}"
    with open(task_full_path, "w") as file:
        file.write(template)

    if "instructions" not in settings.keys():
        return
    
    for instruction in settings['instructions']:
        with open(f"./{year}/{instruction['file']}", "r+") as file:
            file_content = file.read()

            label_idx = file_content.find(instruction["label"])
            file_content = file_content[:label_idx] + instruction["data"] + \
                instruction["label"] + \
                file_content[label_idx+len(instruction["label"]):]
            file_content = file_content.replace(
                "$padded_day", padded_day).replace("$day", str(day))

            file.seek(0)
            file.write(file_content)


def create_readme(year: int, day: int, destination: str):
    body = requests.get(f"https://adventofcode.com/{year}/day/{day}").text
    soup = BeautifulSoup(body, 'html.parser')

    article = str(soup.find("article"))
    article = article.replace("h2", "h1")

    markdown = markdownify.markdownify(
        article, heading_style="ATX", strip=["a"])
    markdown = markdown.replace("--- ", "").replace(" ---", "")
    markdown = re.sub(r"\n{2,}", "\n\n", markdown)

    if (markdown[-2:] == "\n\n"):
        markdown = markdown[:-1]

    with open(f"{destination}/README.md", "w") as f:
        f.write(markdown)


# TODO: Look into a CLI library to make this less weird
SUBCOMMANDS = {
    "prepare": {
        "params": 2,
        "handler": prepare_handler
    }
}


def main():
    if len(sys.argv) < 2:
        print(
            f"Missing subcommand. Must be one of [{','.join(SUBCOMMANDS.keys())}]")
        exit(1)

    if sys.argv[1] not in SUBCOMMANDS.keys():
        print(f"Invalid subcommand '{sys.argv[1]}'")
        exit(1)

    subcommand = SUBCOMMANDS[sys.argv[1]]
    if len(sys.argv[2:]) != subcommand["params"]:
        print("Wrong number of arguments")
        exit(1)

    subcommand["handler"](*sys.argv[2:])


if __name__ == "__main__":
    main()

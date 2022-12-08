# 🎄🎅 Advent of Code 🎅🎄

| Year | Language | Status | 1 - 5 | 6 - 10 | 11 - 15 | 16 - 20 | 21 - 25 |
| :-- | :-- | :-: | :-: | :-: | :-: |  :-: |  :-: | 
| [2022](2022/README.md) | ![Kotlin](https://img.shields.io/badge/Solved%20with-Kotlin-7F52FF) | [![2022](https://img.shields.io/github/workflow/status/augustoccesar/adventofcode/2022?label=2022)](https://github.com/augustoccesar/adventofcode/actions/workflows/test-2022.yml) | ✅✅✅✅✅ | ✅✅✅❌❌ | ❌❌❌❌❌ | ❌❌❌❌❌ | ❌❌❌❌❌ |
| [2021](2021/README.md) | ![Ruby](https://img.shields.io/badge/Solved%20with-Ruby-CC342D) | | ✅✅✅✅✅ | ✅✅✅✅✅ | ✅✅❌✅🟡 | ❌❌❌❌❌ | ❌❌❌❌❌ |
| [2020](2020/README.md) | ![Golang](https://img.shields.io/badge/Solved%20with-Go-79D4FD) | [![2020](https://img.shields.io/github/workflow/status/augustoccesar/adventofcode/2020?label=2020)](https://github.com/augustoccesar/adventofcode/actions/workflows/test-2020.yml) | ✅✅✅✅✅| ✅✅✅✅✅ | ✅✅✅✅✅ | ✅✅✅✅✅ | ✅✅✅✅✅ |
| [2019](2019/README.md) | ![Java](https://img.shields.io/badge/Solved%20with-Java-F0931C) | [![2019](https://img.shields.io/github/workflow/status/augustoccesar/adventofcode/2019?label=2019)](https://github.com/augustoccesar/adventofcode/actions?query=workflow%3A2019) | ✅✅✅✅✅ | ✅✅✅✅✅ | ✅🟡✅✅❌ | ❌❌❌❌❌ | ❌❌❌❌❌ |
| [2018](2018/README.md) | ![Python](https://img.shields.io/badge/Solved%20with-Python-F7CA3E) | | ✅✅✅✅✅ | ✅✅✅✅✅ | ❌❌❌❌❌ | ❌❌❌❌❌ | ❌❌❌❌❌ |
| [2017](2017/README.md) | ![Cpp](https://img.shields.io/badge/Solved%20with-C++-00427E) | | ✅✅✅✅✅ | ✅✅✅✅✅ | ❌❌❌❌❌ | ❌❌❌❌❌ | ❌❌❌❌❌ |
| [2016](2016/README.md) | ![Rust](https://img.shields.io/badge/Solved%20with-Rust-A72145) | | ✅✅✅✅✅ | ✅✅✅✅✅ | ❌❌❌❌❌ | ❌❌❌❌❌ | ❌❌❌❌❌ |
| [2015](2015/README.md) | ![Typescript](https://img.shields.io/badge/Solved%20with-Typescript-3178C6) | | ✅✅✅✅✅ | ✅✅✅✅✅ | ❌❌❌❌❌ | ❌❌❌❌❌ | ❌❌❌❌❌ |


<table>
    <tr>
        <td align="center">❌</td>
        <td align="left">Not started</td>
    </tr>
    <tr>
        <td align="center">🟡</td>
        <td align="left">Partially done</td>
    </tr>
    <tr>
        <td align="center">✅</td>
        <td align="left">Fully done</td>
    </tr>
</table>

## Helpers
### Requirements:
- Python 3.10+

### Setup
To use the helpers, first run
```shell
./bin/setup.sh
```

This will install the Python dependencies.

### Scripts
- `./bin/prepare-day.sh {year} {day}`
    - Prepare the folder of the day on the year
    - Use the template for the year found on `setup/templates` to generate the task file
    - Create a README based on the page on Advent of Code website
    - Update the year project based on the `setup/settings.json` instructions

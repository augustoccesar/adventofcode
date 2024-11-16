# 🎄🎅 Advent of Code 🎅🎄

| Year | Language | Status | 1 - 5 | 6 - 10 | 11 - 15 | 16 - 20 | 21 - 25 |
| :-- | :-- | :-: | :-: | :-: | :-: |  :-: |  :-: | 
| [2024](2024/README.md) | ![C#](https://img.shields.io/badge/C%23-blue) | | ❌❌❌❌❌ | ❌❌❌❌❌ | ❌❌❌❌❌ | ❌❌❌❌❌ | ❌❌❌❌❌ |
| [2023](2023/README.md) | ![Rust](https://img.shields.io/badge/Rust-A72145) | | ✅✅✅✅✅ | ✅✅✅✅✅ | ✅✅✅✅✅ | ✅✅✅❌❌ | ❌❌❌❌❌ |
| [2022](2022/README.md) | ![Zig](https://img.shields.io/badge/Zig-orange) | | ✅✅✅✅✅ | ✅✅✅🟡❌ | ❌❌❌❌❌ | ❌❌❌❌❌ | ❌❌❌❌❌ |
| [2021](2021/README.md) | ![Ruby](https://img.shields.io/badge/Ruby-CC342D) | | ✅✅✅✅✅ | ✅✅✅✅✅ | ✅✅✅✅🟡 | ❌❌❌❌❌ | ❌❌❌❌❌ |
| [2020](2020/README.md) | ![Golang](https://img.shields.io/badge/Go-79D4FD) | [![2020](https://github.com/augustoccesar/adventofcode/actions/workflows/test-2020.yml/badge.svg)](https://github.com/augustoccesar/adventofcode/actions/workflows/test-2020.yml) | ✅✅✅✅✅| ✅✅✅✅✅ | ✅✅✅✅✅ | ✅✅✅✅✅ | ✅✅✅✅✅ |
| [2019](2019/README.md) | ![Java](https://img.shields.io/badge/Java-F0931C) | [![2019](https://github.com/augustoccesar/adventofcode/actions/workflows/test-2019.yml/badge.svg)](https://github.com/augustoccesar/adventofcode/actions/workflows/test-2019.yml) | ✅✅✅✅✅ | ✅✅✅✅✅ | ✅🟡✅✅❌ | ❌❌❌❌❌ | ❌❌❌❌❌ |
| [2018](2018/README.md) | ![Python](https://img.shields.io/badge/Python-F7CA3E) | | ✅✅✅✅✅ | ✅✅✅✅✅ | ❌❌❌❌❌ | ❌❌❌❌❌ | ❌❌❌❌❌ |
| [2017](2017/README.md) | ![Cpp](https://img.shields.io/badge/C++-00427E) | | ✅✅✅✅✅ | ✅✅✅✅✅ | ✅❌❌❌❌ | ❌❌❌❌❌ | ❌❌❌❌❌ |
| [2016](2016/README.md) | ![Rust](https://img.shields.io/badge/Rust-A72145) | | ✅✅✅✅✅ | ✅✅✅✅✅ | ❌❌❌❌❌ | ❌❌❌❌❌ | ❌❌❌❌❌ |
| [2015](2015/README.md) | ![Typescript](https://img.shields.io/badge/Typescript-3178C6) | | ✅✅✅✅✅ | ✅✅✅✅✅ | ❌❌❌❌❌ | ❌❌❌❌❌ | ❌❌❌❌❌ |


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
    - NOTE: Your AoC Session token must be present on the env variable `AOC_SESSION` to be able to download the input

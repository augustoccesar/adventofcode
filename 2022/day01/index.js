import { readInput } from "../input.js";

/**
 * @returns {Promise<string>}
 */
export async function partOne() {
  const elfs = await caloriesPerElf();
  let max = 0;
  elfs.forEach(elf => {
    if (elf > max) {
      max = elf;
    }
  });

  return max.toString();
}

/**
 * @returns {Promise<string>}
 */
export async function partTwo() {
  const elfs = await caloriesPerElf();
  const topElfs = elfs.sort((a, b) => b - a).slice(0, 3);

  return topElfs
    .reduce((partialSum, item) => partialSum + item)
    .toString();
}

/**
 * @returns {Promise<Array<number>>}
 */
async function caloriesPerElf() {
  return (await readInput("01"))
    .split("\n\n")
    .map(group => {
      return group
        .split("\n")
        .map(item => parseInt(item))
        .reduce((partialSum, item) => partialSum + item);
    });
}

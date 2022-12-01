import { readInput } from "../input.js";

/**
 * @returns {Promise<string>}
 */
export async function partOne() {
  const elfs = await resolveInput();
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
  const elfs = await resolveInput();
  const topElfs = elfs.sort((a, b) => b - a).slice(0, 3);

  return topElfs
    .reduce((partialSum, item) => partialSum + item)
    .toString();
}

async function resolveInput() {
  const rawData = await readInput("01");
  const groups = rawData.split("\n\n");

  const elfs = [];
  groups.forEach(group => {
    const groupSum = group
      .split("\n")
      .map(item => parseInt(item))
      .reduce((partialSum, item) => partialSum + item);

    elfs.push(groupSum);
  });

  return elfs;
}

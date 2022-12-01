import { readInput } from "../input.js";

/**
 * @returns {Promise<string>}
 */
export async function partOne() {
  const rawData = await readInput("01");
  const groups = rawData.split("\n\n");

  let max = 0;
  groups.forEach(group => {
    const groupSum = group
      .split("\n")
      .map(item => parseInt(item))
      .reduce((partialSum, item) => partialSum + item);

    if(groupSum > max) {
      max = groupSum;
    }
  });

  return max.toString();
}

/**
 * @returns {Promise<string>}
 */
export async function partTwo() {
  return "-";
}

import { readFile } from "fs/promises";

/**
 * @param {string} day padded day string. E.g. 01
 * @param {*} suffix 
 * @returns {string} file data
 */
export async function readInput(day, suffix = "input") {
  const file = await readFile(`./inputs/day${day}_${suffix}.txt`);
  return file.toString();
}

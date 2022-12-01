import * as assert from "assert";
import { readFile } from "fs/promises";
import {describe, it, before} from "mocha";

const data = (await readFile("./expected_results")).toString().split("\n").map(row => row.split(";"));

data.forEach(async ([day, expectedPartOne, expectedPartTwo]) => {
  let module;

  before(async () => {
    module = await import(`../day${day}/index.js`);
  });

  describe(`Day ${day}`, () => {
    it("should return correct value for part one", async () => {
      const res = await module.partOne();
      assert.equal(res, expectedPartOne, "Wrong result");
    });

    it("should return correct value for part two", async () => {
      const res = await module.partTwo();
      assert.equal(res, expectedPartTwo, "Wrong result");
    });
  });
});

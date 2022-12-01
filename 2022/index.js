import { performance } from "perf_hooks";
import { exit, argv } from "process";

/**
 * @param {string} day
 * @returns {any}
 */
async function loadDayModule(day) {
  try {
    return await import(`./day${day}/index.js`);
  } catch (error) {
    console.error(`Failed to load day ${day} module.`);
    console.error(error);
    exit(1);
  }
}

/**
 * @param {number} totalTimeMs 
 * @returns {string}
 */
function msToTime(totalTimeMs) {
  function pad(n, z) {
    z = z || 2;
    return ("00" + n).slice(-z);
  }

  var ms = totalTimeMs % 1000;
  totalTimeMs = (totalTimeMs - ms) / 1000;
  var secs = totalTimeMs % 60;
  totalTimeMs = (totalTimeMs - secs) / 60;
  var mins = totalTimeMs % 60;

  return pad(mins) + ":" + pad(secs) + "." + pad(ms, 3);
}

/**
 * @param {string} header 
 * @param {Function} func 
 */
async function executeTimed(header, func) {
  const startTime = performance.now();
  const result = await func();
  const endTime = performance.now();

  const totalTime = endTime - startTime;
  const timeStr = msToTime(totalTime);

  console.log(`${header}: ${result}\n(took ${timeStr})\n`);
}

// NOTE(augustoccesar)[2022-12-01]:
//  When using 'yarn start' it is .slice(3), but if using directly 'node', it should
//  be .slice(2). Maybe do this dinamic somehow at some point?
const args = argv.slice(3);
if (args.length != 1) {
  console.error("Invalid number of arguments");
  exit(1);
}

const paddedDay = args[0].toString().padStart(2, "0");
const module = await loadDayModule(paddedDay);

if (!module.partOne) {
  console.error(`Loaded module for day ${paddedDay} doesn't export method partOne`);
  exit(1);
}

if (!module.partTwo) {
  console.error(`Loaded module for day ${paddedDay} doesn't export method partTwo`);
  exit(1);
}

await executeTimed("Part One", module.partOne);
await executeTimed("Part Two", module.partTwo);

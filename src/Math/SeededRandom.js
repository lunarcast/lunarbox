"use strict"

/**
 * Generate a seeded random int inside the interval [min, max).
 *
 * @param seed The seed to use for the generation.
 * @param min THe minimum value.
 * @param max The maximum value.
 */
exports.seededInt = (seed) => (min) => (max) => {
  const generator = seedrandom(seed)
  return min + Math.floor((max - min) * generator())
}

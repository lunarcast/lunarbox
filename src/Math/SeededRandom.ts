import seedrandom from "seedrandom";

/**
 * Generate a seeded random int inside the interval [min, max).
 *
 * @param seed The seed to use for the generation.
 * @param min THe minimum value.
 * @param max The maximum value.
 */
export const seededInt = (seed: string) => (min: number) => (max: number) => {
  const generator = seedrandom(seed);

  return min + Math.floor((max - min) * generator());
};

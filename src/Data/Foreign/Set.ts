/**
 * Converts an array to a set (here for using from within purescript).
 *
 * @param arr The array to convert.
 */
export const arrayToSet = <T>(arr: T[]) => new Set(arr)

/**
 * Find the last element in an array to pass a predicate.
 *
 * @param arr The array to search trough.
 * @param predicate The predicate to run on all elements.
 */
export const findLast = <T>(
  arr: T[],
  predicate: (v: T) => boolean
): T | null => {
  return arr.reduce(
    (acc, curr) => (predicate(curr) ? curr : acc),
    null as T | null
  )
}

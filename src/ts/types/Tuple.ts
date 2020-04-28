/**
 * This is how purescript represents tuples at runtime.
 *
 * I defined this type so I can use it in my interop code.
 */
export type Tuple<T, U> = {
  value0: T;
  value1: U;
};

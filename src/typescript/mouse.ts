/**
 * Enum containing mouse buttons we can check for.
 */
export const enum MouseButtons {
  LeftButton = 1,
  Wheel = -1, // Todo: find the correct code for this
  RightButton = 2
}

/**
 * Check if a mouse button is pressed.
 *
 * @param button The button to check for..
 * @param bits The bits from the event object.
 */
export const isPressed = (bits: number) => (button: MouseButtons) =>
  bits & button

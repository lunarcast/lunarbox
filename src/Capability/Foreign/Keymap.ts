import MouseTrap, { CallbackFunction } from "mousetrap-ts";
import { Tuple } from "../../ts/types/Tuple";

type Keymap = Array<Tuple<string, CallbackFunction>>;

const traps = new Map<String, MouseTrap>();

/**
 * Takes a keymap and a label and returns an effect which mounts the keymap.
 *
 * @param label The label to remember the keybinding by
 */
export const mountKeymap = (label: string) => (keymap: Keymap) => () => {
  const trap = new MouseTrap(document.body);

  traps.set(label, trap);

  keymap.forEach(({ value0: key, value1: callback }) => {
    trap.bind(key, callback, "keypress");
  });
};

/**
 * Does the cleanup after a keymap is no longer needed
 *
 * @param label The label the keymap was assigned to
 */
export const unmountKeymap = (label: string) => () => {
  const trap = traps.get(label);

  if (trap) {
    trap.destroy();

    traps.delete(label);
  }
};

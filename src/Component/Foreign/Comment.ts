// Source: https://gomakethings.com/automatically-expand-a-textarea-as-the-user-types-using-vanilla-javascript/
export const autoExpandImpl = (minHeight: number) => (
  element: HTMLTextAreaElement
) => () => {
  // Reset field height
  element.style.height = "inherit";

  // Get the computed styles for the element
  const computed = window.getComputedStyle(element);

  // Calculate the height
  const rawHeight =
    parseFloat(computed.getPropertyValue("border-top-width")) +
    element.scrollHeight +
    parseFloat(computed.getPropertyValue("border-bottom-width"));

  const height = Math.max(rawHeight, minHeight);

  element.style.height = `${height}px`;

  return height;
};

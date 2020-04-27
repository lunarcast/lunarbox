/**
 * Looks like halogen refs don't work on svg so I had to do this.
 */
export const getSceneBoundingBox = () => {
  const svg = document.getElementById("scene")!;

  return svg.getBoundingClientRect();
};

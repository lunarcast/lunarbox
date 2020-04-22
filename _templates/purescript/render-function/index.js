const toCamelCase = (s) => `${s[0].toLowerCase()}${s.slice(1)}`;

exports.prompt = async ({ args }) => ({
  ...args,
  renderFunction: toCamelCase(args.name.split(".").pop()),
});

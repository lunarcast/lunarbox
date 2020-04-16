exports.prompt = async ({ args }) => ({
  ...args,
  folder: args.page || args.p ? "Page" : "Component",
});

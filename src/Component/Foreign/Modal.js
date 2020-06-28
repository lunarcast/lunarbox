"use strict"

const micromodal = require("micromodal")

// Reexporting those for use from withing purescript
exports.showModal = (name) => () =>
  new Promise((resolve) => {
    let resolved = false

    micromodal.show(name, {
      onClose: (element) => {
        if (resolved) return

        resolved = true

        resolve(element)
      },
      awaitCloseAnimation: true
    })
  })

exports.closeModal = (name) => () => micromodal.close(name)

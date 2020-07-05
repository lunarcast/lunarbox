import { IToHiccup } from "@thi.ng/api"
import { TextAttribs, TextElement } from "../types/Hiccup"
import { Vec2Like, add2, mul2, mulN2 } from "@thi.ng/vectors"
import { Rect } from "@thi.ng/geom"

export interface TWBAttribs {
  padding: Vec2Like
}

/**
 * Text with colored background.
 */
export class TextWithBackground<
  T extends Omit<any, keyof TextAttribs> = {},
  U extends Omit<any, keyof TWBAttribs> = {}
> implements IToHiccup {
  private _value = ""
  private _font = ""
  private dirty = false
  public bg = new Rect([0, 0], [0, 0], {}) as Rect & {
    attribs: Partial<TWBAttribs> & U
  }

  public constructor(
    public attribs: Partial<Omit<TextAttribs, "font">> & T,
    bgAttribs: Partial<TWBAttribs> & U,
    public pos: Vec2Like = [0, 0]
  ) {
    this.bg.attribs = bgAttribs
  }

  private get textElement(): TextElement {
    return [
      "text",
      { ...this.attribs, font: this._font },
      [this.pos[0], this.pos[1] + (this.bg.attribs.padding?.[1] ?? 0)],
      this.value
    ]
  }

  public get value() {
    return this._value
  }

  public get font() {
    return this._font
  }

  public set value(value: string) {
    this._value = value
    this.dirty = true
  }

  public set font(font: string) {
    this._font = font
    this.dirty = true
  }

  public resize(ctx: CanvasRenderingContext2D) {
    if (!this.dirty) return

    ctx.save()
    ctx.font = this._font

    const metrics = ctx.measureText(this._value)

    this.bg.size = [
      metrics.width,
      metrics.actualBoundingBoxAscent + metrics.actualBoundingBoxDescent
    ]

    add2(null, this.bg.size, mulN2([], this.bg.attribs.padding ?? [0, 0], 2))

    this.refresh()

    this.dirty = false
  }

  public refresh() {
    if (this.attribs.align === "center") {
      this.bg.pos[0] = this.pos[0] - this.bg.size[0] / 2
    }

    this.bg.pos[1] = this.pos[1]
  }

  public toHiccup() {
    const text = this.textElement

    return ["g", {}, this.bg, text]
  }
}

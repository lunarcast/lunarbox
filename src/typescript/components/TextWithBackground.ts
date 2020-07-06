import type { IToHiccup } from "@thi.ng/api"
import type { TextAttribs, TextElement } from "../types/Hiccup"
import { Vec2Like, add2, mulN2 } from "@thi.ng/vectors"
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
  private dirty = false
  private doublePadding: Vec2Like
  public bg = new Rect([0, 0], [0, 0], {}) as Rect & {
    attribs: Partial<TWBAttribs> & U
  }

  public constructor(
    public attribs: Partial<Omit<TextAttribs, "font">> & T,
    bgAttribs: Partial<TWBAttribs> & U,
    private _value = "",
    private _font = "",
    public pos: Vec2Like = [0, 0]
  ) {
    this.bg.attribs = bgAttribs
    this.doublePadding = mulN2(
      [],
      this.bg.attribs.padding ?? [0, 0],
      2
    ) as Vec2Like

    if (_font !== "" || _value !== "") {
      this.dirty = true
    }
  }

  private get textElement(): TextElement {
    const paddingY = this.bg.attribs.padding?.[1] ?? 0

    return [
      "text",
      { ...this.attribs, font: this._font },
      [
        this.pos[0],
        this.pos[1] +
          (this.attribs.baseline === "baseline"
            ? -paddingY
            : this.attribs.baseline === "hanging"
            ? paddingY
            : 0)
      ],
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

    ctx.restore()

    this.bg.size = [
      metrics.width,
      metrics.actualBoundingBoxAscent + metrics.actualBoundingBoxDescent
    ]

    add2(null, this.bg.size, this.doublePadding)

    this.refresh()

    this.dirty = false
  }

  public refresh() {
    if (this.attribs.align === "center") {
      this.bg.pos[0] = this.pos[0] - this.bg.size[0] / 2
    }

    if (this.attribs.baseline === "baseline") {
      this.bg.pos[1] = this.pos[1] - this.bg.size[1]
    } else if (this.attribs.baseline === "middle") {
      this.bg.pos[1] = this.pos[1] - this.bg.size[1] / 2
    } else {
      this.bg.pos[1] = this.pos[1]
    }
  }

  public toHiccup() {
    const text = this.textElement

    return ["g", {}, this.bg, text]
  }
}

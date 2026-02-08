module H = Html
module Slug = Slug

type program = {
  filename: string,
  slug: string,
  title: string,
  summary: string,
  tags: array<string>,
  width: int,
  height: int,
  grid: array<string>,
  rawCode: string,
}

let trim = s => s->Js.String2.trim

let parseDocLine = (line: string) => {
  // expects: "; Key: Value"
  let line = line->trim
  if !Js.String2.startsWith(line, ";") {
    None
  } else {
    let body = line->Js.String2.sliceToEnd(1)->trim
    switch body->Js.String2.indexOf(":") {
    | -1 => None
    | idx =>
      let k = body->Js.String2.slice(0, idx)->trim->Js.String2.toLowerCase
      let v = body->Js.String2.sliceToEnd(idx + 1)->trim
      Some((k, v))
    }
  }
}

let splitLines = (s: string) => s->Js.String2.split("\n")

let normalizeEol = (s: string) => s->Js.String2.replaceAll("\r\n", "\n")->Js.String2.replaceAll("\r", "\n")

let takeDocBlock = (lines: array<string>) => {
  // doc block is initial consecutive lines starting with ';'
  let rec loop = (i, acc) =>
    if i >= Belt.Array.length(lines) {
      (acc, i)
    } else {
      let line = lines[i]
      if line->trim->Js.String2.startsWith(";") {
        loop(i + 1, Belt.Array.concat(acc, [line]))
      } else {
        (acc, i)
      }
    }
  loop(0, [||])
}

let parseTags = (s: string) => {
  s->Js.String2.split(",")->Belt.Array.map(trim)->Belt.Array.keep(t => t != "")
}

let padRight = (s: string, w: int) => {
  let len = s->Js.String2.length
  if len >= w {
    s
  } else {
    s ++ Js.String2.make(w - len, " ")->Belt.Array.joinWith("")
  }
}

let clamp = (n, lo, hi) => if n < lo { lo } else if n > hi { hi } else { n }

let parseProgram = (content: string, filename: string, maxW: int, maxH: int): program => {
  let content = content->normalizeEol
  let lines = content->splitLines

  let (docLines, codeStart) = takeDocBlock(lines)

  // defaults
  let title =
    filename->Js.String2.replaceByRe(%re("/\\.bf$/i"), "")
  let summary = ""
  let tags: array<string> = [||]

  // fold doc fields
  let (title, summary, tags) =
    docLines
    ->Belt.Array.reduce(((title, summary, tags), line) => {
      switch parseDocLine(line) {
      | None => (title, summary, tags)
      | Some((k, v)) =>
        switch k {
        | "title" => (v, summary, tags)
        | "summary" => (title, v, tags)
        | "tags" => (title, summary, parseTags(v))
        | _ => (title, summary, tags)
        }
      }
    }, (title, summary, tags))

  // code lines (rest)
  let codeLines = lines->Belt.Array.slice(codeStart, Belt.Array.length(lines) - codeStart)

  // compute grid size with caps
  let widthRaw =
    codeLines->Belt.Array.reduce(0, (w, line) => {
      let len = line->Js.String2.length
      if len > w { len } else { w }
    })

  let width = clamp(widthRaw, 0, maxW)
  let height = clamp(Belt.Array.length(codeLines), 0, maxH)

  let cropped = codeLines->Belt.Array.slice(0, height)

  let grid =
    cropped
    ->Belt.Array.map(line => {
      let line = if line->Js.String2.length > width { line->Js.String2.slice(0, width) } else { line }
      padRight(line, width)
    })

  let rawCode = grid->H.joinLines

  let slug = title->Slug.slugify

  {
    filename,
    slug,
    title,
    summary,
    tags,
    width,
    height,
    grid,
    rawCode,
  }
}

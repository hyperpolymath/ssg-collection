let isAlphaNum = c =>
  switch c {
  | 'a'..'z' | 'A'..'Z' | '0'..'9' => true
  | _ => false
  }

let lower = s => s->Js.String2.toLowerCase

let slugify = (s: string) => {
  let s = s->lower
  let buf = Belt.MutableQueue.make()
  let rec loop = (i, lastDash) =>
    if i >= s->Js.String2.length {
      ()
    } else {
      let ch = s->Js.String2.charAt(i)
      let c = ch->Js.String2.get(0)
      if isAlphaNum(c) {
        Belt.MutableQueue.add(buf, ch)
        loop(i + 1, false)
      } else if !lastDash {
        Belt.MutableQueue.add(buf, "-")
        loop(i + 1, true)
      } else {
        loop(i + 1, lastDash)
      }
    }
  loop(0, true)

  let out = buf->Belt.MutableQueue.toArray->Belt.Array.joinWith("")
  out->Js.String2.replaceByRe(%re("/^-+|-+$/g"), "")
}

let escape = (s: string) => {
  s
  ->Js.String2.replaceAll("&", "&amp;")
  ->Js.String2.replaceAll("<", "&lt;")
  ->Js.String2.replaceAll(">", "&gt;")
  ->Js.String2.replaceAll("\"", "&quot;")
  ->Js.String2.replaceAll("'", "&#39;")
}

let joinLines = (lines: array<string>) => lines->Belt.Array.joinWith("\n")

// WebSocket Bindings for Deno
// SPDX-License-Identifier: AGPL-3.0-or-later

type t

type readyState =
  | @as(0) Connecting
  | @as(1) Open
  | @as(2) Closing
  | @as(3) Closed

type messageEvent = {
  data: string,
}

type closeEvent = {
  code: int,
  reason: string,
}

@get external readyState: t => int = "readyState"

@send external send: (t, string) => unit = "send"
@send external close: (t, ~code: int=?, ~reason: string=?) => unit = "close"

@set external onopen: (t, unit => unit) => unit = "onopen"
@set external onmessage: (t, messageEvent => unit) => unit = "onmessage"
@set external onclose: (t, closeEvent => unit) => unit = "onclose"
@set external onerror: (t, 'error => unit) => unit = "onerror"

let isOpen = (ws: t): bool => readyState(ws) == 1

let sendJson = (ws: t, data: Js.Json.t): unit => {
  send(ws, Js.Json.stringify(data))
}

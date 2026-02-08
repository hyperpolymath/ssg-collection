// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 hyperpolymath
//
// Consent Plugin for poly-ssg family
// Extracted from dei-ssg for cross-SSG reuse
//
// Provides consent gates for:
// - External resource loading (fonts, scripts, iframes)
// - Analytics and tracking
// - Third-party embeds
// - Cookie/storage access

type consentCategory =
  | ExternalFonts
  | Analytics
  | ThirdPartyEmbeds
  | ExternalScripts
  | Cookies
  | LocalStorage
  | Geolocation
  | Camera
  | Microphone
  | Custom(string)

type consentState =
  | Granted
  | Denied
  | Pending
  | Expired

type consentRecord = {
  category: consentCategory,
  state: consentState,
  grantedAt: option<float>,
  expiresAt: option<float>,
  reason: option<string>,
}

type consentStore = {
  mutable records: array<consentRecord>,
  mutable defaultPolicy: consentState,
}

let createStore = (~defaultPolicy=Denied) => {
  records: [],
  defaultPolicy,
}

let categoryToString = (cat: consentCategory): string => {
  switch cat {
  | ExternalFonts => "external-fonts"
  | Analytics => "analytics"
  | ThirdPartyEmbeds => "third-party-embeds"
  | ExternalScripts => "external-scripts"
  | Cookies => "cookies"
  | LocalStorage => "local-storage"
  | Geolocation => "geolocation"
  | Camera => "camera"
  | Microphone => "microphone"
  | Custom(name) => name
  }
}

let stringToCategory = (str: string): consentCategory => {
  switch str {
  | "external-fonts" => ExternalFonts
  | "analytics" => Analytics
  | "third-party-embeds" => ThirdPartyEmbeds
  | "external-scripts" => ExternalScripts
  | "cookies" => Cookies
  | "local-storage" => LocalStorage
  | "geolocation" => Geolocation
  | "camera" => Camera
  | "microphone" => Microphone
  | name => Custom(name)
  }
}

let getConsent = (store: consentStore, category: consentCategory): consentState => {
  let now = Js.Date.now()

  store.records
  ->Array.find(r => r.category == category)
  ->Option.map(r => {
    // Check if expired
    switch r.expiresAt {
    | Some(exp) if exp < now => Expired
    | _ => r.state
    }
  })
  ->Option.getOr(store.defaultPolicy)
}

let setConsent = (
  store: consentStore,
  category: consentCategory,
  state: consentState,
  ~expiresInMs: option<float>=?,
  ~reason: option<string>=?,
) => {
  let now = Js.Date.now()
  let expiresAt = expiresInMs->Option.map(ms => now +. ms)

  let record: consentRecord = {
    category,
    state,
    grantedAt: state == Granted ? Some(now) : None,
    expiresAt,
    reason,
  }

  // Remove existing record for this category
  store.records = store.records->Array.filter(r => r.category != category)
  // Add new record
  store.records = store.records->Array.concat([record])
}

// Consent gate - the core "only if okay" pattern from WokeLang
let onlyIfOkay = (
  store: consentStore,
  category: consentCategory,
  ~onGranted: unit => 'a,
  ~onDenied: unit => 'a,
  ~onPending: unit => 'a,
): 'a => {
  switch getConsent(store, category) {
  | Granted => onGranted()
  | Denied => onDenied()
  | Pending | Expired => onPending()
  }
}

// Async consent gate with prompt
let askConsent = async (
  store: consentStore,
  category: consentCategory,
  ~promptFn: consentCategory => promise<bool>,
): consentState => {
  switch getConsent(store, category) {
  | Granted => Granted
  | _ => {
      let granted = await promptFn(category)
      let newState = granted ? Granted : Denied
      setConsent(store, category, newState)
      newState
    }
  }
}

// Batch consent operations
let grantAll = (store: consentStore, categories: array<consentCategory>) => {
  categories->Array.forEach(cat => setConsent(store, cat, Granted))
}

let denyAll = (store: consentStore, categories: array<consentCategory>) => {
  categories->Array.forEach(cat => setConsent(store, cat, Denied))
}

let revokeAll = (store: consentStore) => {
  store.records = []
}

// Serialize/deserialize for persistence
let toJson = (store: consentStore): Js.Json.t => {
  let recordsJson = store.records->Array.map(r => {
    Js.Dict.fromArray([
      ("category", Js.Json.string(categoryToString(r.category))),
      ("state", Js.Json.string(
        switch r.state {
        | Granted => "granted"
        | Denied => "denied"
        | Pending => "pending"
        | Expired => "expired"
        }
      )),
      ("grantedAt", r.grantedAt->Option.mapOr(Js.Json.null, Js.Json.number)),
      ("expiresAt", r.expiresAt->Option.mapOr(Js.Json.null, Js.Json.number)),
      ("reason", r.reason->Option.mapOr(Js.Json.null, Js.Json.string)),
    ])->Js.Json.object_
  })

  Js.Dict.fromArray([
    ("records", Js.Json.array(recordsJson)),
    ("defaultPolicy", Js.Json.string(
      switch store.defaultPolicy {
      | Granted => "granted"
      | Denied => "denied"
      | Pending => "pending"
      | Expired => "expired"
      }
    )),
  ])->Js.Json.object_
}

// Essential consent categories for GDPR/privacy compliance
let essentialCategories = [
  ExternalFonts,
  Analytics,
  ThirdPartyEmbeds,
  ExternalScripts,
  Cookies,
]

// Generate consent banner HTML
let generateBannerHtml = (categories: array<consentCategory>): string => {
  let categoryCheckboxes = categories
    ->Array.map(cat => {
      let id = categoryToString(cat)
      `<label>
        <input type="checkbox" name="consent" value="${id}" />
        ${id->String.replaceAll("-", " ")->String.charAt(0)->String.toUpperCase ++ id->String.sliceToEnd(~start=1)->String.replaceAll("-", " ")}
      </label>`
    })
    ->Array.join("\n")

  `<div class="consent-banner" role="dialog" aria-labelledby="consent-title">
  <h2 id="consent-title">Privacy Preferences</h2>
  <p>This site requests consent for the following features:</p>
  <form id="consent-form">
    ${categoryCheckboxes}
    <div class="consent-actions">
      <button type="button" onclick="denyAll()">Deny All</button>
      <button type="button" onclick="acceptSelected()">Accept Selected</button>
      <button type="button" onclick="acceptAll()">Accept All</button>
    </div>
  </form>
</div>`
}

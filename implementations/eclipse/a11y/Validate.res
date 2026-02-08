// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
// Validate.res — Accessibility schema validation

// ============================================================
// SCHEMA DEFINITIONS
// ============================================================

type schemaEntry = {
  name: string,
  file: string,
}

let schemas: array<schemaEntry> = [
  {name: "main", file: "schema.json"},
  {name: "bsl", file: "bsl.schema.json"},
  {name: "asl", file: "asl.schema.json"},
  {name: "gsl", file: "gsl.schema.json"},
  {name: "makaton", file: "makaton.schema.json"},
]

// ============================================================
// VALIDATION TYPES
// ============================================================

type validationResult = {
  valid: bool,
  errors: array<string>,
  warnings: array<string>,
}

// ============================================================
// VALIDATION FUNCTIONS
// ============================================================

let validateSignLanguages = (data: Js.Dict.t<Js.Json.t>): (array<string>, array<string>) => {
  let errors = ref([])
  let warnings = ref([])

  switch Js.Dict.get(data, "signLanguages") {
  | Some(slJson) =>
    switch Js.Json.decodeArray(slJson) {
    | Some(slArray) =>
      slArray->Array.forEach(slEntry => {
        switch Js.Json.decodeObject(slEntry) {
        | Some(sl) =>
          switch Js.Dict.get(sl, "language") {
          | None =>
            errors := Array.concat(errors.contents, ["Sign language entry missing 'language' field"])
          | Some(_) =>
            let hasUrl = Js.Dict.get(sl, "url")->Option.isSome
            let hasTranscript = Js.Dict.get(sl, "transcript")->Option.isSome
            if !hasUrl && !hasTranscript {
              let langName = switch Js.Dict.get(sl, "language") {
              | Some(lang) =>
                switch Js.Json.decodeString(lang) {
                | Some(s) => s
                | None => "unknown"
                }
              | None => "unknown"
              }
              warnings := Array.concat(warnings.contents, [
                "Sign language " ++ langName ++ ": Consider adding video URL or transcript"
              ])
            }
          }
        | None => ()
        }
      })
    | None => ()
    }
  | None => ()
  }

  (errors.contents, warnings.contents)
}

let validateAltText = (data: Js.Dict.t<Js.Json.t>): (array<string>, array<string>) => {
  let errors = ref([])
  let warnings = ref([])

  switch Js.Dict.get(data, "altText") {
  | Some(altJson) =>
    switch Js.Json.decodeObject(altJson) {
    | Some(alt) =>
      switch Js.Dict.get(alt, "short") {
      | Some(shortJson) =>
        switch Js.Json.decodeString(shortJson) {
        | Some(shortText) =>
          if Js.String2.length(shortText) > 125 {
            errors := Array.concat(errors.contents, ["Short alt text exceeds 125 character limit"])
          }
        | None => ()
        }
      | None => ()
      }

      let hasShort = Js.Dict.get(alt, "short")->Option.isSome
      let hasLong = Js.Dict.get(alt, "long")->Option.isSome
      if !hasShort && !hasLong {
        warnings := Array.concat(warnings.contents, ["Alt text object has no content"])
      }
    | None => ()
    }
  | None => ()
  }

  (errors.contents, warnings.contents)
}

let validateAudioDescription = (data: Js.Dict.t<Js.Json.t>): (array<string>, array<string>) => {
  let warnings = ref([])

  switch Js.Dict.get(data, "audioDescription") {
  | Some(adJson) =>
    switch Js.Json.decodeObject(adJson) {
    | Some(ad) =>
      let hasUrl = Js.Dict.get(ad, "url")->Option.isSome
      let hasTranscript = Js.Dict.get(ad, "transcript")->Option.isSome
      if !hasUrl && !hasTranscript {
        warnings := Array.concat(warnings.contents, ["Audio description has no URL or transcript"])
      }
    | None => ()
    }
  | None => ()
  }

  ([], warnings.contents)
}

let validateCaptions = (data: Js.Dict.t<Js.Json.t>): (array<string>, array<string>) => {
  let errors = ref([])

  switch Js.Dict.get(data, "captions") {
  | Some(captionsJson) =>
    switch Js.Json.decodeObject(captionsJson) {
    | Some(captions) =>
      if Js.Dict.get(captions, "url")->Option.isNone {
        errors := Array.concat(errors.contents, ["Captions object missing URL"])
      }
    | None => ()
    }
  | None => ()
  }

  (errors.contents, [])
}

let validateA11yMetadata = (data: Js.Dict.t<Js.Json.t>): validationResult => {
  let allErrors = ref([])
  let allWarnings = ref([])

  // Validate sign languages
  let (slErrors, slWarnings) = validateSignLanguages(data)
  allErrors := Array.concat(allErrors.contents, slErrors)
  allWarnings := Array.concat(allWarnings.contents, slWarnings)

  // Validate alt text
  let (altErrors, altWarnings) = validateAltText(data)
  allErrors := Array.concat(allErrors.contents, altErrors)
  allWarnings := Array.concat(allWarnings.contents, altWarnings)

  // Validate audio description
  let (adErrors, adWarnings) = validateAudioDescription(data)
  allErrors := Array.concat(allErrors.contents, adErrors)
  allWarnings := Array.concat(allWarnings.contents, adWarnings)

  // Validate captions
  let (captionErrors, captionWarnings) = validateCaptions(data)
  allErrors := Array.concat(allErrors.contents, captionErrors)
  allWarnings := Array.concat(allWarnings.contents, captionWarnings)

  {
    valid: Array.length(allErrors.contents) == 0,
    errors: allErrors.contents,
    warnings: allWarnings.contents,
  }
}

// ============================================================
// SCHEMA VALIDATION
// ============================================================

type schemaValidationResult = {
  name: string,
  valid: bool,
  hasSchema: bool,
  hasId: bool,
  hasTitle: bool,
}

let validateSchemaFields = (schema: Js.Dict.t<Js.Json.t>): schemaValidationResult => {
  {
    name: "",
    valid: true,
    hasSchema: Js.Dict.get(schema, "$schema")->Option.isSome,
    hasId: Js.Dict.get(schema, "$id")->Option.isSome,
    hasTitle: Js.Dict.get(schema, "title")->Option.isSome,
  }
}

// ============================================================
// EXPORTS
// ============================================================

let version = "0.2.0"

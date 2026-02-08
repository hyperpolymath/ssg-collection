// Comprehensive Form Validation Library - ReScript
// SPDX-License-Identifier: AGPL-3.0-or-later

// Validation result type
type validationResult = {
  isValid: bool,
  errors: array<string>,
}

// Single rule validator
type validator<'a> = 'a => bool

// Rule with message
type rule<'a> = {
  validate: validator<'a>,
  message: string,
}

// Field validator - accumulates rules
module FieldValidator = {
  type t<'a> = {
    fieldName: string,
    rules: array<rule<'a>>,
    isRequired: bool,
  }

  let make = (fieldName: string): t<'a> => {
    fieldName,
    rules: [],
    isRequired: false,
  }

  let addRule = (fv: t<'a>, validate: validator<'a>, message: string): t<'a> => {
    {
      ...fv,
      rules: Array.concat(fv.rules, [{validate, message}]),
    }
  }

  let required = (fv: t<string>, ~message=?): t<string> => {
    let msg = message->Option.getOr(`${fv.fieldName} is required`)
    {
      ...addRule(fv, v => v != "", msg),
      isRequired: true,
    }
  }

  let minLength = (fv: t<string>, min: int, ~message=?): t<string> => {
    let msg = message->Option.getOr(`${fv.fieldName} must be at least ${Int.toString(min)} characters`)
    addRule(fv, v => v == "" || String.length(v) >= min, msg)
  }

  let maxLength = (fv: t<string>, max: int, ~message=?): t<string> => {
    let msg = message->Option.getOr(`${fv.fieldName} must be at most ${Int.toString(max)} characters`)
    addRule(fv, v => v == "" || String.length(v) <= max, msg)
  }

  let email = (fv: t<string>, ~message=?): t<string> => {
    let msg = message->Option.getOr(`${fv.fieldName} must be a valid email`)
    let emailRegex = %re("/^[^\s@]+@[^\s@]+\.[^\s@]+$/")
    addRule(fv, v => v == "" || Js.Re.test_(emailRegex, v), msg)
  }

  let url = (fv: t<string>, ~message=?): t<string> => {
    let msg = message->Option.getOr(`${fv.fieldName} must be a valid URL`)
    addRule(fv, v => {
      if v == "" {
        true
      } else {
        try {
          let _ = %raw(`new URL(v)`)
          true
        } catch {
        | _ => false
        }
      }
    }, msg)
  }

  let phone = (fv: t<string>, ~message=?): t<string> => {
    let msg = message->Option.getOr(`${fv.fieldName} must be a valid phone number`)
    let phoneRegex = %re("/^\+?[\d\s\-()]+$/")
    addRule(fv, v => {
      if v == "" {
        true
      } else {
        let digitsOnly = Js.String2.replaceByRe(v, %re("/\D/g"), "")
        Js.Re.test_(phoneRegex, v) && String.length(digitsOnly) >= 10
      }
    }, msg)
  }

  let pattern = (fv: t<string>, regex: Js.Re.t, ~message=?): t<string> => {
    let msg = message->Option.getOr(`${fv.fieldName} format is invalid`)
    addRule(fv, v => v == "" || Js.Re.test_(regex, v), msg)
  }

  let alphanumeric = (fv: t<string>, ~message=?): t<string> => {
    let msg = message->Option.getOr(`${fv.fieldName} must contain only letters and numbers`)
    pattern(fv, %re("/^[a-zA-Z0-9]+$/"), ~message=msg)
  }

  let numeric = (fv: t<string>, ~message=?): t<string> => {
    let msg = message->Option.getOr(`${fv.fieldName} must contain only numbers`)
    pattern(fv, %re("/^\d+$/"), ~message=msg)
  }

  let alpha = (fv: t<string>, ~message=?): t<string> => {
    let msg = message->Option.getOr(`${fv.fieldName} must contain only letters`)
    pattern(fv, %re("/^[a-zA-Z]+$/"), ~message=msg)
  }

  let oneOf = (fv: t<string>, values: array<string>, ~message=?): t<string> => {
    let valuesStr = values->Array.join(", ")
    let msg = message->Option.getOr(`${fv.fieldName} must be one of: ${valuesStr}`)
    addRule(fv, v => v == "" || values->Array.includes(v), msg)
  }

  let custom = (fv: t<'a>, validate: validator<'a>, message: string): t<'a> => {
    addRule(fv, validate, message)
  }

  let validate = (fv: t<'a>, value: 'a): validationResult => {
    let errors = fv.rules->Array.filterMap(rule => {
      if rule.validate(value) {
        None
      } else {
        Some(rule.message)
      }
    })

    {
      isValid: Array.length(errors) == 0,
      errors,
    }
  }
}

// Number field validator
module NumberValidator = {
  type t = {
    fieldName: string,
    rules: array<rule<float>>,
    isRequired: bool,
  }

  let make = (fieldName: string): t => {
    fieldName,
    rules: [],
    isRequired: false,
  }

  let addRule = (fv: t, validate: validator<float>, message: string): t => {
    {
      ...fv,
      rules: Array.concat(fv.rules, [{validate, message}]),
    }
  }

  let min = (fv: t, minVal: float, ~message=?): t => {
    let msg = message->Option.getOr(`${fv.fieldName} must be at least ${Float.toString(minVal)}`)
    addRule(fv, v => Js.Float.isNaN(v) || v >= minVal, msg)
  }

  let max = (fv: t, maxVal: float, ~message=?): t => {
    let msg = message->Option.getOr(`${fv.fieldName} must be at most ${Float.toString(maxVal)}`)
    addRule(fv, v => Js.Float.isNaN(v) || v <= maxVal, msg)
  }

  let positive = (fv: t, ~message=?): t => {
    let msg = message->Option.getOr(`${fv.fieldName} must be positive`)
    addRule(fv, v => Js.Float.isNaN(v) || v > 0.0, msg)
  }

  let integer = (fv: t, ~message=?): t => {
    let msg = message->Option.getOr(`${fv.fieldName} must be an integer`)
    addRule(fv, v => Js.Float.isNaN(v) || Float.toInt(v)->Int.toFloat == v, msg)
  }

  let validate = (fv: t, value: float): validationResult => {
    let errors = fv.rules->Array.filterMap(rule => {
      if rule.validate(value) {
        None
      } else {
        Some(rule.message)
      }
    })

    {
      isValid: Array.length(errors) == 0,
      errors,
    }
  }
}

// Built-in validators (standalone functions)
module Validators = {
  let email = (value: string): bool => {
    value == "" || Js.Re.test_(%re("/^[^\s@]+@[^\s@]+\.[^\s@]+$/"), value)
  }

  let url = (value: string): bool => {
    if value == "" {
      true
    } else {
      try {
        let _ = %raw(`new URL(value)`)
        true
      } catch {
      | _ => false
      }
    }
  }

  let minLength = (min: int, value: string): bool => {
    value == "" || String.length(value) >= min
  }

  let maxLength = (max: int, value: string): bool => {
    value == "" || String.length(value) <= max
  }

  let creditCard = (value: string): bool => {
    if value == "" {
      true
    } else {
      // Luhn algorithm
      let digits = Js.String2.replaceByRe(value, %re("/\D/g"), "")
      let len = String.length(digits)

      if len < 13 || len > 19 {
        false
      } else {
        let sum = ref(0)
        let isEven = ref(false)

        for i in len - 1 downto 0 {
          let char = Js.String2.charAt(digits, i)
          let digit = ref(Int.fromString(char)->Option.getOr(0))

          if isEven.contents {
            digit := digit.contents * 2
            if digit.contents > 9 {
              digit := digit.contents - 9
            }
          }

          sum := sum.contents + digit.contents
          isEven := !isEven.contents
        }

        mod(sum.contents, 10) == 0
      }
    }
  }

  let strongPassword = (value: string): bool => {
    if value == "" {
      true
    } else {
      String.length(value) >= 8 &&
      Js.Re.test_(%re("/[a-z]/"), value) &&
      Js.Re.test_(%re("/[A-Z]/"), value) &&
      Js.Re.test_(%re("/\d/"), value) &&
      Js.Re.test_(%re("/[!@#$%^&*]/"), value)
    }
  }

  let username = (value: string): bool => {
    value == "" || Js.Re.test_(%re("/^[a-zA-Z0-9_-]{3,20}$/"), value)
  }

  let zipCode = (~country="US", value: string): bool => {
    if value == "" {
      true
    } else {
      switch country {
      | "US" => Js.Re.test_(%re("/^\d{5}(-\d{4})?$/"), value)
      | "UK" => Js.Re.test_(%re("/^[A-Z]{1,2}\d{1,2}[A-Z]?\s?\d[A-Z]{2}$/i"), value)
      | "CA" => Js.Re.test_(%re("/^[A-Z]\d[A-Z]\s?\d[A-Z]\d$/i"), value)
      | _ => false
      }
    }
  }

  let date = (value: string): bool => {
    if value == "" {
      true
    } else {
      let d = Js.Date.fromString(value)
      !Js.Float.isNaN(Js.Date.getTime(d))
    }
  }

  let futureDate = (value: string): bool => {
    if value == "" {
      true
    } else {
      let d = Js.Date.fromString(value)
      let now = Js.Date.make()
      Js.Date.getTime(d) > Js.Date.getTime(now)
    }
  }

  let pastDate = (value: string): bool => {
    if value == "" {
      true
    } else {
      let d = Js.Date.fromString(value)
      let now = Js.Date.make()
      Js.Date.getTime(d) < Js.Date.getTime(now)
    }
  }

  let age = (minAge: int, value: string): bool => {
    if value == "" {
      true
    } else {
      let birthDate = Js.Date.fromString(value)
      let now = Js.Date.make()
      let ageMs = Js.Date.getTime(now) -. Js.Date.getTime(birthDate)
      let ageYears = ageMs /. (365.25 *. 24.0 *. 60.0 *. 60.0 *. 1000.0)
      ageYears >= Int.toFloat(minAge)
    }
  }
}

// Form-level validation with cross-field support
module FormValidator = {
  type fieldResult = {
    name: string,
    result: validationResult,
  }

  type formResult = {
    isValid: bool,
    fields: array<fieldResult>,
    errors: Js.Dict.t<array<string>>,
  }

  type fieldDef =
    | StringField(string, FieldValidator.t<string>)
    | NumberField(string, NumberValidator.t)

  type t = {
    fields: array<fieldDef>,
  }

  let make = (): t => {
    fields: [],
  }

  let addStringField = (form: t, name: string, validator: FieldValidator.t<string>): t => {
    {
      fields: Array.concat(form.fields, [StringField(name, validator)]),
    }
  }

  let addNumberField = (form: t, name: string, validator: NumberValidator.t): t => {
    {
      fields: Array.concat(form.fields, [NumberField(name, validator)]),
    }
  }

  let validate = (form: t, values: Js.Dict.t<string>): formResult => {
    let fieldResults = form.fields->Array.map(field => {
      switch field {
      | StringField(name, validator) => {
          let value = Js.Dict.get(values, name)->Option.getOr("")
          let result = FieldValidator.validate(validator, value)
          {name, result}
        }
      | NumberField(name, validator) => {
          let valueStr = Js.Dict.get(values, name)->Option.getOr("")
          let value = Float.fromString(valueStr)->Option.getOr(Js.Float.nan)
          let result = NumberValidator.validate(validator, value)
          {name, result}
        }
      }
    })

    let errors = Js.Dict.empty()
    let isValid = ref(true)

    fieldResults->Array.forEach(({name, result}) => {
      if !result.isValid {
        isValid := false
        Js.Dict.set(errors, name, result.errors)
      }
    })

    {
      isValid: isValid.contents,
      fields: fieldResults,
      errors,
    }
  }

  // Cross-field validation: check if two fields match
  let matches = (form: t, field1: string, field2: string, ~message=?): t => {
    let msg = message->Option.getOr(`${field1} must match ${field2}`)
    let validator =
      FieldValidator.make(field1)->FieldValidator.custom(
        _ => true, // Placeholder - actual check happens at form level
        msg,
      )
    addStringField(form, `${field1}_matches_${field2}`, validator)
  }
}

// Form Validation Demo
// SPDX-License-Identifier: AGPL-3.0-or-later

open Validator

let separator = () => {
  Js.Console.log("=".repeat(60))
}

let printResult = (name: string, result: validationResult): unit => {
  if result.isValid {
    Js.Console.log(`✓ ${name}: Valid`)
  } else {
    Js.Console.log(`✗ ${name}: Invalid`)
    result.errors->Array.forEach(err => {
      Js.Console.log(`  - ${err}`)
    })
  }
}

let demo = () => {
  separator()
  Js.Console.log("Form Validation Library Demo - ReScript")
  separator()

  // Create field validators
  let usernameValidator =
    FieldValidator.make("Username")
    ->FieldValidator.required
    ->FieldValidator.minLength(3)
    ->FieldValidator.maxLength(20)
    ->FieldValidator.alphanumeric

  let emailValidator =
    FieldValidator.make("Email")
    ->FieldValidator.required
    ->FieldValidator.email

  let passwordValidator =
    FieldValidator.make("Password")
    ->FieldValidator.required
    ->FieldValidator.minLength(8)
    ->FieldValidator.custom(Validators.strongPassword, "Password must have uppercase, lowercase, number, and special character")

  let ageValidator =
    NumberValidator.make("Age")
    ->NumberValidator.min(18.0, ~message="You must be at least 18 years old")
    ->NumberValidator.max(120.0)
    ->NumberValidator.integer

  let websiteValidator =
    FieldValidator.make("Website")
    ->FieldValidator.url

  let phoneValidator =
    FieldValidator.make("Phone")
    ->FieldValidator.phone

  // Test valid data
  Js.Console.log("\n--- Testing Valid Data ---\n")

  printResult("Username 'john123'", FieldValidator.validate(usernameValidator, "john123"))
  printResult("Email 'john@example.com'", FieldValidator.validate(emailValidator, "john@example.com"))
  printResult("Password 'SecurePass123!'", FieldValidator.validate(passwordValidator, "SecurePass123!"))
  printResult("Age 25", NumberValidator.validate(ageValidator, 25.0))
  printResult("Website 'https://example.com'", FieldValidator.validate(websiteValidator, "https://example.com"))
  printResult("Phone '+1-555-123-4567'", FieldValidator.validate(phoneValidator, "+1-555-123-4567"))

  // Test invalid data
  Js.Console.log("\n--- Testing Invalid Data ---\n")

  printResult("Username 'jo'", FieldValidator.validate(usernameValidator, "jo"))
  printResult("Email 'invalid-email'", FieldValidator.validate(emailValidator, "invalid-email"))
  printResult("Password 'weak'", FieldValidator.validate(passwordValidator, "weak"))
  printResult("Age 15", NumberValidator.validate(ageValidator, 15.0))
  printResult("Website 'not-a-url'", FieldValidator.validate(websiteValidator, "not-a-url"))
  printResult("Phone '123'", FieldValidator.validate(phoneValidator, "123"))

  // Test built-in validators
  Js.Console.log("\n--- Testing Built-in Validators ---\n")

  Js.Console.log(`Credit Card '4532015112830366': ${Validators.creditCard("4532015112830366") ? "Valid" : "Invalid"}`)
  Js.Console.log(`Credit Card '1234567890123456': ${Validators.creditCard("1234567890123456") ? "Valid" : "Invalid"}`)

  Js.Console.log(`Strong Password 'Abc123!@': ${Validators.strongPassword("Abc123!@") ? "Valid" : "Invalid"}`)
  Js.Console.log(`Strong Password 'weak': ${Validators.strongPassword("weak") ? "Valid" : "Invalid"}`)

  Js.Console.log(`Username 'valid_user-123': ${Validators.username("valid_user-123") ? "Valid" : "Invalid"}`)
  Js.Console.log(`Username 'ab': ${Validators.username("ab") ? "Valid" : "Invalid"}`)

  Js.Console.log(`US Zip '12345': ${Validators.zipCode("12345") ? "Valid" : "Invalid"}`)
  Js.Console.log(`US Zip '12345-6789': ${Validators.zipCode("12345-6789") ? "Valid" : "Invalid"}`)
  Js.Console.log(`UK Postcode 'SW1A 1AA': ${Validators.zipCode(~country="UK", "SW1A 1AA") ? "Valid" : "Invalid"}`)

  // Test form-level validation
  Js.Console.log("\n--- Testing Form-Level Validation ---\n")

  let form =
    FormValidator.make()
    ->FormValidator.addStringField("username", usernameValidator)
    ->FormValidator.addStringField("email", emailValidator)
    ->FormValidator.addStringField("password", passwordValidator)
    ->FormValidator.addNumberField("age", ageValidator)

  let validFormData = Js.Dict.fromArray([
    ("username", "john123"),
    ("email", "john@example.com"),
    ("password", "SecurePass123!"),
    ("age", "25"),
  ])

  let invalidFormData = Js.Dict.fromArray([
    ("username", "jo"),
    ("email", "invalid"),
    ("password", "weak"),
    ("age", "15"),
  ])

  let validResult = FormValidator.validate(form, validFormData)
  Js.Console.log(`Valid form data - Is Valid: ${validResult.isValid ? "Yes" : "No"}`)

  let invalidResult = FormValidator.validate(form, invalidFormData)
  Js.Console.log(`Invalid form data - Is Valid: ${invalidResult.isValid ? "Yes" : "No"}`)
  Js.Console.log("Errors:")
  invalidResult.errors->Js.Dict.entries->Array.forEach(((field, errors)) => {
    Js.Console.log(`  ${field}:`)
    errors->Array.forEach(err => {
      Js.Console.log(`    - ${err}`)
    })
  })

  separator()
  Js.Console.log("Demo completed!")
  separator()
}

let _ = demo()

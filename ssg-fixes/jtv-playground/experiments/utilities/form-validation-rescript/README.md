# Form Validation - ReScript

A comprehensive, type-safe form validation library in ReScript.

## Features

- Chainable validation rules
- Built-in validators (email, URL, phone, credit card, etc.)
- Number validation (min, max, positive, integer)
- Custom validators
- Form-level validation
- Cross-field validation support

## Usage

### Field Validation

```rescript
open Validator

// Create a field validator with chained rules
let usernameValidator =
  FieldValidator.make("Username")
  ->FieldValidator.required
  ->FieldValidator.minLength(3)
  ->FieldValidator.maxLength(20)
  ->FieldValidator.alphanumeric

// Validate a value
let result = FieldValidator.validate(usernameValidator, "john123")
// { isValid: true, errors: [] }
```

### Number Validation

```rescript
let ageValidator =
  NumberValidator.make("Age")
  ->NumberValidator.min(18.0)
  ->NumberValidator.max(120.0)
  ->NumberValidator.integer

let result = NumberValidator.validate(ageValidator, 25.0)
```

### Built-in Validators

```rescript
Validators.email("test@example.com")      // true
Validators.url("https://example.com")     // true
Validators.creditCard("4532015112830366") // true (Luhn check)
Validators.strongPassword("Abc123!@")     // true
Validators.username("valid_user-123")     // true
Validators.zipCode("12345")               // true (US)
Validators.zipCode(~country="UK", "SW1A 1AA") // true
```

### Form Validation

```rescript
let form =
  FormValidator.make()
  ->FormValidator.addStringField("username", usernameValidator)
  ->FormValidator.addStringField("email", emailValidator)
  ->FormValidator.addNumberField("age", ageValidator)

let values = Js.Dict.fromArray([
  ("username", "john123"),
  ("email", "john@example.com"),
  ("age", "25"),
])

let result = FormValidator.validate(form, values)
// { isValid: true, fields: [...], errors: {} }
```

## Available Validators

### String Validators
- `required` - Value must not be empty
- `minLength(n)` - Minimum string length
- `maxLength(n)` - Maximum string length
- `email` - Valid email format
- `url` - Valid URL format
- `phone` - Valid phone number (10+ digits)
- `pattern(regex)` - Match custom regex
- `alphanumeric` - Letters and numbers only
- `numeric` - Numbers only
- `alpha` - Letters only
- `oneOf(values)` - Must be one of specified values
- `custom(fn, message)` - Custom validation function

### Number Validators
- `min(n)` - Minimum value
- `max(n)` - Maximum value
- `positive` - Must be positive
- `integer` - Must be whole number

### Standalone Validators
- `Validators.email`
- `Validators.url`
- `Validators.creditCard` - Luhn algorithm
- `Validators.strongPassword` - 8+ chars, upper, lower, number, special
- `Validators.username` - 3-20 chars, alphanumeric with _ and -
- `Validators.zipCode(~country)` - US, UK, CA formats
- `Validators.date` - Valid date
- `Validators.futureDate` - Date in future
- `Validators.pastDate` - Date in past
- `Validators.age(minAge)` - Minimum age from birth date

## Running

```bash
# Build ReScript
deno task build

# Run demo
deno task demo
```

## License

SPDX-License-Identifier: MPL-2.0-or-later

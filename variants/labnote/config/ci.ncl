# Nickel CI configuration schema
# Validates GitHub Actions workflow configuration

let CIWorkflow = {
  name
    | String
    | doc "Workflow name",

  on = {
    push = {
      branches
        | Array String
        | default = ["main"],
    },
    pull_request = {
      branches
        | Array String
        | default = ["main"],
    },
  },

  env = {
    DENO_VERSION
      | String
      | default = "v1.40.0",
  },

  jobs = {
    lint = {
      runs_on
        | String
        | default = "ubuntu-latest",
      steps
        | Array { name | String, uses | String | optional, run | String | optional }
        | doc "Job steps",
    },
    test = {
      runs_on
        | String
        | default = "ubuntu-latest",
      needs
        | Array String
        | default = ["lint"],
    },
    security = {
      runs_on
        | String
        | default = "ubuntu-latest",
    },
  },
}

let workflow : CIWorkflow = {
  name = "CI",
  jobs = {
    lint = {
      runs_on = "ubuntu-latest",
      steps = [
        { name = "Checkout", uses = "actions/checkout@v4" },
        { name = "Setup Deno", uses = "denoland/setup-deno@v2" },
        { name = "Lint", run = "deno lint adapters/" },
      ],
    },
    test = {
      runs_on = "ubuntu-latest",
      needs = ["lint"],
    },
    security = {
      runs_on = "ubuntu-latest",
    },
  },
}

in workflow

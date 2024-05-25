set shell := ["nu", "-c"]
default:
  stack run

watch:
  watch . --glob=**/*.hs {|| just default }

new_module name:
  open app/template.hs | str replace --all "TEMPLATE" {{name}} | save app/{{name}}.hs

set_module:
  source set_module.nu


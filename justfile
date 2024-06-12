set shell := ["nu", "-c"]
default:
  stack run

watch:
  watch . --glob=**/*.hs {|| just default }

new_module name:
  open app/template.hs | str replace --all "TEMPLATE" {{name}} | save app/{{name}}.hs
  open app/Main.hs | lines | insert 2 "import {{name}}" | collect {|| save -f app/Main.hs}
set_module:
  source set_module.nu

gc:
 ls | where name =~ ".scad$" or name =~ ".stl$" | get name | each {|| rm $in}

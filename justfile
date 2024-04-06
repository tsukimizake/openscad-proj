set shell := ["nu", "-c"]
default:
  stack run

watch:
  watch . --glob=**/*.hs {|| just default }


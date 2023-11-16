default:
  just ghcid

test: db
  cabal test --test-show-details=streaming --test-options='--print-slow-items --format=specdoc --color'

clean-golden:
  rm -r ./test/.golden

commit-golden:
  find -name '*.actual*' | parallel --plus mv -v {} {/actual/expected}

test-watch: db
  watchexec -w test/ -w lib/ -e hs just test

ghcid: db
  ghcid -a -c "cabal repl luql-test -fghci-load-test-with-lib" --test=':run Main.main --print-slow-items --format=specdoc --color' --warnings

db:
  docker-compose up -d

db-init: db
  #!/usr/bin/env bash
  set -euo pipefail
  set -x
  curl -L https://github.com/devrimgunduz/pagila/raw/master/pagila-schema.sql | psql -f - postgres://postgres:123456@localhost:5432/postgres
  curl -L https://github.com/devrimgunduz/pagila/raw/master/pagila-data.sql | psql -f - postgres://postgres:123456@localhost:5432/postgres

pgcli:
  pgcli postgres://postgres:123456@localhost:5432/postgres

stop:
  docker-compose down

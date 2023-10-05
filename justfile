default:
  just ghcid

test: db
  cabal test --test-show-details=streaming --test-options='--golden-start --golden-reset --retries=0'

clean-golden:
  rm -r ./test/.golden

test-watch: db
  watchexec -w test/ -w lib/ -e hs just test

ghcid: db
  ghcid -a -c "cabal repl luql-test -fghci-load-test-with-lib" --test=':run Main.main --golden-reset --golden-start --retries=0' --warnings

db:
  docker-compose up -d

pgcli:
  pgcli postgres://postgres:123456@localhost:5432/postgres

stop:
  docker-compose down
